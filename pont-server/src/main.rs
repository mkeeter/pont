use std::{
    collections::HashMap,
    env,
    io::Error as IoError,
    net::SocketAddr,
    sync::{Arc, Mutex},
};
use rand::Rng;
use log::{error, info, trace, warn};
use env_logger::Env;

use futures::future;
use futures::stream::{StreamExt, SplitSink};
use futures::sink::SinkExt;
use futures::channel::mpsc::{unbounded, UnboundedSender, UnboundedReceiver};

use tokio::net::{TcpListener, TcpStream};
use tungstenite::protocol::Message as WebsocketMessage;
use tokio_tungstenite::WebSocketStream;

use pont_common::{ClientMessage, ServerMessage};

////////////////////////////////////////////////////////////////////////////////

lazy_static::lazy_static! {
    // words.txt is the EFF's random word list for passphrases
    static ref WORD_LIST: Vec<&'static str> = include_str!("words.txt")
        .split('\n')
        .collect();
}

// This message is passed into a Room task when a new player joins.
// The room task then owns the relationship with that player.
struct PlayerJoined {
    name: String,
    addr: SocketAddr,
    ws: WebSocketStream<TcpStream>,
}

type RoomList = Arc<Mutex<HashMap<String, UnboundedSender<PlayerJoined>>>>;

struct ActivePlayer {
    name: String,
    ws: UnboundedSender<ServerMessage>,
}

struct Room {
    name: String,
    started: bool,
    players: HashMap<SocketAddr, ActivePlayer>,
}

impl Room {
    fn running(&self) -> bool {
        !self.started || self.players.len() > 0
    }

    fn add_player(&mut self, addr: SocketAddr, name: String,
        ws: SplitSink<WebSocketStream<TcpStream>, WebsocketMessage>)
    {
        // Store an UnboundedSender so we can write to websockets
        // without an async call, with messages being passed to
        // the actual socket by another worker task.
        let (ws_tx, ws_rx) = unbounded();
        tokio::spawn(ws_rx
            .map(|c| serde_json::to_string(&c)
                .expect("Could not encode"))
            .map(|t| WebsocketMessage::Text(t))
            .map(|m| Ok(m))
            .forward(ws));

        let player = ActivePlayer { name, ws: ws_tx };
        player.ws.unbounded_send(ServerMessage::JoinedRoom{
                name: player.name.clone(),
                room: self.name.clone() })
            .expect("Could not send JoinedRoom");
        player.ws.unbounded_send(ServerMessage::Information(
                format!("Welcome, {}!", player.name.clone())))
            .expect("Could not send JoinedRoom");

        for p in self.players.values() {
            p.ws.unbounded_send(ServerMessage::Information(
                    format!("{} joined the room", player.name)))
                .expect("Failed to write chat message");
        }

        self.players.insert(addr, player);
        self.started = true;
    }

    fn on_message(&mut self, addr: SocketAddr, msg: ClientMessage) {
        trace!("[{}] Got message {:?} from {}", self.name, msg, addr);
        match msg {
            ClientMessage::Disconnected => {
                if let Some(p) = self.players.remove(&addr) {
                    info!("[{}] Removed disconnected player '{}'",
                             self.name, p.name);
                    for p in self.players.values() {
                        p.ws.unbounded_send(ServerMessage::Information(
                                format!("{} disconnected", p.name)))
                            .expect("Failed to write chat message");
                    }
                } else {
                    error!("[{}] Tried to remove non-existent player at {}",
                             self.name, addr);
                }
            },
            ClientMessage::Chat(c) => {
                let name = self.players.get(&addr)
                    .map_or("unknown", |p| &p.name);
                for p in self.players.values() {
                    p.ws.unbounded_send(ServerMessage::Chat{
                            from: name.to_string(),
                            message: c.clone()})
                        .expect("Failed to write chat message");
                }
            },
            ClientMessage::CreateRoom(_) | ClientMessage::JoinRoom(_, _) => {
                warn!("Invalid client message {:?}", msg);
            },
            /*
            ClientMessage::Play(pieces) => {
                if let Some(p) = self.players.get(&addr) {
                    trace!("[{}] {} played {:?}", self.name, p.name, pieces);
                } else {
                    trace!("[{}] Invalid player {}", self.name, addr);
                }
            }
            ClientMessage::Swap(pieces) => {
                if let Some(p) = self.players.get(&addr) {
                    trace!("[{}] {} swapped {:?}", self.name, p.name, pieces);
                } else {
                    trace!("[{}] Invalid player {}", self.name, addr);
                }
            }
            */
        }
    }
}

async fn run_room(room_name: String,
                  in_rx: UnboundedReceiver<PlayerJoined>,
                  mut done: UnboundedSender<String>)
{
    // We'll funnel all Websocket communication through this MPSC connection,
    // so each websocket's incoming stream runs in its own little task
    let (ws_tx, ws_rx) = unbounded();

    let mut room = Room {
        name: room_name,
        started: false,
        players: HashMap::new(),
    };

    info!("[{}] Started room!", room.name);

    enum Either {
        Left(PlayerJoined),
        Right((SocketAddr, ClientMessage)),
    }
    use Either::*;

    let mut inputs = futures::stream::select(
        in_rx.map(|p| Left(p)),
        ws_rx.map(|m| Right(m)));

    while room.running() {
        if let Some(m) = inputs.next().await {
            match m {
                Left(p) => {
                    info!("[{}] Player '{}' joined", room.name, p.name);

                    let (incoming, outgoing) = p.ws.split();
                    room.add_player(p.addr, p.name, incoming);

                    // Messages from every websocket are asynchronously
                    // forwarded to a single MPSC queue
                    let addr = p.addr.clone();
                    tokio::spawn(outgoing.map(|m|
                        match m {
                            Ok(WebsocketMessage::Text(t)) => Some(
                                serde_json::from_str::<ClientMessage>(&t)
                                    .expect("Could not decode message")),
                            _ => None,
                        })
                        .take_while(|m| future::ready(m.is_some()))
                        .map(|m| m.unwrap())
                        .chain(futures::stream::once(async {
                            ClientMessage::Disconnected }))
                        .map(move |m| Ok((addr, m)))
                        .forward(ws_tx.clone()));
                }
                Right((addr, msg)) => {
                    room.on_message(addr, msg);
                }
            }
        }
    }

    info!("[{}] All players left, closing room.", room.name);
    done.send(room.name).await
        .expect("Could not close room");
}

fn new_room(rooms: RoomList, tx: UnboundedSender<PlayerJoined>) -> String {
    let mut rng = rand::thread_rng();
    loop {
        let room_name = format!("{} {} {}",
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())],
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())],
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())]);

        use std::collections::hash_map::Entry;
        match rooms.lock().unwrap().entry(room_name.clone()) {
            Entry::Occupied(_) => continue,
            Entry::Vacant(v) => {
                v.insert(tx);
                return room_name;
            },
        }
    }
}

async fn handle_connection(rooms: RoomList,
                           raw_stream: TcpStream,
                           addr: SocketAddr,
                           close_room: UnboundedSender<String>)
{
    info!("[{}] Incoming TCP connection", addr);

    let mut ws_stream = tokio_tungstenite::accept_async(raw_stream)
        .await
        .expect("Error during the websocket handshake occurred");
    info!("[{}] WebSocket connection established", addr);

    // Clients are only allowed to send text messages at this stage.
    // If they do anything else, then just disconnect.
    while let Some(Ok(WebsocketMessage::Text(t))) = ws_stream.next().await {
        let msg = serde_json::from_str::<ClientMessage>(&t)
            .expect("Could not decode message");
        match msg {
            ClientMessage::CreateRoom(name) => {
                let (tx, rx) = unbounded();

                // Pass the player into the room's task, which will inform
                // them that they've joined the room
                tx.unbounded_send(PlayerJoined {addr, name, ws: ws_stream})
                    .expect("Could not pass player to room");

                // Pick a new room name and attach the queue to it
                let room_name = new_room(rooms, tx);

                // This is the task which actually handles running
                // each room, now that we've created it.
                tokio::spawn(run_room(room_name, rx, close_room));

                // We've passed everything to the spawned room task,
                // so we return right away (rather than handling more
                // messages from the websocket)
                return;
            },
            ClientMessage::JoinRoom(name, room) => {
                // If the room name is valid, then join it by passing
                // the new user and their connection into the room task
                //
                // We do a little bit of dancing here to avoid the borrow
                // checker, since the tx in tx.send(...).await must live
                // through yield points.
                if let Some(tx) = rooms.lock().unwrap().get(&room) {
                    tx.unbounded_send(PlayerJoined {addr, name, ws: ws_stream})
                        .expect("Could not send player into room");
                    return;
                }

                // Otherwise, here's the error handler
                let msg = ServerMessage::UnknownRoom(room);
                let encoded = serde_json::to_string(&msg)
                    .expect("Could not encode message");
                ws_stream.send(WebsocketMessage::Text(encoded)).await
                    .expect("Could not send message");
            }
            msg => {
                warn!("[{}] Got unexpected message {:?}", addr, msg);
                break;
            }
        }
    }
    info!("[{}] Dropping connection", addr);
}

async fn close_rooms(rooms: RoomList, mut rx: UnboundedReceiver<String>) {
    while let Some(r) = rx.next().await {
        info!("Closing room [{}]", r);
        rooms.lock().unwrap().remove(&r);
    }
}


#[tokio::main]
async fn main() -> Result<(), IoError> {
    env_logger::from_env(Env::default().default_filter_or("pont_server=TRACE"))
        .init();
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "0.0.0.0:8080".to_string());

    // Create the event loop and TCP listener we'll accept connections on.
    let mut listener = TcpListener::bind(&addr).await
        .expect("Failed to bind");
    info!("Listening on: {}", addr);

    // Each connection is initially handled by its own task;
    // once it joins a room, it will be handled by a room-specific task
    let rooms = RoomList::new(Mutex::new(HashMap::new()));
    let (tx, rx) = unbounded();
    tokio::spawn(close_rooms(rooms.clone(), rx));

    while let Ok((stream, addr)) = listener.accept().await {
        tokio::spawn(handle_connection(rooms.clone(), stream,
                                       addr, tx.clone()));
    }

    Ok(())
}
