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

use pont_common::{ClientMessage, ServerMessage, Game, Piece};

////////////////////////////////////////////////////////////////////////////////

lazy_static::lazy_static! {
    // words.txt is the EFF's random word list for passphrases
    static ref WORD_LIST: Vec<&'static str> = include_str!("words.txt")
        .split('\n')
        .filter(|w| !w.is_empty())
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

struct Player {
    name: String,
    score: u32,
    hand: HashMap<Piece, usize>,
    ws: Option<UnboundedSender<ServerMessage>>
}

struct Room {
    name: String,
    started: bool,
    connections: HashMap<SocketAddr, usize>,
    players: Vec<Player>,
    active_player: usize,
    game: Game,
}

impl Room {
    fn running(&self) -> bool {
        !self.started || !self.connections.is_empty()
    }

    fn broadcast(&self, s: ServerMessage) {
        for c in self.connections.values() {
            self.players[*c].ws.as_ref().unwrap().unbounded_send(s.clone())
                .expect("Failed to send broadcast");
        }
    }

    fn broadcast_except(&self, i: usize, s: ServerMessage) {
        for (j, p) in self.players.iter().enumerate() {
            if i != j {
                if let Some(ws) = p.ws.as_ref() {
                    ws.unbounded_send(s.clone())
                        .expect("Failed to send broadcast");
                }
            }
        }
    }

    fn send(&self, i: usize, s: ServerMessage) {
        if let Some(p) = self.players[i].ws.as_ref() {
            p.unbounded_send(s).expect("Failed to send player message");
        } else {
            error!("[{}] Tried sending message to inactive player", self.name);
        }
    }

    fn add_player(&mut self, addr: SocketAddr, player_name: String,
                  ws: SplitSink<WebSocketStream<TcpStream>, WebsocketMessage>)
    {
        // Add the new player to the scoreboard
        self.broadcast(ServerMessage::NewPlayer(player_name.clone()));

        // Store an UnboundedSender so we can write to websockets
        // without an async call, with messages being passed to
        // the actual socket by another worker task.
        let (ws_tx, ws_rx) = unbounded();
        tokio::spawn(ws_rx
            .map(|c| bincode::serialize(&c)
                .expect(&format!("Could not encode {:?}", c)))
            .map(WebsocketMessage::Binary)
            .map(Ok)
            .forward(ws));

        // Add the new player to the active list of connections and players
        self.connections.insert(addr, self.players.len());
        let hand = self.game.deal(6);
        let mut pieces = Vec::new();
        for (piece, count) in hand.iter() {
            for _i in 0..*count {
                pieces.push(piece.clone());
            }
        }
        self.players.push(Player {
            name: player_name,
            score: 0,
            hand,
            ws: Some(ws_tx.clone()) });

        // Tell the player that they have joined the room
        ws_tx.unbounded_send(ServerMessage::JoinedRoom{
                room_name: self.name.clone(),
                players: self.players.iter()
                    .map(|p| (p.name.clone(), p.score, p.ws.is_some()))
                    .collect(),
                active_player: self.active_player,
                board: self.game.board.iter()
                    .map(|(k, v)| (*k, *v))
                    .collect(),
                pieces})
            .expect("Could not send JoinedRoom");

        self.started = true;
    }

    fn next_player(&mut self) {
        if !self.connections.is_empty() {
            self.active_player = (self.active_player + 1) %
                                  self.players.len();
            while self.players[self.active_player].ws.is_none() {
                self.active_player = (self.active_player + 1) %
                                      self.players.len();
            }
            info!("[{}] Active player changed to {}", self.name,
                  self.players[self.active_player].name);

            self.broadcast(ServerMessage::PlayerTurn(self.active_player));
        }
    }

    fn on_client_disconnected(&mut self, addr: SocketAddr) {
        if let Some(p) = self.connections.remove(&addr) {
            let player_name = self.players[p].name.clone();
            info!("[{}] Removed disconnected player '{}'",
                  self.name, player_name);
            self.players[p].ws = None;
            self.broadcast(ServerMessage::PlayerDisconnected(p));

            // Find the next active player and broadcast out that info
            self.next_player();
        } else {
            error!("[{}] Tried to remove non-existent player at {}",
                     self.name, addr);
        }
    }

    fn subtract_from_hand(&mut self, piece: Piece) -> bool {
        let p = &mut self.players[self.active_player];
        if let Some(count) = p.hand.get_mut(&piece) {
            if *count > 0 {
                *count -= 1;
                return true;
            }
        }
        false
    }

    fn on_play(&mut self, pieces: &[(Piece, i32, i32)]) {
        let mut board = self.game.board.clone();
        for (piece, x, y) in pieces.iter() {
            board.insert((*x, *y), *piece);
        }
        if !Game::invalid(&board).is_empty() ||
           !Game::is_linear(&pieces.iter().map(|p| (p.1, p.2)).collect())
        {
            warn!("[{}] Player {} tried to make an illegal move",
                  self.name, self.players[self.active_player].name);
            self.send(self.active_player, ServerMessage::MoveRejected);
            return;
        }

        for p in pieces {
            if !self.subtract_from_hand(p.0) {
                warn!("[{}] Player {} tried to play an unowned piece {:?}",
                      self.name, self.players[self.active_player].name, p);
                self.send(self.active_player, ServerMessage::MoveRejected);
                return;
            }
        }

        if let Some(score) = self.game.play(pieces) {
            // Broadcast the new score to all players
            self.players[self.active_player].score += score;
            self.broadcast(ServerMessage::PlayerScore {
                index: self.active_player,
                delta: score,
                total: self.players[self.active_player].score,
            });

            let mut deal = Vec::new();

            let hand_size: usize = self.players[self.active_player].hand
                .values().sum();
            for (piece, count) in self.game.deal(6 - hand_size) {
                *self.players[self.active_player].hand.entry(piece)
                    .or_insert(0) += count;
                for _i in 0..count {
                    deal.push(piece);
                }
            }
            info!("{:?}", self.players[self.active_player].hand);
            self.send(self.active_player, ServerMessage::MoveAccepted(deal));

            // Broadcast the play to other players
            self.broadcast_except(self.active_player,
                ServerMessage::Played(pieces.to_vec()));
        } else {
            warn!("[{}] Player {} snuck an illegal move past the first filters",
                  self.name, self.players[self.active_player].name);
            self.send(self.active_player, ServerMessage::MoveRejected);
        }
    }

    fn on_message(&mut self, addr: SocketAddr, msg: ClientMessage) {
        trace!("[{}] Got message {:?} from {}", self.name, msg, addr);
        match msg {
            ClientMessage::Disconnected => self.on_client_disconnected(addr),
            ClientMessage::Chat(c) => {
                let name = self.connections.get(&addr)
                    .map_or("unknown", |i| &self.players[*i].name);
                self.broadcast(ServerMessage::Chat{
                            from: name.to_string(),
                            message: c});
            },
            ClientMessage::CreateRoom(_) | ClientMessage::JoinRoom(_, _) => {
                warn!("[{}] Invalid client message {:?}", self.name, msg);
            },
            ClientMessage::Play(pieces) => {
                if let Some(i) = self.connections.get(&addr).copied() {
                    if i == self.active_player {
                        self.on_play(&pieces);
                        self.next_player();
                    } else {
                        warn!("[{}] Player {} out of turn", self.name, addr);
                    }
                } else {
                    warn!("[{}] Invalid player {}", self.name, addr);
                }
            }
            /*
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
        connections: HashMap::new(),
        players: Vec::new(),
        active_player: 0,
        game: Game::new(),
    };

    info!("[{}] Started room!", room.name);

    enum Either {
        Left(PlayerJoined),
        Right((SocketAddr, ClientMessage)),
    }
    use Either::*;

    let mut inputs = futures::stream::select(
        in_rx.map(Left),
        ws_rx.map(Right));

    while room.running() {
        if let Some(m) = inputs.next().await {
            match m {
                Left(p) => {
                    info!("[{}] Player '{}' joined", room.name, p.name);

                    let (incoming, outgoing) = p.ws.split();
                    room.add_player(p.addr, p.name, incoming);

                    // Messages from every websocket are asynchronously
                    // forwarded to a single MPSC queue
                    let addr = p.addr;
                    tokio::spawn(outgoing.map(|m|
                        match m {
                            Ok(WebsocketMessage::Binary(t)) => Some(
                                bincode::deserialize::<ClientMessage>(&t)
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
    while let Some(Ok(WebsocketMessage::Binary(t))) = ws_stream.next().await {
        let msg = bincode::deserialize::<ClientMessage>(&t)
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
                let encoded = bincode::serialize(&msg)
                    .expect("Could not encode message");
                ws_stream.send(WebsocketMessage::Binary(encoded)).await
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
