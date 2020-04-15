use std::{
    collections::HashMap,
    env,
    io::Error as IoError,
    net::SocketAddr,
    sync::{Arc, Mutex},
};
use rand::Rng;

use futures::{select, future};
use futures::stream::{StreamExt};
use futures::sink::{SinkExt};
use futures::channel::mpsc::{unbounded, UnboundedSender, UnboundedReceiver};

use tokio::net::{TcpListener, TcpStream};
use tungstenite::protocol::Message;
use tokio_tungstenite::WebSocketStream;

use pont_common::{ClientMessage, ServerMessage};

struct PlayerJoined {
    name: String,
    addr: SocketAddr,
    ws: WebSocketStream<TcpStream>,
}

type RoomList = Arc<Mutex<HashMap<String, UnboundedSender<PlayerJoined>>>>;

async fn run_room(room_name: String,
                  mut in_rx: UnboundedReceiver<PlayerJoined>)
{
    let mut players = HashMap::new();

    // We'll funnel all Websocket communication through this MPSC connection,
    // so each websocket's incoming stream runs in its own little task
    let (ws_tx, ws_rx) = unbounded();

    println!("[{}] Started room!", room_name);

    let mut starting = true;
    while starting || players.len() > 0 {
        let p = in_rx.next().await
            .expect("Could not get player joining");
        println!("[{}] Player '{}' joined", room_name, p.name);

        let (incoming, outgoing) = p.ws.split();
        players.insert(p.addr, (p.name, incoming));
        starting = false;

        tokio::spawn(outgoing.map(|m|
            match m {
                Ok(Message::Text(t)) => Some(
                    serde_json::from_str::<ClientMessage>(&t)
                        .expect("Could not decode message")),
                _ => None,
            })
            .take_while(|m| future::ready(m.is_some()))
            .map(|m| m.unwrap())
            .chain(futures::stream::once(async { ClientMessage::Disconnected }))
            .map(|m| Ok(m))
            .forward(ws_tx.clone()));
    }

    println!("[{}] All players left, shutting down.", room_name);
}

async fn handle_connection(rooms: RoomList,
                           namer: impl Fn() -> String,
                           raw_stream: TcpStream,
                           addr: SocketAddr)
{
    println!("[{}] Incoming TCP connection", addr);

    let mut ws_stream = tokio_tungstenite::accept_async(raw_stream)
        .await
        .expect("Error during the websocket handshake occurred");
    println!("[{}] WebSocket connection established", addr);

    // Clients are only allowed to send text messages at this stage.
    // If they do anything else, then just disconnect.
    while let Some(Ok(Message::Text(t))) = ws_stream.next().await {
        let msg = serde_json::from_str::<ClientMessage>(&t)
            .expect("Could not decode message");
        match msg {
            ClientMessage::CreateRoom(name) => {
                let room_name = namer();
                let (mut tx, rx) = unbounded();
                {   // Nested scope to avoid issues with yield point
                    rooms.lock().unwrap().insert(room_name.clone(), tx.clone());
                }

                // This is the task which actually handles running
                // each room, now that we've created it.
                tokio::spawn(run_room(room_name.clone(), rx));

                // Tell the player that they've entered the room
                let msg = ServerMessage::CreatedRoom(room_name);
                let encoded = serde_json::to_string(&msg)
                    .expect("Could not encode message");
                ws_stream.send(Message::Text(encoded)).await
                    .expect("Could not send message");

                // Then pass the player into the room's task
                tx.send(PlayerJoined {addr, name, ws: ws_stream})
                    .await
                    .expect("Could not pass player to room");

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
                let mut tx = None;
                if let Some(tx_) = rooms.lock().unwrap().get(&room) {
                    tx = Some(tx_.clone());
                }

                if let Some(mut tx) = tx {
                    tx.send(PlayerJoined {addr, name, ws: ws_stream})
                        .await
                        .expect("Could not send player into room");
                    return;
                } else {
                    // Otherwise, here's the error handler
                    let msg = ServerMessage::UnknownRoom(room);
                    let encoded = serde_json::to_string(&msg)
                        .expect("Could not encode message");
                    ws_stream.send(Message::Text(encoded)).await
                        .expect("Could not send message");
                }
            }
            msg => {
                println!("[{}] Got unexpected message {:?}", addr, msg);
                break;
            }
        }
    }
    println!("[{}] Dropping connection", addr);
}

#[tokio::main]
async fn main() -> Result<(), IoError> {
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string());

    // words.txt is the EFF's random word list for passphrases
    let words = include_str!("words.txt")
        .split('\n')
        .collect::<Vec<&str>>();
    let namer = move || {
        let mut rng = rand::thread_rng();
        let i = rng.gen_range(0, words.len());
        let j = rng.gen_range(0, words.len());
        let k = rng.gen_range(0, words.len());
        return format!("{} {} {}", words[i], words[j], words[k]);
    };

    // Create the event loop and TCP listener we'll accept connections on.
    let mut listener = TcpListener::bind(&addr).await
        .expect("Failed to bind");
    println!("Listening on: {}", addr);

    // Each connection is initially handled by its own task;
    // once it joins a room, it will be handled by a room-specific task
    let rooms = RoomList::new(Mutex::new(HashMap::new()));
    while let Ok((stream, addr)) = listener.accept().await {
        tokio::spawn(handle_connection(rooms.clone(), namer.clone(),
                                       stream, addr));
    }

    Ok(())
}
