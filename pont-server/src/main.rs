use std::{
    collections::HashMap,
    env,
    io::Error as IoError,
    net::SocketAddr,
    sync::{Arc, Mutex},
};
use rand::Rng;

use futures_util::{SinkExt, StreamExt};
use futures_channel::mpsc::{unbounded, UnboundedSender, UnboundedReceiver};

use tokio::net::{TcpListener, TcpStream};
use tungstenite::protocol::Message;

use pont_common::{ClientMessage, ServerMessage};

type Rx = UnboundedReceiver<String>;
type Tx = UnboundedSender<String>;
type RoomList = Arc<Mutex<HashMap<String, Tx>>>;

async fn run_room(room_name: String, mut rx: Rx) {
    // Wait on:
    //  - Incoming player list
    //  - All player websockets
    println!("[{}] Started room", room_name);
    while let Some(m) = rx.next().await {
        println!("[{}]: Got message {:?}", room_name, m);
    }
}

async fn handle_connection(rooms: RoomList,
                           namer: impl Fn() -> String,
                           raw_stream: TcpStream,
                           addr: SocketAddr)
{
    println!("Incoming TCP connection from: {}", addr);

    let mut ws_stream = tokio_tungstenite::accept_async(raw_stream)
        .await
        .expect("Error during the websocket handshake occurred");
    println!("WebSocket connection established: {}", addr);

    // Clients are only allowed to send text messages at this stage.
    // If they do anything else, then just disconnect.
    while let Some(Ok(Message::Text(t))) = ws_stream.next().await {
        let msg = serde_json::from_str::<ClientMessage>(&t)
            .expect("Could not decode message");
        match msg {
            ClientMessage::CreateRoom(name) => {
                let room_name = namer();
                let (tx, rx) = unbounded();
                tx.unbounded_send(name)
                    .expect("Could not send name");
                rooms.lock().unwrap().insert(room_name.clone(), tx);

                // This is the task which actually handles running
                // each room, now that we've created it.
                tokio::spawn(run_room(room_name.clone(), rx));

                let msg = ServerMessage::CreatedRoom(room_name);
                let encoded = serde_json::to_string(&msg)
                    .expect("Could not encode message");
                ws_stream.send(Message::Text(encoded)).await
                    .expect("Could not send message");

                // We've passed everything to the spawned room task,
                // so we return right away (rather than handling more
                // messages from the websocket)
                break;
            },
            ClientMessage::JoinRoom(name, room) => {
                // If the room name is valid, then join it by passing
                // the new user and their connection into the room task
                if let Some(tx) = rooms.lock().unwrap().get(&room) {
                    tx.unbounded_send(name)
                        .expect("Could not send name");
                    break;
                }
                // Otherwise, here's the error handler
                let msg = ServerMessage::UnknownRoom(room);
                let encoded = serde_json::to_string(&msg)
                    .expect("Could not encode message");
                ws_stream.send(Message::Text(encoded)).await
                    .expect("Could not send message");
            }
        }
    }
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
