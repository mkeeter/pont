use std::{
    collections::HashMap,
    env,
    io::Error as IoError,
    net::SocketAddr,
    sync::{Arc, Mutex},
};
use rand::Rng;

use futures_util::StreamExt;
use futures_channel::mpsc::{unbounded, UnboundedSender, UnboundedReceiver};

use tokio::net::{TcpListener, TcpStream};
use tungstenite::protocol::Message;

use pont_common::{ClientMessage, ServerMessage};

type Rx = UnboundedReceiver<String>;
type Tx = UnboundedSender<String>;
type RoomList = Arc<Mutex<HashMap<String, Tx>>>;

async fn run_room(name: String, mut rx: Rx) {
    while let Some(m) = rx.next().await {
        println!("Got message {:?}", m);
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

    println!("sending message to stream");
    /*
    let m = pont_common::Command::CreateRoom;
    futures::stream::once(
            async { Ok(Message::Text(serde_json::to_string(&m).unwrap())) })
        .forward(ws_stream).await;
    println!("done");
    */

    //let (incoming, outgoing) = ws_stream.split();
    //let msg = incoming.read().await;
    if let Some(Ok(Message::Text(t))) = ws_stream.next().await {
        if let Ok(msg) = serde_json::from_str::<pont_common::ClientMessage>(&t) {
            println!("{:?}", msg);
            match msg {
                ClientMessage::CreateRoom(name) => {
                    let room_name = namer();
                    let (tx, rx) = unbounded();
                    rooms.lock().unwrap().insert(room_name.clone(), tx);

                    tokio::spawn(run_room(room_name, rx));
                },
                ClientMessage::JoinRoom(name, room) => {
                    if let Some(tx) = rooms.lock().unwrap().get(&room) {
                        tx.unbounded_send(name);
                    } else {
                        // Send "Unknown room" error back down websocket
                    }
                }
            }
        }
    }
    /*
    // Insert the write part of this peer to the peer map.
    let (tx, rx) = unbounded();
    peer_map.lock().unwrap().insert(addr, tx);

    let broadcast_incoming = incoming.try_for_each(|msg| {
        println!(
            "Received a message from {}: {}",
            addr,
            msg.to_text().unwrap()
        );
        let peers = peer_map.lock().unwrap();

        // We want to broadcast the message to everyone except ourselves.
        let broadcast_recipients = peers
            .iter()
            .filter(|(peer_addr, _)| peer_addr != &&addr)
            .map(|(_, ws_sink)| ws_sink);

        for recp in broadcast_recipients {
            recp.unbounded_send(msg.clone()).unwrap();
        }
    });

    let receive_from_others = rx.map(Ok).forward(outgoing);

    pin_mut!(broadcast_incoming, receive_from_others);
    future::select(broadcast_incoming, receive_from_others).await;

    println!("{} disconnected", &addr);
    peer_map.lock().unwrap().remove(&addr);
        */
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

    let rooms = RoomList::new(Mutex::new(HashMap::new()));

    // Create the event loop and TCP listener we'll accept connections on.
    let try_socket = TcpListener::bind(&addr).await;
    let mut listener = try_socket.expect("Failed to bind");
    println!("Listening on: {}", addr);

    // Let's spawn the handling of each connection in a separate task.
    while let Ok((stream, addr)) = listener.accept().await {
        tokio::spawn(handle_connection(rooms.clone(), namer.clone(), stream, addr));
    }

    Ok(())
}
