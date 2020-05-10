use std::{
    collections::HashMap,
    env,
    io::Error as IoError,
    net::{TcpStream, TcpListener, SocketAddr},
    sync::{Arc, Mutex},
    time::Duration,
};
use rand::Rng;
use log::{error, info, trace, warn};
use env_logger::Env;

use futures::future;
use futures::stream::{StreamExt, SplitSink};
use futures::sink::SinkExt;
use futures::channel::mpsc::{unbounded, UnboundedSender, UnboundedReceiver};

use tungstenite::Message as WebsocketMessage;
use async_tungstenite::WebSocketStream;
use smol::{Async, Task, Timer};

use pont_common::{ClientMessage, ServerMessage, Game, Piece};

////////////////////////////////////////////////////////////////////////////////

lazy_static::lazy_static! {
    // words.txt is the EFF's random word list for passphrases
    static ref WORD_LIST: Vec<&'static str> = include_str!("words.txt")
        .split('\n')
        .filter(|w| !w.is_empty())
        .collect();
}

// Normally, a room exists as a relatively standalone task:
// Client websockets send their messages to `write`, and `run_room` reads
// them from `read` and applies them to the `room` object.
//
// It's made more complicated by the fact that adding players needs to
// access the room object *before* clients are plugged into the `read`/`write`
// infrastructure, so it must be shared and accessible from `handle_connection`
type TaggedClientMessage = (SocketAddr, ClientMessage);
#[derive(Clone)]
struct RoomHandle {
    write: UnboundedSender<TaggedClientMessage>,
    room: Arc<Mutex<Room>>,
}

impl RoomHandle {
    fn add_player(&mut self, name: String, addr: SocketAddr,
                  ws_stream: WebSocketStream<Async<TcpStream>>)
    {
        // Messages from every websocket are asynchronously
        // forwarded to the room's MPSC queue
        let (incoming, outgoing) = ws_stream.split();
        Task::spawn(outgoing.map(|m|
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
                .forward(self.write.clone()))
            .expect("Could not create task")
            .detach();

        let room = &mut self.room.lock().unwrap();
        room.add_player(addr, name, incoming);
    }
}

type RoomList = Arc<Mutex<HashMap<String, RoomHandle>>>;

struct Room {
    name: String,
    started: bool,
    ended: bool,
    connections: HashMap<SocketAddr, usize>,
    players: Vec<Player>,
    active_player: usize,
    game: Game,
}

struct Player {
    name: String,
    score: u32,
    hand: HashMap<Piece, usize>,
    ws: Option<UnboundedSender<ServerMessage>>
}

impl Player {
    // Tries to remove a set of pieces from the player's hand
    // On failure, returns false.
    fn try_remove(&mut self, pieces: &[Piece]) -> bool {
        let mut count = HashMap::new();
        for piece in pieces {
            *count.entry(piece).or_insert(0) += 1;
        }

        for (piece, n) in count.iter() {
            if let Some(m) = self.hand.get(&piece) {
                if *m < *n {
                    return false;
                }
            } else {
                return false;
            }
        }

        for (piece, n) in count.iter() {
            if let Some(m) = self.hand.get_mut(&piece) {
                *m -= n;
            }
        }
        true
    }

    fn hand_is_empty(&self) -> bool {
        self.hand.values().all(|i| *i == 0)
    }
}

impl Room {
    fn running(&self) -> bool {
        !self.connections.is_empty()
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
                  ws: SplitSink<WebSocketStream<Async<TcpStream>>, WebsocketMessage>)
    {
        // Add the new player to the scoreboard
        self.broadcast(ServerMessage::NewPlayer(player_name.clone()));

        // Store an UnboundedSender so we can write to websockets
        // without an async call, with messages being passed to
        // the actual socket by another worker task.
        let (ws_tx, ws_rx) = unbounded();
        Task::spawn(ws_rx
                .map(|c| bincode::serialize(&c)
                    .expect(&format!("Could not encode {:?}", c)))
                .map(WebsocketMessage::Binary)
                .map(Ok)
                .forward(ws))
            .expect("Could not create encoding task")
            .detach();

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
                pieces,
                remaining: self.game.bag.len()})
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

            self.broadcast(ServerMessage::PlayerTurn(self.active_player,
                                                     self.game.bag.len()));
        }
    }

    fn on_client_disconnected(&mut self, addr: SocketAddr) {
        if let Some(p) = self.connections.remove(&addr) {
            let player_name = self.players[p].name.clone();
            info!("[{}] Removed disconnected player '{}'",
                  self.name, player_name);
            self.players[p].ws = None;
            for (k, v) in self.players[p].hand.drain() {
                for _i in 0..v {
                    self.game.bag.push(k.clone());
                }
            }
            self.game.shuffle();
            self.broadcast(ServerMessage::PlayerDisconnected(p));

            // Find the next active player and broadcast out that info
            if p == self.active_player {
                self.next_player();
            }
        } else {
            error!("[{}] Tried to remove non-existent player at {}",
                     self.name, addr);
        }
    }

    fn on_play(&mut self, pieces: &[(Piece, i32, i32)]) {
        let player = &mut self.players[self.active_player];

        let mut board = self.game.board.clone();
        for (piece, x, y) in pieces.iter() {
            board.insert((*x, *y), *piece);
        }
        if !Game::invalid(&board).is_empty() ||
           !Game::is_linear(&pieces.iter().map(|p| (p.1, p.2)).collect())
        {
            warn!("[{}] Player {} tried to make an illegal move",
                  self.name, player.name);
            self.send(self.active_player, ServerMessage::MoveRejected);
            return;
        }

        {   // Remove the pieces from the player's hand
            let pieces: Vec<Piece> = pieces.iter().map(|p| p.0).collect();
            if !player.try_remove(&pieces) {
                warn!("[{}] Player {} tried to play an unowned piece",
                      self.name, player.name);
                self.send(self.active_player, ServerMessage::MoveRejected);
                return;
            }
        }

        if let Some(mut delta) = self.game.play(pieces) {
            // Broadcast the new score to all players
            let mut deal = Vec::new();
            for (piece, count) in self.game.deal(pieces.len()) {
                *player.hand.entry(piece)
                    .or_insert(0) += count;
                for _i in 0..count {
                    deal.push(piece);
                }
            }
            // Check whether the game is over!
            let over = player.hand_is_empty() && self.game.bag.is_empty();
            if over {
                delta += 6;
            }
            player.score += delta;

            let total = player.score; // Release the borrow of player
            self.broadcast(ServerMessage::PlayerScore { delta, total, });
            self.send(self.active_player, ServerMessage::MoveAccepted(deal));

            // Broadcast the play to other players
            self.broadcast_except(self.active_player,
                ServerMessage::Played(pieces.to_vec()));

            if over {
                let winner = self.players.iter()
                    .enumerate()
                    .max_by_key(|(_i, p)| p.score).unwrap().0;
                self.broadcast(ServerMessage::ItsOver(winner));
                self.ended = true;
            }
        } else {
            warn!("[{}] Player {} snuck an illegal move past the first filters",
                  self.name, player.name);
            self.send(self.active_player, ServerMessage::MoveRejected);
        }
    }

    fn on_swap(&mut self, pieces: &[Piece]) {
        let player = &mut self.players[self.active_player];
        if !player.try_remove(pieces) {
            warn!("[{}] Player {} tried to play an unowned piece",
                  self.name, player.name);
            self.send(self.active_player, ServerMessage::MoveRejected);
            return;
        } else if let Some(deal) = self.game.swap(pieces) {
            for piece in deal.iter() {
                *player.hand.entry(*piece).or_insert(0) += 1;
            }
            self.send(self.active_player, ServerMessage::MoveAccepted(deal));

            // Broadcast the swap to other players
            self.broadcast(ServerMessage::Swapped(pieces.len()));
        } else {
            warn!("[{}] Player {} couldn't be dealt {} pieces",
                  self.name, player.name,
                  pieces.len());
        }
    }

    fn on_message(&mut self, addr: SocketAddr, msg: ClientMessage) -> bool {
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
                if self.ended {
                    warn!("[{}] Got play after move ended", self.name);
                } else if let Some(i) = self.connections.get(&addr).copied() {
                    if i == self.active_player {
                        self.on_play(&pieces);
                        if !self.ended {
                            self.next_player();
                        }
                    } else {
                        warn!("[{}] Player {} out of turn", self.name, addr);
                    }
                } else {
                    warn!("[{}] Invalid player {}", self.name, addr);
                }
            },
            ClientMessage::Swap(pieces) => {
                if self.ended {
                    warn!("[{}] Got play after move ended", self.name);
                } else if let Some(i) = self.connections.get(&addr).copied() {
                    if i == self.active_player {
                        self.on_swap(&pieces);
                        self.next_player();
                    } else {
                        warn!("[{}] Player {} out of turn", self.name, addr);
                    }
                } else {
                    warn!("[{}] Invalid player {}", self.name, addr);
                }
            },
        }
        self.running()
    }
}

async fn run_room(handle: RoomHandle,
                  mut read: UnboundedReceiver<TaggedClientMessage>)
{
    while let Some((addr, msg)) = read.next().await {
        if !handle.room.lock().unwrap().on_message(addr, msg) {
            break;
        }
    }
}

fn new_room(rooms: &mut HashMap<String, RoomHandle>) ->
    (UnboundedReceiver<TaggedClientMessage>, String, RoomHandle)
{
    let mut rng = rand::thread_rng();
    // This loop should only run once, unless we're starting to saturate the
    // space of possible room names (which is quite large)
    loop {
        let room_name = format!("{} {} {}",
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())],
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())],
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())]);

        use std::collections::hash_map::Entry;
        match rooms.entry(room_name.clone()) {
            Entry::Occupied(_) => continue,
            Entry::Vacant(v) => {
                // We'll funnel all Websocket communication through one
                // MPSC queue per room, with websockets running in their
                // own little tasks writing to the queue.
                let (write, read) = unbounded();

                let room = Arc::new(Mutex::new(Room {
                    name: room_name.clone(),
                    started: false,
                    ended: false,
                    connections: HashMap::new(),
                    players: Vec::new(),
                    active_player: 0,
                    game: Game::new(),
                }));
                let handle = RoomHandle { write, room };
                v.insert(handle.clone());
                return (read, room_name, handle);
            },
        }
    }
}

async fn handle_connection(rooms: RoomList,
                           raw_stream: Async<TcpStream>,
                           addr: SocketAddr,
                           mut close_room: UnboundedSender<String>)
{
    info!("[{}] Incoming TCP connection", addr);

    let mut ws_stream = async_tungstenite::accept_async(raw_stream)
        .await
        .expect("Error during the websocket handshake occurred");
    info!("[{}] WebSocket connection established", addr);

    // Clients are only allowed to send text messages at this stage.
    // If they do anything else, then just disconnect.
    while let Some(Ok(WebsocketMessage::Binary(t))) = ws_stream.next().await {
        let msg = bincode::deserialize::<ClientMessage>(&t)
            .expect("Could not decode message");

        // Try to interpret their message as joining a room
        match msg {
            ClientMessage::CreateRoom(name) => {
                // Pick a new room name and insert it into the global hashmap
                let map = &mut rooms.lock().unwrap();
                let (pipe, room_name, mut handle) = new_room(map);
                handle.add_player(name, addr, ws_stream);

                // This is the task which actually handles running
                // each room, now that we've created it.
                Task::spawn(async move {
                    run_room(handle, pipe).await;
                    info!("[{}] All players left, closing room.", room_name);
                    close_room.send(room_name).await
                        .expect("Could not close room");
                }).detach();

                // We've passed everything to the spawned room task,
                // so we return right away (rather than handling more
                // messages from the websocket)
                return;
            },
            ClientMessage::JoinRoom(name, room_name) => {
                // If the room name is valid, then join it by passing
                // the new user and their connection into the room task
                //
                // We do a little bit of dancing here to avoid the borrow
                // checker, since the tx in tx.send(...).await must live
                // through yield points.
                let handle = rooms.lock().unwrap().get_mut(&room_name).cloned();

                // If we tried to join an existing room, then check that there
                // are enough pieces in the bag to deal a full hand.
                if let Some(mut h) = handle {
                    if h.room.lock().unwrap().game.bag.len() >= 6 {
                        // Happy case: add the player to the room, then return
                        // (because the connection will be handled by the room's
                        // task from here on out).
                        h.add_player(name, addr, ws_stream);
                        return;
                    } else {
                        // Not enough pieces, so report an error to the client
                        let msg = ServerMessage::JoinFailed(
                            "Not enough pieces left".to_string());
                        let encoded = bincode::serialize(&msg)
                            .expect("Could not encode message");
                        ws_stream.send(WebsocketMessage::Binary(encoded)).await
                            .expect("Could not send message");
                    }
                } else {
                    // Otherwise, reply that we don't know anything about that
                    // particular room name.
                    let msg = ServerMessage::JoinFailed(
                        format!("Could not find room '{}'", room_name));
                    let encoded = bincode::serialize(&msg)
                        .expect("Could not encode message");
                    ws_stream.send(WebsocketMessage::Binary(encoded)).await
                        .expect("Could not send message");
                }
            }
            // If they send an illegal message, then they obviously have ill
            // intentions and we should disconnect them right now.
            msg => {
                warn!("[{}] Got unexpected message {:?}", addr, msg);
                break;
            }
        }
    }
    info!("[{}] Dropping connection", addr);
}

fn main() -> Result<(), IoError> {
    env_logger::from_env(Env::default().default_filter_or("pont_server=TRACE"))
        .init();

    // Create an executor thread pool.
    for _ in 0..num_cpus::get().max(1) {
        std::thread::spawn(|| smol::run(future::pending::<()>()));
    }

    let rooms = RoomList::new(Mutex::new(HashMap::new()));

    // Run a small task whose job is to close rooms when the last player leaves.
    // This task accepts room names through a MPSC queue, which all of the
    // room tasks push their names into.
    let close_room = {
        let (tx, mut rx) = unbounded();
        let rooms = rooms.clone();
        Task::spawn(async move {
            while let Some(r) = rx.next().await {
                info!("Closing room [{}]", r);
                rooms.lock().unwrap().remove(&r);
            }
        }).detach();
        tx
    };

    {   // Periodically print the number of open rooms to the logs
        let rooms = rooms.clone();
        Task::spawn(async move {
            loop {
                Timer::after(Duration::from_secs(60)).await;
                info!("{} rooms open", rooms.lock().unwrap().len());
            }
        }).detach()
    }

    // The target address + port is optionally specified on the command line
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "0.0.0.0:8080".to_string());

    smol::block_on(async {
        // Create the event loop and TCP listener we'll accept connections on.
        info!("Listening on: {}", addr);
        let listener = Async::<TcpListener>::bind(addr)
            .expect("Could not create listener");

        // The main loop accepts incoming connections asynchronously
        while let Ok((stream, addr)) = listener.accept().await {
            Task::spawn(handle_connection(rooms.clone(), stream,
                                          addr, close_room.clone()))
                .detach();
        }
    });

    Ok(())
}
