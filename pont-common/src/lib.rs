use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum ClientMessage {
    CreateRoom(String),
    JoinRoom(String, String),
    Chat(String),
    Disconnected,
}

#[derive(Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum ServerMessage {
    JoinedRoom {
        name: String,
        room: String,
    },
    UnknownRoom(String),
    Chat {
        from: String,
        message: String,
    },
    Information(String),
}
