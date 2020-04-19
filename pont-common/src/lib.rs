use std::collections::HashMap;
use serde::{Serialize, Deserialize};

#[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum ClientMessage {
    CreateRoom(String),
    JoinRoom(String, String),
    Chat(String),

    /*
    Play(Vec<(Piece, i32, i32)>),
    Swap(Vec<Piece>),
    */

    Disconnected,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum ServerMessage {
    JoinedRoom {
        room_name: String,
        players: Vec<(String, u32, bool)>,
        active_player: usize,
    },
    UnknownRoom(String),
    Chat {
        from: String,
        message: String,
    },
    Information(String),
    NewPlayer(String),
    PlayerDisconnected(usize),
    PlayerTurn(usize),

    /*
    Players {
        players: Vec<(String, usize)>,
        turn: usize,
    },
    YourTurn,
    NotYourTurn,
    Board(Board), // Used to send the initial board
    Draw(Vec<Piece>),
    InvalidMove(String),
    */
}

#[derive(Copy, Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Shape {
    Clover,
    Star,
    Square,
    Diamond,
    Cross,
    Circle,
}

#[derive(Copy, Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Color {
    Orange,
    Yellow,
    Green,
    Red,
    Blue,
    Purple,
}

#[derive(Copy, Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Piece(Shape, Color);

pub type Board = HashMap<(i32, i32), Piece>;
