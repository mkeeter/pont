use std::collections::HashMap;
use serde::{Serialize, Deserialize};

use rand::thread_rng;
use rand::seq::SliceRandom;

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
        board: HashMap<(i32, i32), Piece>,
        pieces: Vec<Piece>,
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

pub type Piece = (Shape, Color);

#[derive(Debug, Deserialize, Serialize)]
pub struct Game {
    pub board: HashMap<(i32, i32), Piece>,
    pub bag: Vec<Piece>,
}

impl Game {
    pub fn new() -> Game {
        use Color::*;
        use Shape::*;
        let mut bag = Vec::new();
        for c in &[Orange, Yellow, Green, Red, Blue, Purple] {
            for s in &[Clover, Star, Square, Diamond, Cross, Circle] {
                for _ in 0..3 {
                    bag.push((*s, *c));
                }
            }
        }
        bag.shuffle(&mut thread_rng());

        Game {
            board: HashMap::new(), bag
        }
    }

    pub fn deal(&mut self) -> Vec<Piece> {
        let mut out = Vec::new();
        for _ in 0..7 {
            if let Some(p) = self.bag.pop() {
                out.push(p);
            }
        }
        out
    }

    pub fn exchange(&mut self, pieces: Vec<Piece>) -> Option<Vec<Piece>> {
        if pieces.len() <= self.bag.len() {
            let mut out = Vec::new();
            for _ in 0..pieces.len() {
                out.push(self.bag.pop().unwrap());
            }
            for p in pieces.into_iter() {
                self.bag.push(p);
            }
            self.bag.shuffle(&mut thread_rng());
            Some(out)
        } else {
            None
        }
    }
}
