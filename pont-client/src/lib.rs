use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::sync::{Arc, Mutex};
use web_sys::{
    Event,
    EventTarget,
    Document,
    KeyboardEvent,
    HtmlButtonElement,
    HtmlElement,
    HtmlInputElement,
    MessageEvent,
    WebSocket,
};

use pont_common::{ClientMessage, ServerMessage};

// Minimal logging macro
macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format!($($t)*).into()))
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, PartialEq)]
enum State {
    Connecting,
    CreateOrJoin,
    InRoom,
}

struct Handle {
    ws: WebSocket,
    state: State,
}

fn document() -> Document {
    web_sys::window()
        .expect("no global `window` exists")
        .document()
        .expect("should have a document on window")
}
// Returns the HTML document and the main `div` into which we'll draw things
fn doc_div() -> (Document, HtmlElement) {
    let doc = document();
    (doc.clone(), doc.get_element_by_id("main")
         .expect("Could not find `main` div")
         .dyn_into::<HtmlElement>()
         .expect("Failed to convert into `HtmlElement`"))
}

// Boilerplate to wrap, bind, and forget a callback
fn set_event_cb<E, F, T>(obj: &E, name: &str, f: F)
    where E: JsCast + Clone + std::fmt::Debug,
          F: FnMut(T) + 'static,
          T: FromWasmAbi + 'static
{
    let cb = Closure::wrap(Box::new(f) as Box<dyn FnMut(T)>);
    let target = obj.clone()
        .dyn_into::<EventTarget>()
        .expect("Could not convert into `EventTarget`");
    target.add_event_listener_with_callback(name, cb.as_ref().unchecked_ref())
        .expect("Could not add event listener");
    cb.forget();
}

fn get_err_span() -> Result<HtmlElement, web_sys::Element> {
    let (doc, div) = doc_div();
    match doc.get_element_by_id("err_span") {
        Some(err) => err,
        None => {
            let err_div = doc.create_element("div")?;
            err_div.set_id("error");

            let i = doc.create_element("i")?;
            i.set_class_name("fas fa-exclamation-triangle");

            let span = doc.create_element("span")?;
            span.set_id("err_span");

            err_div.append_child(&i)?;
            err_div.append_child(&span)?;

            div.append_child(&err_div)?;
            span
        },
    }.dyn_into::<HtmlElement>()
}

fn clear_div(div: &HtmlElement) -> Result<(), JsValue> {
    while let Some(c) = div.first_child() {
        div.remove_child(&c)?;
    }
    Ok(())
}

////////////////////////////////////////////////////////////////////////////////

impl Handle {
    fn on_unknown_room(&mut self, room: String) -> Result<(), JsValue> {
        let err = get_err_span()?;
        err.set_text_content(Some(&format!("Could not find room '{}'", room)));

        document().get_element_by_id("play_button")
            .expect("Could not find button")
            .dyn_into::<HtmlButtonElement>()?
            .set_disabled(false);
        Ok(())
    }

    fn sender(&self) -> impl Fn(ClientMessage) {
        let ws = self.ws.clone();
        move |msg| {
            let encoded = serde_json::to_string(&msg)
                .expect("Failed to encode");
            ws.send_with_str(&encoded)
                .expect("Could not send message");
        }
    }

    fn on_chat(&self, from: String, message: String) -> Result<(), JsValue> {
        let doc = document();
        if let Some(div) = doc.get_element_by_id("chat") {
            let p = doc.create_element("p")?;
            p.set_class_name("msg");

            let b = doc.create_element("b")?;
            b.set_text_content(Some(&from));
            p.append_child(&b)?;

            let s = doc.create_element("b")?;
            s.set_text_content(Some(":"));
            p.append_child(&s)?;

            let s = doc.create_element("span")?;
            s.set_text_content(Some(&message));
            p.append_child(&s)?;

            div.append_child(&p)?;
            p.scroll_into_view();
        }
        Ok(())
    }

    fn on_information(&self, message: String) -> Result<(), JsValue> {
        let doc = document();
        if let Some(div) = doc.get_element_by_id("chat") {
            let p = doc.create_element("p")?;
            p.set_class_name("msg");

            let i = doc.create_element("i")?;
            i.set_text_content(Some(&message));
            p.append_child(&i)?;
            div.append_child(&p)?;
            p.scroll_into_view();
        }
        Ok(())
    }

    fn on_joined_room(&mut self, name: String, room: String) -> Result<(), JsValue> {
        let (doc, div) = doc_div();
        clear_div(&div)?;

        let p = doc.create_element("p")?;
        let b = doc.create_element("b")?;
        b.set_text_content(Some("Room: "));
        let s = doc.create_element("span")?;
        s.set_text_content(Some(&room));
        p.append_child(&b)?;
        p.append_child(&s)?;
        div.append_child(&p)?;

        let chat_div = doc.create_element("div")?;
        chat_div.set_id("chat");
        div.append_child(&chat_div)?;

        // Name + text input
        let p = doc.create_element("p")?;
        let chat_input = doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        chat_input.set_id("chat_input");
        chat_input.set_attribute("placeholder", "Send message...")?;

        let b = doc.create_element("b")?;
        b.set_text_content(Some(&name));
        p.append_child(&b)?;
        let b = doc.create_element("b")?;
        b.set_text_content(Some(":"));
        p.append_child(&b)?;

        p.append_child(&chat_input)?;
        div.append_child(&p)?;

        // If Enter is pressed while focus is in the chat box,
        // send a chat message to the server.
        let chat_input_ = chat_input.clone();
        let sender = self.sender();
        set_event_cb(&chat_input, "keyup", move |e: KeyboardEvent| {
            if e.key_code() == 13 { // Enter key
                e.prevent_default();
                let i = chat_input_.value();
                if !i.is_empty() {
                    chat_input_.set_value("");
                    sender(ClientMessage::Chat(i));
                }
            }
        });

        self.state = State::InRoom;
        Ok(())
    }

    fn on_message(&mut self, msg: ServerMessage) -> Result<(), JsValue> {
        use ServerMessage::*;
        console_log!("Got message {:?}", msg);
        match msg {
            UnknownRoom(name) => self.on_unknown_room(name),
            JoinedRoom{name, room} => self.on_joined_room(name, room),
            Chat{from, message} => self.on_chat(from, message),
            Information(message) => self.on_information(message),
            Players{ players, turn } => self.on_players(players, turn),
            YourTurn => self.on_my_turn(),
            NotYourTurn => self.on_not_my_turn(),
            Board(b) => self.on_board(b),
            Draw(pieces) => self.on_draw(pieces),
            InvalidMove(s) => self.on_invalid_move(s),
        }
    }

    fn on_connected(&mut self) -> Result<(), JsValue> {
        assert!(self.state == State::Connecting);
        self.state = State::CreateOrJoin;

        // Remove the "Connecting..." message
        let (doc, div) = doc_div();
        clear_div(&div)?;

        // When any of the text fields change, check to see whether
        // the "Join" button should be enabled
        let form = doc.create_element("form")?;

        let p = doc.create_element("p")?;
        let b = doc.create_element("b")?;
        b.set_text_content(Some("Name:"));
        p.append_child(&b)?;
        let name_input = doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        name_input.set_id("name_input");
        name_input.set_attribute("placeholder", "John Smith")?;
        name_input.set_required(true);
        p.append_child(&name_input)?;
        form.append_child(&p)?;

        let p = doc.create_element("p")?;
        let b = doc.create_element("b")?;
        b.set_text_content(Some("Room:"));
        p.append_child(&b)?;
        let room_input = doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        room_input.set_id("room_input");
        room_input.set_pattern("^[a-z]+ [a-z]+ [a-z]+$");
        p.append_child(&room_input)?;
        let room_input_ = room_input.clone();
        set_event_cb(&room_input, "invalid", move |_: Event| {
            room_input_.set_custom_validity("three lowercase words");
        });
        form.append_child(&p)?;

        let p = doc.create_element("p")?;
        let button = doc.create_element("button")?
            .dyn_into::<HtmlButtonElement>()?;
        button.set_text_content(Some("Create new room"));
        button.set_id("play_button");
        button.set_type("submit");
        p.append_child(&button)?;
        form.append_child(&p)?;

        div.append_child(&form)?;

        let room_input_ = room_input.clone();
        let button_ = button.clone();
        set_event_cb(&room_input, "input", move |_: Event| {
            let v = room_input_.value();
            if v.is_empty() {
                button_.set_text_content(Some("Create new room"));
            } else {
                button_.set_text_content(Some("Join existing room"));
            }
            room_input_.set_custom_validity("");
        });

        let send = self.sender();
        set_event_cb(&form, "submit", move |e: Event| {
            button.set_disabled(true);
            let name = name_input.value();
            let room = room_input.value();
            let msg = if room.is_empty() {
                ClientMessage::CreateRoom(name)
            } else {
                ClientMessage::JoinRoom(name, room)
            };
            send(msg);
            e.prevent_default();
        });

        Ok(())
    }

    fn on_players(&mut self, players: Vec<(String, usize)>, turn: usize)
        -> Result<(), JsValue>
    {
        Ok(())
    }

    fn on_my_turn(&mut self) -> Result<(), JsValue> {
        Ok(())
    }
    fn on_not_my_turn(&mut self) -> Result<(), JsValue> {
        Ok(())
    }
    fn on_board(&mut self, board: pont_common::Board) -> Result<(), JsValue> {
        Ok(())
    }
    fn on_draw(&mut self, pieces: Vec<pont_common::Piece>) -> Result<(), JsValue> {
        Ok(())
    }
    fn on_invalid_move(&mut self, msg: String) -> Result<(), JsValue> {
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();

    let doc = document();
    let body = doc.body().expect("document should have a body");

    // Manufacture the element we're gonna append
    let val = doc.create_element("p")?;
    val.set_text_content(Some("Connecting..."));

    let div = doc.create_element("div")?;
    div.set_id("main");
    div.append_child(&val)?;

    body.append_child(&div)?;

    let hostname = doc.location().unwrap().hostname()?;
    let ws = WebSocket::new(&format!("ws://{}:8080", hostname))?;

    let handle = Arc::new(Mutex::new(Handle {
        ws: ws.clone(),
        state: State::Connecting,
    }));

    let handle_ = handle.clone();
    set_event_cb(&ws, "open", move |_: JsValue| {
        let mut state = handle_.lock().unwrap();
        state.on_connected().expect("Failed to connect");
    });

    set_event_cb(&ws, "message", move |e: MessageEvent| {
        let msg = serde_json::from_str(&e.data().as_string().unwrap())
            .expect("Failed to decode message");
        let mut state = handle.lock().unwrap();
        state.on_message(msg)
            .expect("Failed to handle message");
    });

    set_event_cb(&ws, "close", move |_: Event| {
        console_log!("Socket closed");
    });

    Ok(())
}


    /*
    // Add an SVG
    let svg = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "svg")?;
    svg.set_attribute("width", "100")?;
    svg.set_attribute("hight", "100")?;
    svg.set_attribute("viewBox", "0 0 100 100")?;

    let circle = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "circle")?;
    circle.set_attribute("cx", "50")?;
    circle.set_attribute("cy", "50")?;
    circle.set_attribute("r", "20")?;
    circle.set_attribute("stroke", "black")?;
    circle.set_attribute("fill", "red")?;
    svg.append_child(&circle)?;
    body.append_child(&svg)?;
    */
