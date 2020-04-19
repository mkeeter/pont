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
    HtmlTableCellElement,
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

struct PlayingState {
    chat_div: HtmlElement,
    chat_input: HtmlInputElement,
    score_table: HtmlElement,
    player_index: usize,
    active_player: usize,

    // Callback is owned so that it lives as long as the state
    _keyup_cb: Closure<dyn FnMut(KeyboardEvent)>,
}

struct CreateOrJoinState {
    name_input: HtmlInputElement,
    room_input: HtmlInputElement,
    play_button: HtmlButtonElement,
    err_div: HtmlElement,
    err_span: HtmlElement,

    // Callbacks are owned so that it lives as long as the state
    _room_invalid_cb: Closure<dyn FnMut(Event)>,
    _input_cb: Closure<dyn FnMut(Event)>,
    _submit_cb: Closure<dyn FnMut(Event)>,
}

enum State {
    Connecting,
    CreateOrJoin(CreateOrJoinState),
    Playing(PlayingState),
}

struct Handle {
    doc: Document,
    main_div: HtmlElement,
    ws: WebSocket,
    state: State,
}
unsafe impl Send for Handle { /* YOLO */}

lazy_static::lazy_static! {
    static ref HANDLE: Arc<Mutex<Handle>> = {
        let doc = web_sys::window()
            .expect("no global `window` exists")
            .document()
            .expect("should have a document on window");
        let body = doc.body().expect("document should have a body");

        // Manufacture the element we're gonna append
        let val = doc.create_element("p")
            .expect("Could not create <p>");
        val.set_text_content(Some("Connecting..."));

        let main_div = doc.create_element("div")
            .expect("Could not create main <div>")
            .dyn_into::<HtmlElement>()
            .expect("Failed to convert into `HtmlElement`");
        main_div.set_id("main");
        main_div.append_child(&val)
            .expect("Could not append child");
        body.insert_adjacent_element("afterbegin", &main_div)
            .expect("Could not append child");

        let hostname = doc.location().unwrap().hostname()
            .expect("Could not find hostname");
        let ws = WebSocket::new(&format!("ws://{}:8080", hostname))
            .expect("Could not create websocket");

        Arc::new(Mutex::new(Handle {
            doc,
            main_div,
            ws: ws,
            state: State::Connecting,
        }))
    };
}

////////////////////////////////////////////////////////////////////////////////

// Boilerplate to wrap and bind a callback.
// The resulting callback must be stored for as long as it may be used.
#[must_use]
fn set_event_cb<E, F, T>(obj: &E, name: &str, f: F) -> Closure<dyn std::ops::FnMut(T)>
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
    cb
}

////////////////////////////////////////////////////////////////////////////////

impl CreateOrJoinState {
    fn new(doc: &Document, main_div: &HtmlElement) -> Result<State, JsValue> {
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
        let room_invalid_cb = set_event_cb(&room_input, "invalid",
            move |_: Event| {
                HANDLE.lock().unwrap().set_room_invalid();
            });
        form.append_child(&p)?;

        let p = doc.create_element("p")?;
        let play_button = doc.create_element("button")?
            .dyn_into::<HtmlButtonElement>()?;
        play_button.set_text_content(Some("Create new room"));
        play_button.set_id("play_button");
        play_button.set_type("submit");
        p.append_child(&play_button)?;
        form.append_child(&p)?;

        main_div.append_child(&form)?;
        let input_cb = set_event_cb(&room_input, "input", move |_: Event| {
            HANDLE.lock().unwrap().check_join_inputs();
        });
        let submit_cb = set_event_cb(&form, "submit", move |e: Event| {
            e.prevent_default();
            HANDLE.lock().unwrap().try_join();
        });

        let err_div = doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        err_div.set_id("error");

        let i = doc.create_element("i")?;
        i.set_class_name("fas fa-exclamation-triangle");

        let err_span = doc.create_element("span")?
            .dyn_into::<HtmlElement>()?;
        err_span.set_id("err_span");

        err_div.append_child(&i)?;
        err_div.append_child(&err_span)?;
        err_div.set_hidden(true);

        main_div.append_child(&err_div)?;

        Ok(State::CreateOrJoin(CreateOrJoinState {
            name_input,
            room_input,
            play_button,
            err_div,
            err_span,

            _input_cb: input_cb,
            _submit_cb: submit_cb,
            _room_invalid_cb: room_invalid_cb,
        }))
    }
}

////////////////////////////////////////////////////////////////////////////////

impl PlayingState {
    fn new(doc: &Document, main_div: &HtmlElement,
           room_name: &str,
           players: &[(String, u32, bool)],
           active_player: usize)
        -> Result<State, JsValue>
    {
        let player_index = players.len() - 1;

        // The title lists the room name
        let p = doc.create_element("p")?;
        let b = doc.create_element("b")?;
        b.set_text_content(Some("Room: "));
        let s = doc.create_element("span")?;
        s.set_text_content(Some(room_name));
        p.append_child(&b)?;
        p.append_child(&s)?;
        main_div.append_child(&p)?;

        // This div is styled as either 1-3 columns based on screen size
        let game_div = doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        game_div.set_id("game");
        main_div.append_child(&game_div)?;

        // Add an SVG
        let svg = doc.create_element_ns(Some("http://www.w3.org/2000/svg"), "svg")?;
        svg.set_id("game");
        svg.set_attribute("width", "100")?;
        svg.set_attribute("hight", "100")?;
        svg.set_attribute("viewBox", "0 0 100 100")?;

        let circle = doc.create_element_ns(Some("http://www.w3.org/2000/svg"), "circle")?;
        circle.set_attribute("cx", "50")?;
        circle.set_attribute("cy", "50")?;
        circle.set_attribute("r", "20")?;
        circle.set_attribute("stroke", "black")?;
        circle.set_attribute("fill", "red")?;
        svg.append_child(&circle)?;
        game_div.append_child(&svg)?;

        let score_col = doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        let score_table = doc.create_element("table")?
            .dyn_into::<HtmlElement>()?;
        score_table.set_id("scores");
        let tr = doc.create_element("tr")?;
        let th = doc.create_element("th")?
            .dyn_into::<HtmlTableCellElement>()?;
        th.set_col_span(2);
        th.set_text_content(Some("Player"));
        tr.append_child(&th)?;
        let th = doc.create_element("th")?;
        th.set_text_content(Some("Score"));
        tr.append_child(&th)?;
        score_table.append_child(&tr)?;
        score_col.append_child(&score_table)?;
        game_div.append_child(&score_col)?;

        // Create the column for chatting
        let chat_col = doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        chat_col.set_id("chat_col");
        let chat_div = doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        chat_div.set_id("chat");
        chat_col.append_child(&chat_div)?;

        // Name + text input
        let chat_input_div = doc.create_element("div")?;
        chat_input_div.set_id("chat_input");

        let chat_name_div = doc.create_element("p")?;
        chat_name_div.set_id("chat_name");
        let b = doc.create_element("b")?;
        b.set_text_content(Some(&format!("{}:", players[player_index].0)));
        chat_name_div.append_child(&b)?;
        chat_input_div.append_child(&chat_name_div)?;

        let chat_input = doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        chat_input.set_id("chat_input");
        chat_input.set_attribute("placeholder", "Send message...")?;
        chat_input_div.append_child(&chat_input)?;

        chat_col.append_child(&chat_input_div)?;
        game_div.append_child(&chat_col)?;

        // If Enter is pressed while focus is in the chat box,
        // send a chat message to the server.
        let keyup_cb = set_event_cb(&chat_input, "keyup",
            move |e: KeyboardEvent| {
                if e.key_code() == 13 { // Enter key
                    e.prevent_default();
                    HANDLE.lock().unwrap().send_chat();
                }
            });

        let out = PlayingState {
            chat_input,
            chat_div,
            score_table,
            player_index,
            active_player,

            _keyup_cb: keyup_cb,
        };

        for (i, (name, score, connected)) in players.iter().enumerate() {
            out.add_player_row(doc,
                if i == player_index {
                    format!("{} (you)", name)
                } else {
                    name.to_string()
                },
                *score as usize, i == active_player, *connected)?;
        }
        Ok(State::Playing(out))
    }

    fn append_chat_message(&self, doc: &Document, from: &str, msg: &str)
        -> Result<(), JsValue>
    {
        let p = doc.create_element("p")?;
        p.set_class_name("msg");

        let b =doc.create_element("b")?;
        b.set_text_content(Some(from));
        p.append_child(&b)?;

        let s = doc.create_element("b")?;
        s.set_text_content(Some(":"));
        p.append_child(&s)?;

        let s = doc.create_element("span")?;
        s.set_text_content(Some(msg));
        p.append_child(&s)?;

        self.chat_div.append_child(&p)?;
        p.scroll_into_view();
        Ok(())
    }

    fn append_info_message(&self, doc: &Document, msg: &str)
        -> Result<(), JsValue>
    {
        let p = doc.create_element("p")?;
        p.set_class_name("msg");

        let i = doc.create_element("i")?;
        i.set_text_content(Some(msg));
        p.append_child(&i)?;
        self.chat_div.append_child(&p)?;
        p.scroll_into_view();
        Ok(())
    }

    fn add_player_row(&self, doc: &Document,
                      name: String, score: usize,
                      active: bool, connected: bool)
        -> Result<(), JsValue>
    {
        let tr = doc.create_element("tr")?;
        tr.set_class_name("player-row");
        if active {
            tr.class_list().add_1("active")?;
        }

        let td = doc.create_element("td")?;
        let i = doc.create_element("i")?;
        i.set_class_name("fas fa-caret-right");
        td.append_child(&i)?;
        tr.append_child(&td)?;

        let td = doc.create_element("td")?;
        td.set_text_content(Some(&name));
        tr.append_child(&td)?;

        let td = doc.create_element("td")?;
        td.set_text_content(Some("0"));
        tr.append_child(&td)?;

        if !connected {
            tr.class_list().add_1("disconnected")?;
        }

        self.score_table.append_child(&tr)?;

        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

impl Handle {
    fn on_unknown_room(&self, room: String) -> Result<(), JsValue> {
        if let State::CreateOrJoin(s) = &self.state {
            let err = format!("Could not find room '{}'", room);
            s.err_span.set_text_content(Some(&err));
            s.err_div.set_hidden(false);
            s.play_button.set_disabled(false);
        }
        Ok(())
    }

    fn send(&self, msg: ClientMessage) {
        let encoded = serde_json::to_string(&msg)
            .expect("Failed to encode");
        self.ws.send_with_str(&encoded)
            .expect("Could not send message");
    }

    fn on_chat(&self, from: String, message: String) -> Result<(), JsValue> {
        if let State::Playing(state) = &self.state {
            state.append_chat_message(&self.doc, &from, &message)
        } else {
            Ok(())
        }
    }

    fn on_information(&self, message: String) -> Result<(), JsValue> {
        if let State::Playing(state) = &self.state {
            state.append_info_message(&self.doc, &message)
        } else {
            Ok(())
        }
    }

    fn clear_main_div(&self) -> Result<(), JsValue> {
        while let Some(c) = self.main_div.first_child() {
            self.main_div.remove_child(&c)?;
        }
        Ok(())
    }

    fn on_joined_room(&mut self, room_name: String,
                      players: Vec<(String, u32, bool)>,
                      active_player: usize)
        -> Result<(), JsValue>
    {
        self.clear_main_div()?;
        self.state = PlayingState::new(
            &self.doc, &self.main_div,
            &room_name,
            &players, active_player)?;
        Ok(())
    }

    fn send_chat(&self) {
        if let State::Playing(state) = &self.state {
            let i = state.chat_input.value();
            if !i.is_empty() {
                state.chat_input.set_value("");
                self.send(ClientMessage::Chat(i));
            }
        }
    }

    fn on_message(&mut self, msg: ServerMessage) -> Result<(), JsValue> {
        use ServerMessage::*;
        console_log!("Got message {:?}", msg);
        match msg {
            UnknownRoom(name) => self.on_unknown_room(name),
            JoinedRoom{room_name, players, active_player} =>
                self.on_joined_room(room_name, players, active_player),
            Chat{from, message} => self.on_chat(from, message),
            Information(message) => self.on_information(message),
            NewPlayer(name) => self.on_new_player(name),
            PlayerDisconnected(index) => self.on_player_disconnected(index),
            PlayerTurn(active_player) => self.on_player_turn(active_player),
            /*
            Players{ players, turn } => self.on_players(players, turn),
            YourTurn => self.on_my_turn(),
            NotYourTurn => self.on_not_my_turn(),
            Board(b) => self.on_board(b),
            Draw(pieces) => self.on_draw(pieces),
            InvalidMove(s) => self.on_invalid_move(s),
            */
        }
    }

    fn on_player_disconnected(&self, index: usize) -> Result<(), JsValue> {
        if let State::Playing(state) = &self.state {
            let c = state.score_table.child_nodes()
                .item((index + 1) as u32)
                .unwrap()
                .dyn_into::<HtmlElement>()?;
            c.class_list().add_1("disconnected")?;
        }
        Ok(())
    }

    fn on_player_turn(&mut self, active_player: usize) -> Result<(), JsValue> {
        if let State::Playing(state) = &mut self.state {
            let children = state.score_table.child_nodes();
            children
                .item((state.active_player + 1) as u32)
                .unwrap()
                .dyn_into::<HtmlElement>()?
                .class_list()
                .remove_1("active")?;

            state.active_player = active_player;
            children
                .item((state.active_player + 1) as u32)
                .unwrap()
                .dyn_into::<HtmlElement>()?
                .class_list()
                .add_1("active")?;
        }
        Ok(())
    }

    fn on_new_player(&self, name: String) -> Result<(), JsValue> {
        // Append a player to the bottom of the scores list
        if let State::Playing(state) = &self.state {
            state.add_player_row(&self.doc, name.clone(), 0, false, true)?;
            state.append_info_message(&self.doc,
                                      &format!("{} joined the room", name))?;
        }
        Ok(())
    }

    fn on_connected(&mut self) -> Result<(), JsValue> {
        // Remove the "Connecting..." message
        self.clear_main_div()?;
        self.state = CreateOrJoinState::new(&self.doc, &self.main_div)?;

        // Insta-join a room
        //self.send(ClientMessage::CreateRoom("Matt".to_string()));
        Ok(())
    }

    fn set_room_invalid(&mut self) {
        if let State::CreateOrJoin(state) = &self.state {
            state.room_input.set_custom_validity("three lowercase words");
        }
    }

    fn check_join_inputs(&mut self) {
        if let State::CreateOrJoin(state) = &self.state {
            state.play_button.set_text_content(Some(
                    if state.room_input.value().is_empty() {
                        "Create new room"
                    } else {
                        "Join existing room"
                    }));
            state.room_input.set_custom_validity("");
        }
    }

    fn try_join(&mut self) {
        if let State::CreateOrJoin(s) = &self.state {
            s.play_button.set_disabled(true);
            let name = s.name_input.value();
            let room = s.room_input.value();
            let msg = if room.is_empty() {
                ClientMessage::CreateRoom(name)
            } else {
                ClientMessage::JoinRoom(name, room)
            };
            self.send(msg);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();

    // These callbacks are deliberately forgotten so that they aren't
    // destroyed when main() ends.  Other callbacks are owned in State
    // objects, but that's not an option here.
    set_event_cb(&HANDLE.lock().unwrap().ws, "open", move |_: JsValue| {
        let mut state = HANDLE.lock().unwrap();
        state.on_connected().expect("Failed to connect");
    }).forget();

    set_event_cb(&HANDLE.lock().unwrap().ws, "message", move |e: MessageEvent| {
        let msg = serde_json::from_str(&e.data().as_string().unwrap())
            .expect("Failed to decode message");
        let mut state = HANDLE.lock().unwrap();
        state.on_message(msg)
            .expect("Failed to handle message");
    }).forget();

    set_event_cb(&HANDLE.lock().unwrap().ws, "close", move |_: Event| {
        console_log!("Socket closed");
    }).forget();

    Ok(())
}
