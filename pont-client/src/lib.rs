use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::sync::{Arc, Mutex};
use web_sys::{
    Element,
    Event,
    EventTarget,
    Document,
    KeyboardEvent,
    HtmlButtonElement,
    HtmlElement,
    HtmlTableCellElement,
    HtmlInputElement,
    MessageEvent,
    PointerEvent,
    SvgGraphicsElement,
    WebSocket,
};

use pont_common::{ClientMessage, ServerMessage, Shape, Color, Piece};

// Minimal logging macro
macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format!($($t)*).into()))
}

////////////////////////////////////////////////////////////////////////////////

type Pos = (f32, f32);
enum DragState {
    Idle,
    Dragging(Element, Pos),
    Dropping {
        target: Element,
        start: Pos,
        end: Pos,
        t0: f64
    },
}

struct Board {
    doc: Document,
    svg: SvgGraphicsElement,

    drag: DragState,

    pointer_down_cb: Closure<dyn FnMut(PointerEvent)>,
    pointer_move_cb: Closure<dyn FnMut(PointerEvent)>,
    pointer_up_cb: Closure<dyn FnMut(PointerEvent)>,
    anim_cb: Closure<dyn FnMut(f64)>,
}

struct PlayingState {
    chat_div: HtmlElement,
    chat_input: HtmlInputElement,
    score_table: HtmlElement,
    player_index: usize,
    active_player: usize,

    board: Board,

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
fn build_cb<F, T>(f: F) -> Closure<dyn std::ops::FnMut(T)>
    where F: FnMut(T) + 'static,
          T: FromWasmAbi + 'static
{
    Closure::wrap(Box::new(f) as Box<dyn FnMut(T)>)
}

#[must_use]
fn set_event_cb<E, F, T>(obj: &E, name: &str, f: F) -> Closure<dyn std::ops::FnMut(T)>
    where E: JsCast + Clone + std::fmt::Debug,
          F: FnMut(T) + 'static,
          T: FromWasmAbi + 'static
{
    let cb = build_cb(f);
    let target = obj.clone()
        .dyn_into::<EventTarget>()
        .expect("Could not convert into `EventTarget`");
    target.add_event_listener_with_callback(name, cb.as_ref().unchecked_ref())
        .expect("Could not add event listener");
    cb
}

////////////////////////////////////////////////////////////////////////////////

impl Board {
    fn new(doc: &Document, game_div: &HtmlElement) -> Result<Board, JsValue> {
        // Add an SVG
        let svg = doc.create_element_ns(Some("http://www.w3.org/2000/svg"), "svg")?
            .dyn_into::<SvgGraphicsElement>()?;
        svg.set_id("game");
        svg.set_attribute("width", "100")?;
        svg.set_attribute("hight", "100")?;
        svg.set_attribute("viewBox", "0 0 200 200")?;
        game_div.append_child(&svg)?;

        let pointer_down_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .canvas_pointer_down(evt)
                .expect("Failed to pointer_down event");
        });
        let pointer_move_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .canvas_pointer_move(evt)
                .expect("Failed to pointer_down event");
        });
        let pointer_up_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .canvas_pointer_up(evt)
                .expect("Failed to pointer_down event");
        });
        let anim_cb = build_cb(move |evt: f64| {
            HANDLE.lock().unwrap()
                .canvas_anim(evt)
                .expect("Failed to anim event");
        });

        let mut out = Board {
            doc: doc.clone(),
            drag: DragState::Idle,
            svg,
            pointer_down_cb,
            pointer_up_cb,
            pointer_move_cb,
            anim_cb};
        out.add_piece((Shape::Circle, Color::Red), 0, 0)?;
        out.add_piece((Shape::Square, Color::Blue), 2, 0)?;
        out.add_piece((Shape::Clover, Color::Yellow), 1, 0)?;
        out.add_piece((Shape::Diamond, Color::Green), 3, 0)?;
        out.add_piece((Shape::Cross, Color::Purple), 4, 0)?;
        out.add_piece((Shape::Star, Color::Orange), 5, 0)?;
        Ok(out)
    }

    fn get_transform(e: &Element) -> (f32, f32) {
        let t = e.get_attribute("transform").unwrap();
        let s = t.chars()
            .filter(|&c| c.is_digit(10) || c == ' ' || c == '.' || c == '-')
            .collect::<String>();
        let mut itr = s.split(' ')
            .map(|s| s.parse().unwrap());

        let dx = itr.next().unwrap();
        let dy = itr.next().unwrap();

        (dx, dy)
    }

    fn mouse_pos(&self, evt: PointerEvent) -> (f32, f32) {
        let mat = self.svg.get_screen_ctm().unwrap();
        let x = (evt.client_x() as f32 - mat.e()) / mat.a();
        let y = (evt.client_y() as f32 - mat.f()) / mat.d();
        (x, y)
    }

    fn pointer_down(&mut self, evt: PointerEvent) -> Result<(), JsValue> {
        evt.prevent_default();
        let t = evt.target().unwrap();

        let mut t = t.dyn_into::<Element>()?;

        // Walk up the tree to find the piece's <g> group,
        // which sets its position with a translation
        while !t.has_attribute("transform") {
            t = t.parent_node().unwrap().dyn_into::<Element>()?;
        }
        self.svg.remove_child(&t)?;
        self.svg.append_child(&t)?;

        t.set_pointer_capture(evt.pointer_id())?;
        t.add_event_listener_with_callback("pointermove",
                self.pointer_move_cb.as_ref().unchecked_ref())
            .expect("Could not add event listener");
        t.add_event_listener_with_callback("pointerup",
                self.pointer_up_cb.as_ref().unchecked_ref())
            .expect("Could not add event listener");
        let (mx, my) = self.mouse_pos(evt);
        let (dx, dy) = Self::get_transform(&t);

        self.drag = DragState::Dragging(t, (mx - dx, my - dy));
        Ok(())
    }

    fn pointer_up(&mut self, evt: PointerEvent) -> Result<(), JsValue> {
        console_log!("pointer up {:?}", evt);
        evt.prevent_default();
        if let DragState::Dragging(t, _offset) = &self.drag {
            t.remove_event_listener_with_callback("pointermove",
                    self.pointer_move_cb.as_ref().unchecked_ref())
                .expect("Could not remove event listener");
            t.remove_event_listener_with_callback("pointerup",
                    self.pointer_up_cb.as_ref().unchecked_ref())
                .expect("Could not remove event listener");
            let (x, y) = Self::get_transform(&t);
            web_sys::window()
                .expect("no global `window` exists")
                .request_animation_frame(self.anim_cb.as_ref()
                                         .unchecked_ref())?;
            self.drag = DragState::Dropping {
                target: t.clone(),
                start: (x, y),
                end: ((x / 10.0).round() * 10.0, (y / 10.0).round() * 10.0),
                t0: evt.time_stamp(),
            };
        }

        Ok(())
    }

    fn pointer_move(&self, evt: PointerEvent) -> Result<(), JsValue> {
        evt.prevent_default();
        if let DragState::Dragging(t, (dx, dy)) = &self.drag {
            let (mx, my) = self.mouse_pos(evt);
            t.set_attribute("transform", &format!("translate({} {})",
                mx - dx,
                my - dy))?;
        }
        Ok(())
    }

    fn anim(&mut self, t: f64) -> Result<(), JsValue> {
        if let DragState::Dropping{target, start, end, t0} = &mut self.drag {
            let anim_length = 10.0;
            let mut frac = ((t - *t0) / anim_length) as f32;
            if frac > 1.0 {
                frac = 1.0;
            }
            let x = start.0 * (1.0 - frac) + end.0 * frac;
            let y = start.1 * (1.0 - frac) + end.1 * frac;
            target.set_attribute("transform",
                                 &format!("translate({} {})", x, y))?;
            if frac < 1.0 {
                web_sys::window()
                    .expect("no global `window` exists")
                    .request_animation_frame(self.anim_cb.as_ref()
                                             .unchecked_ref())?;
            }
        }
        Ok(())
    }

    fn create(&self, t: &str) -> Result<Element, JsValue> {
        self.doc.create_element_ns(Some("http://www.w3.org/2000/svg"), t)
    }

    fn add_piece(&mut self, p: Piece, x: i32, y: i32) -> Result<(), JsValue> {
        let g = self.create("g")?;
        let r = self.create("rect")?;
        r.class_list().add_1("piece")?;
        r.set_attribute("width", "10.0")?;
        r.set_attribute("height", "10.0")?;
        let s = match p.0 {
            Shape::Circle => {
                let s = self.create("circle")?;
                s.set_attribute("r", "3.0")?;
                s.set_attribute("cx", "5.0")?;
                s.set_attribute("cy", "5.0")?;
                s
            },
            Shape::Square => {
                let s = self.create("rect")?;
                s.set_attribute("width", "6.0")?;
                s.set_attribute("height", "6.0")?;
                s.set_attribute("x", "2.0")?;
                s.set_attribute("y", "2.0")?;
                s
            }
            Shape::Clover => {
                let s = self.create("g")?;
                for (x, y) in &[(5.0, 3.0), (5.0, 7.0), (3.0, 5.0), (7.0, 5.0)]
                {
                    let c = self.create("circle")?;
                    c.set_attribute("r", "1.5")?;
                    c.set_attribute("cx", &x.to_string())?;
                    c.set_attribute("cy", &y.to_string())?;
                    s.append_child(&c)?;
                }
                let r = self.create("rect")?;
                r.set_attribute("width", "4.0")?;
                r.set_attribute("height", "3.0")?;
                r.set_attribute("x", "3.0")?;
                r.set_attribute("y", "3.5")?;
                s.append_child(&r)?;

                let r = self.create("rect")?;
                r.set_attribute("width", "3.0")?;
                r.set_attribute("height", "4.0")?;
                r.set_attribute("x", "3.5")?;
                r.set_attribute("y", "3.0")?;
                s.append_child(&r)?;

                s
            }
            Shape::Diamond => {
                let s = self.create("polygon")?;
                s.set_attribute("points", "2,5 5,8 8,5 5,2")?;
                s
            }
            Shape::Cross => {
                let s = self.create("polygon")?;
                s.set_attribute("points", "2,2 3.5,5 2,8 5,6.5 8,8 6.5,5 8,2 5,3.5")?;
                s
            }
            Shape::Star => {
                let g = self.create("g")?;
                let s = self.create("polygon")?;
                s.set_attribute("points", "3,3 4,5 3,7 5,6 7,7 6,5 7,3 5,4")?;
                g.append_child(&s)?;
                let s = self.create("polygon")?;
                s.set_attribute("points", "1,5 4,6 5,9 6,6 9,5 6,4 5,1 4,4")?;
                g.append_child(&s)?;
                g
            }
        };
        s.class_list().add_1(match p.1 {
            Color::Orange => "shape-orange",
            Color::Yellow => "shape-yellow",
            Color::Green => "shape-green",
            Color::Red => "shape-red",
            Color::Blue => "shape-blue",
            Color::Purple => "shape-purple",
        })?;

        g.append_child(&r)?;
        g.append_child(&s)?;
        g.set_attribute("transform", &format!("translate({} {})", x * 10, y * 10))?;

        let target = g.clone()
            .dyn_into::<EventTarget>()
            .expect("Could not convert into `EventTarget`");
        target.add_event_listener_with_callback("pointerdown",
                self.pointer_down_cb.as_ref().unchecked_ref())
            .expect("Could not add event listener");

        self.svg.append_child(&g)?;
        Ok(())
    }
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

        let board = Board::new(doc, &game_div)?;

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
            board,

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
        td.set_text_content(Some(&format!("{}", score)));
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
            Ok(())
        } else {
            panic!("Invalid state");
        }
    }

    fn canvas_pointer_down(&mut self, evt: PointerEvent) -> Result<(), JsValue> {
        if let State::Playing(state) = &mut self.state {
            state.board.pointer_down(evt)
        } else {
            panic!("Invalid state");
        }
    }

    fn canvas_pointer_move(&self, evt: PointerEvent) -> Result<(), JsValue> {
        if let State::Playing(state) = &self.state {
            state.board.pointer_move(evt)
        } else {
            panic!("Invalid state");
        }
    }

    fn canvas_pointer_up(&mut self, evt: PointerEvent) -> Result<(), JsValue> {
        if let State::Playing(state) = &mut self.state {
            state.board.pointer_up(evt)
        } else {
            panic!("Invalid state");
        }
    }

    fn canvas_anim(&mut self, t: f64) -> Result<(), JsValue> {
        if let State::Playing(state) = &mut self.state {
            state.board.anim(t)
        } else {
            panic!("Invalid state");
        }
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
            panic!("Invalid state");
        }
    }

    fn on_information(&self, message: String) -> Result<(), JsValue> {
        if let State::Playing(state) = &self.state {
            state.append_info_message(&self.doc, &message)
        } else {
            panic!("Invalid state");
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
        } else {
            panic!("Invalid state");
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
            Ok(())
        } else {
            panic!("Invalid state");
        }
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
            Ok(())
        } else {
            panic!("Invalid state");
        }
    }

    fn on_new_player(&self, name: String) -> Result<(), JsValue> {
        // Append a player to the bottom of the scores list
        if let State::Playing(state) = &self.state {
            state.add_player_row(&self.doc, name.clone(), 0, false, true)?;
            state.append_info_message(&self.doc,
                                      &format!("{} joined the room", name))?;
            Ok(())
        } else {
            panic!("Invalid state");
        }
    }

    fn on_connected(&mut self) -> Result<(), JsValue> {
        // Remove the "Connecting..." message
        self.clear_main_div()?;
        self.state = CreateOrJoinState::new(&self.doc, &self.main_div)?;

        // Insta-join a room
        self.send(ClientMessage::CreateRoom("Matt".to_string()));
        Ok(())
    }

    fn set_room_invalid(&mut self) {
        if let State::CreateOrJoin(state) = &self.state {
            state.room_input.set_custom_validity("three lowercase words");
        } else {
            panic!("Invalid state");
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
        } else {
            panic!("Invalid state");
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
        } else {
            panic!("Invalid state");
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
