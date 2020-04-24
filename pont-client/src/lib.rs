use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::collections::HashMap;
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

type JsResult<T> = Result<T, JsValue>;
type JsError = Result<(), JsValue>;

////////////////////////////////////////////////////////////////////////////////

macro_rules! methods {
    ($($sub:ident => [$($name:ident($($var:ident: $type:ty),*)),+ $(,)?]),+
       $(,)?) =>
    {
        $($(
        fn $name(&mut self, $($var: $type),* ) -> JsError {
            match self {
                State::$sub(s) => s.$name($($var),*),
                _ => panic!("Invalid state"),
            }
        }
        )+)+
    }
}

macro_rules! transitions {
    ($($sub:ident => [$($name:ident($($var:ident: $type:ty),*)
                        -> $into:ident),+ $(,)?]),+$(,)?) =>
    {
        $($(
        fn $name(&mut self, $($var: $type),* ) -> JsError {
            let s = std::mem::replace(self, State::Empty);
            match s {
                State::$sub(s) => *self = State::$into(s.$name($($var),*)?),
                _ => panic!("Invalid state"),
            }
            Ok(())
        }
        )+)+
    }
}

////////////////////////////////////////////////////////////////////////////////

type Pos = (f32, f32);
struct Dragging {
    target: Element,
    shadow: Element,
    offset: Pos,
    grid_origin: Option<(i32, i32)>,
    hand_index: usize,
}

struct TileAnimation {
    target: Element,
    start: Pos,
    end: Pos,
    t0: f64,
}

impl TileAnimation {
    // Returns true if the animation should keep running
    fn run(&self, t: f64) -> JsResult<bool> {
        let anim_length = 100.0;
        let mut frac = ((t - self.t0) / anim_length) as f32;
        if frac > 1.0 {
            frac = 1.0;
        }
        let x = self.start.0 * (1.0 - frac) + self.end.0 * frac;
        let y = self.start.1 * (1.0 - frac) + self.end.1 * frac;
        self.target.set_attribute("transform", &format!("translate({} {})",
                                                        x, y))?;
        Ok(frac < 1.0)
    }
}

struct DropToGrid {
    anim: TileAnimation,
    shadow: Element,
}

struct ReturnToHand {
    anim: TileAnimation,
}

enum DragState {
    Idle,
    Dragging(Dragging),
    DropToGrid(DropToGrid),
    ReturnToHand(ReturnToHand),
}

pub struct Board {
    doc: Document,
    svg: SvgGraphicsElement,

    drag: DragState,

    grid: HashMap<(i32, i32), Piece>,
    tentative: HashMap<(i32, i32), usize>,
    hand: Vec<Piece>,

    pointer_down_cb: Closure<dyn FnMut(PointerEvent)>,
    pointer_move_cb: Closure<dyn FnMut(PointerEvent)>,
    pointer_up_cb: Closure<dyn FnMut(PointerEvent)>,
    anim_cb: Closure<dyn FnMut(f64)>,
}

impl Board {
    fn new(doc: &Document, game_div: &HtmlElement) -> JsResult<Board> {
        // Add an SVG
        let svg = doc.create_element_ns(
                Some("http://www.w3.org/2000/svg"), "svg")?
            .dyn_into::<SvgGraphicsElement>()?;

        let dummy = doc.create_element_ns(
                Some("http://www.w3.org/2000/svg"), "svg")?
            .dyn_into::<SvgGraphicsElement>()?;
        dummy.set_attribute("viewBox", "0 0 200 200")?;
        dummy.set_id("dummy");

        svg.set_id("game");
        svg.set_attribute("width", "100")?;
        svg.set_attribute("hight", "100")?;
        svg.set_attribute("viewBox", "0 0 200 200")?;

        let svg_div = doc.create_element("div")?;
        svg_div.set_id("svg");
        svg_div.append_child(&dummy)?;

        let hand_div = doc.create_element("div")?;
        hand_div.set_id("hand");
        hand_div.set_class_name("background");
        svg_div.append_child(&hand_div)?;

        let board_div = doc.create_element("div")?;
        board_div.set_class_name("background");
        board_div.set_id("board");
        svg_div.append_child(&board_div)?;

        let accept_button = doc.create_element("button")?
            .dyn_into::<HtmlButtonElement>()?;
        accept_button.set_inner_html("<i class=\"fas fa-check\"></i>");
        accept_button.set_id("accept_button");
        accept_button.set_class_name("gameplay");
        svg_div.append_child(&accept_button)?;
        set_event_cb(&accept_button, "click", move |e: Event| {
            e.prevent_default();
            console_log!("CLICKED");
        }).forget();

        let reject_button = doc.create_element("button")?
            .dyn_into::<HtmlButtonElement>()?;
        reject_button.set_inner_html("<i class=\"fas fa-times\"></i>");
        reject_button.set_id("reject_button");
        reject_button.set_class_name("gameplay");
        svg_div.append_child(&reject_button)?;

        svg_div.append_child(&svg)?;

        game_div.append_child(&svg_div)?;

        let pointer_down_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_down(evt)
                .expect("Failed to pointer_down event");
        });
        let pointer_move_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_move(evt)
                .expect("Failed to pointer_down event");
        });
        let pointer_up_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_up(evt)
                .expect("Failed to pointer_down event");
        });
        let anim_cb = build_cb(move |evt: f64| {
            HANDLE.lock().unwrap()
                .on_anim(evt)
                .expect("Failed to anim event");
        });

        let out = Board {
            doc: doc.clone(),
            drag: DragState::Idle,
            svg,
            grid: HashMap::new(),
            tentative: HashMap::new(),
            hand: Vec::new(),
            pointer_down_cb,
            pointer_up_cb,
            pointer_move_cb,
            anim_cb};

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

    fn mouse_pos(&self, evt: &PointerEvent) -> (f32, f32) {
        let mat = self.svg.get_screen_ctm().unwrap();
        let x = (evt.client_x() as f32 - mat.e()) / mat.a();
        let y = (evt.client_y() as f32 - mat.f()) / mat.d();
        (x, y)
    }

    fn on_pointer_down(&mut self, evt: PointerEvent) -> JsError {
        // We only drag if nothing else is dragging;
        // no fancy multi-touch dragging here.
        match self.drag {
            DragState::Idle => (),
            _ => return Ok(()),
        }

        evt.prevent_default();
        let mut target = evt.target()
            .unwrap()
            .dyn_into::<Element>()?;

        // Shadow goes underneath the dragged piece
        let shadow = self.create("rect")?;
        shadow.class_list().add_1("shadow")?;
        shadow.set_attribute("width", "10.0")?;
        shadow.set_attribute("height", "10.0")?;
        shadow.set_attribute("visibility", "hidden")?;
        self.svg.append_child(&shadow)?;

        // Walk up the tree to find the piece's <g> group,
        // which sets its position with a translation
        while !target.has_attribute("transform") {
            target = target.parent_node().unwrap().dyn_into::<Element>()?;
        }
        self.svg.remove_child(&target)?;
        self.svg.append_child(&target)?;

        target.set_pointer_capture(evt.pointer_id())?;
        target.add_event_listener_with_callback("pointermove",
                self.pointer_move_cb.as_ref().unchecked_ref())
            .expect("Could not add event listener");
        target.add_event_listener_with_callback("pointerup",
                self.pointer_up_cb.as_ref().unchecked_ref())
            .expect("Could not add event listener");
        let (mx, my) = self.mouse_pos(&evt);
        let (dx, dy) = Self::get_transform(&target);

        let x = dx.round() as i32;
        let y = dy.round() as i32;
        console_log!("Got piece from {} {}", x, y);

        let (hand_index, grid_origin) = if y == 185 {
            let i = ((x - 5) / 15) as usize;
            (i, None)
        } else {
            let x = x / 10;
            let y = y / 10;
            (self.tentative.remove(&(x, y)).unwrap(), Some((x, y)))
        };

        self.drag = DragState::Dragging(Dragging {
            target,
            shadow,
            offset: (mx - dx, my - dy),
            hand_index,
            grid_origin,
        });
        Ok(())
    }

    fn on_pointer_move(&self, evt: PointerEvent) -> JsError {
        if let DragState::Dragging(d) = &self.drag {
            evt.prevent_default();
            let (mx, my) = self.mouse_pos(&evt);

            let x = mx - d.offset.0;
            let y = my - d.offset.1;
            d.target.set_attribute("transform",
                                   &format!("translate({} {})", x, y))?;
            let tx = (x / 10.0).round() as i32;
            let ty = (y / 10.0).round() as i32;

            let overlapping = self.grid.contains_key(&(tx, ty)) ||
                              self.tentative.contains_key(&(tx, ty));
            let offscreen = tx < 0 || tx >= 20 || ty < 0 || ty >= 20;
            if ty < 18 && !overlapping && !offscreen {
                let x = tx as f32 * 10.0;
                let y = ty as f32 * 10.0;
                d.shadow.set_attribute("x", &x.to_string())?;
                d.shadow.set_attribute("y", &y.to_string())?;
                d.shadow.set_attribute("visibility", "visible")?;
            } else {
                d.shadow.set_attribute("visibility", "hidden")?;
            }
        }
        Ok(())
    }

    fn on_pointer_up(&mut self, evt: PointerEvent) -> JsError {
        console_log!("pointer up {:?}", evt);
        if let DragState::Dragging(d) = &self.drag {
            evt.prevent_default();
            let (mx, my) = self.mouse_pos(&evt);

            let x = mx - d.offset.0;
            let y = my - d.offset.1;

            d.target.remove_event_listener_with_callback("pointermove",
                    self.pointer_move_cb.as_ref().unchecked_ref())
                .expect("Could not remove event listener");
            d.target.remove_event_listener_with_callback("pointerup",
                    self.pointer_up_cb.as_ref().unchecked_ref())
                .expect("Could not remove event listener");

            let tx = (x / 10.0).round() as i32;
            let ty = (y / 10.0).round() as i32;

            let overlapping = self.grid.contains_key(&(tx, ty)) ||
                              self.tentative.contains_key(&(tx, ty));
            let offscreen = tx < 0 || tx >= 20 || ty < 0 || ty >= 20;
            console_log!("Dropping at {} {}", tx, ty);
            self.drag = if ty >= 18 {
                // Drop to hand
                self.svg.remove_child(&d.shadow)?;
                DragState::ReturnToHand(ReturnToHand{
                    anim: TileAnimation {
                        target: d.target.clone(),
                        start: (x, y),
                        end: ((d.hand_index * 15 + 5) as f32, 185.0),
                        t0: evt.time_stamp()
                    }})
            } else if !overlapping && !offscreen {
                // Insert into the grid with a dropping animation
                self.tentative.insert((tx, ty), d.hand_index);
                DragState::DropToGrid(DropToGrid{
                    anim: TileAnimation {
                        target: d.target.clone(),
                        start: (x, y),
                        end: (tx as f32 * 10.0, ty as f32 * 10.0),
                        t0: evt.time_stamp(),
                    },
                    shadow: d.shadow.clone(),
                })
            } else {
                // Return to its previous grid position or your hand
                match d.grid_origin {
                    None => {
                        self.svg.remove_child(&d.shadow)?;
                        DragState::ReturnToHand(ReturnToHand{
                            anim: TileAnimation {
                                target: d.target.clone(),
                                start: (x, y),
                                end: ((d.hand_index * 15 + 5) as f32, 185.0),
                                t0: evt.time_stamp()
                            }
                        })
                    }
                    Some((gx, gy)) => {
                        self.tentative.insert((gx, gy), d.hand_index);
                        DragState::DropToGrid(DropToGrid{
                            anim: TileAnimation {
                                target: d.target.clone(),
                                start: (x, y),
                                end: ((gx * 10) as f32, (gy * 10) as f32),
                                t0: evt.time_stamp(),
                            },
                            shadow: d.shadow.clone(),
                        })
                    }
                }
            };

            web_sys::window()
                .expect("no global `window` exists")
                .request_animation_frame(self.anim_cb.as_ref()
                                         .unchecked_ref())?;

        }
        Ok(())
    }

    fn on_anim(&mut self, t: f64) -> JsError {
        match &mut self.drag {
            DragState::DropToGrid(d) => {
                if d.anim.run(t)? {
                    web_sys::window()
                        .expect("no global `window` exists")
                        .request_animation_frame(self.anim_cb.as_ref()
                                                 .unchecked_ref())?;
                } else {
                    self.svg.remove_child(&d.shadow)?;
                    self.drag = DragState::Idle;
                }
            },
            DragState::ReturnToHand(d) => {
                if d.anim.run(t)? {
                    web_sys::window()
                        .expect("no global `window` exists")
                        .request_animation_frame(self.anim_cb.as_ref()
                                                 .unchecked_ref())?;
                } else {
                    self.drag = DragState::Idle;
                }
            }
            _ => panic!("Invalid state"),
        }
        Ok(())
    }

    fn create(&self, t: &str) -> JsResult<Element> {
        self.doc.create_element_ns(Some("http://www.w3.org/2000/svg"), t)
    }

    fn add_hand(&mut self, p: Piece) -> JsError {
        self.hand.push(p);
        let g = self.new_piece(p)?;
        g.class_list().add_1("piece")?;
        g.set_attribute("transform", &format!("translate({} 185)",
                                              5 + 15 * (self.hand.len() - 1)))?;

        let target = g.clone()
            .dyn_into::<EventTarget>()
            .expect("Could not convert into `EventTarget`");
        target.add_event_listener_with_callback("pointerdown",
                self.pointer_down_cb.as_ref().unchecked_ref())
            .expect("Could not add event listener");

        Ok(())
    }

    fn new_piece(&self, p: Piece) -> JsResult<Element> {
        let g = self.create("g")?;
        let r = self.create("rect")?;
        r.class_list().add_1("tile")?;
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

        self.svg.append_child(&g)?;

        Ok(g)
    }

    fn add_piece(&mut self, p: Piece, x: i32, y: i32) -> JsError {
        self.grid.insert((x, y), p);

        let g = self.new_piece(p)?;
        g.class_list().add_1("placed")?;
        g.set_attribute("transform", &format!("translate({} {})", x * 10, y * 10))?;

        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct Base {
    doc: Document,
    main_div: HtmlElement,
    ws: WebSocket,

    _open_cb: Closure<dyn FnMut(JsValue)>,
    _message_cb: Closure<dyn FnMut(MessageEvent)>,
    _close_cb: Closure<dyn FnMut(Event)>,
}

impl Base {
    fn clear_main_div(&self) -> JsError {
        while let Some(c) = self.main_div.first_child() {
            self.main_div.remove_child(&c)?;
        }
        Ok(())
    }

    fn send(&self, msg: ClientMessage) -> JsError {
        let encoded = serde_json::to_string(&msg)
            .map_err(|e| JsValue::from_str(
                    &format!("Could not encode: {}", e)))?;
        self.ws.send_with_str(&encoded)
    }
}

////////////////////////////////////////////////////////////////////////////////

// These are the states in the system
struct Connecting {
    base: Base
}

struct CreateOrJoin {
    base: Base,

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

struct Playing {
    base: Base,

    chat_div: HtmlElement,
    chat_input: HtmlInputElement,
    score_table: HtmlElement,
    player_index: usize,
    active_player: usize,

    board: Board,

    // Callback is owned so that it lives as long as the state
    _keyup_cb: Closure<dyn FnMut(KeyboardEvent)>,
}

////////////////////////////////////////////////////////////////////////////////

enum State {
    Connecting(Connecting),
    CreateOrJoin(CreateOrJoin),
    Playing(Playing),
    Empty,
}

impl State {
    transitions!(
        Connecting => [
            on_connected() -> CreateOrJoin,
        ],
        CreateOrJoin => [
            on_joined_room(room_name: &str, players: &[(String, u32, bool)],
                           active_players: usize,
                           board: &HashMap<(i32, i32), Piece>,
                           pieces: &[Piece]) -> Playing,
        ],
    );

    methods!(
        Playing => [
            on_pointer_down(evt: PointerEvent),
            on_pointer_up(evt: PointerEvent),
            on_pointer_move(evt: PointerEvent),
            on_anim(t: f64),
            on_send_chat(),
            on_chat(from: &str, msg: &str),
            on_information(msg: &str),
            on_new_player(name: &str),
            on_player_disconnected(index: usize),
            on_player_turn(active_player: usize),
        ],
        CreateOrJoin => [
            on_room_name_invalid(),
            on_join_inputs_changed(),
            on_join_button(),
            on_unknown_room(room: &str),
        ],
    );
}

unsafe impl Send for State { /* YOLO */}

lazy_static::lazy_static! {
    static ref HANDLE: Arc<Mutex<State>> = Arc::new(Mutex::new(State::Empty));
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
fn set_event_cb<E, F, T>(obj: &E, name: &str, f: F)
    -> Closure<dyn std::ops::FnMut(T)>
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

impl Connecting {
    fn on_connected(self) -> JsResult<CreateOrJoin> {
        // Remove the "Connecting..." message
        self.base.clear_main_div()?;

        // Insta-join a room
        self.base.send(ClientMessage::CreateRoom("Matt".to_string()))?;

        // Return the new state
        CreateOrJoin::new(self.base)
    }
}

impl CreateOrJoin {
    fn new(base: Base) -> JsResult<CreateOrJoin> {
        // When any of the text fields change, check to see whether
        // the "Join" button should be enabled
        let form = base.doc.create_element("form")?;

        let p = base.doc.create_element("p")?;
        let b = base.doc.create_element("b")?;
        b.set_text_content(Some("Name:"));
        p.append_child(&b)?;
        let name_input = base.doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        name_input.set_id("name_input");
        name_input.set_attribute("placeholder", "John Smith")?;
        name_input.set_required(true);
        p.append_child(&name_input)?;
        form.append_child(&p)?;

        let p = base.doc.create_element("p")?;
        let b = base.doc.create_element("b")?;
        b.set_text_content(Some("Room:"));
        p.append_child(&b)?;
        let room_input = base.doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        room_input.set_id("room_input");
        room_input.set_pattern("^[a-z]+ [a-z]+ [a-z]+$");
        p.append_child(&room_input)?;
        let room_invalid_cb = set_event_cb(&room_input, "invalid",
            move |_: Event| {
                HANDLE.lock().unwrap().on_room_name_invalid()
                    .expect("Error in callback");
            });
        form.append_child(&p)?;

        let p = base.doc.create_element("p")?;
        let play_button = base.doc.create_element("button")?
            .dyn_into::<HtmlButtonElement>()?;
        play_button.set_text_content(Some("Create new room"));
        play_button.set_id("play_button");
        play_button.set_type("submit");
        p.append_child(&play_button)?;
        form.append_child(&p)?;

        base.main_div.append_child(&form)?;
        let input_cb = set_event_cb(&room_input, "input", move |_: Event| {
            HANDLE.lock().unwrap().on_join_inputs_changed()
                .expect("Error in callback");
        });
        let submit_cb = set_event_cb(&form, "submit", move |e: Event| {
            e.prevent_default();
            HANDLE.lock().unwrap().on_join_button()
                .expect("Error in callback");
        });

        let err_div = base.doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        err_div.set_id("error");

        let i = base.doc.create_element("i")?;
        i.set_class_name("fas fa-exclamation-triangle");

        let err_span = base.doc.create_element("span")?
            .dyn_into::<HtmlElement>()?;
        err_span.set_id("err_span");

        err_div.append_child(&i)?;
        err_div.append_child(&err_span)?;
        err_div.set_hidden(true);

        base.main_div.append_child(&err_div)?;

        Ok(CreateOrJoin {
            base,
            name_input,
            room_input,
            play_button,
            err_div,
            err_span,

            _input_cb: input_cb,
            _submit_cb: submit_cb,
            _room_invalid_cb: room_invalid_cb,
        })
    }

    fn on_unknown_room(&self, room: &str) -> JsError {
        let err = format!("Could not find room '{}'", room);
        self.err_span.set_text_content(Some(&err));
        self.err_div.set_hidden(false);
        self.play_button.set_disabled(false);
        Ok(())
    }

    fn on_joined_room(self, room_name: &str, players: &[(String, u32, bool)],
                      active_players: usize, board: &HashMap<(i32, i32), Piece>,
                      pieces: &[Piece]) -> JsResult<Playing>
    {
        self.base.clear_main_div()?;
        Playing::new(self.base, room_name, players, active_players, board, pieces)
    }

    fn on_join_button(&self) -> JsError {
        self.play_button.set_disabled(true);
        let name = self.name_input.value();
        let room = self.room_input.value();
        let msg = if room.is_empty() {
            ClientMessage::CreateRoom(name)
        } else {
            ClientMessage::JoinRoom(name, room)
        };
        self.base.send(msg)
    }

    fn on_join_inputs_changed(&self) -> JsError {
        self.play_button.set_text_content(Some(
            if self.room_input.value().is_empty() {
                "Create new room"
            } else {
                "Join existing room"
            }));
        self.room_input.set_custom_validity("");
        Ok(())
    }

    fn on_room_name_invalid(&self) -> JsError {
        self.room_input.set_custom_validity("three lowercase words");
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

impl Playing {
    fn new(base: Base, room_name: &str, players: &[(String, u32, bool)],
           active_player: usize, in_board: &HashMap<(i32, i32), Piece>,
           pieces: &[Piece]) -> JsResult<Playing>
    {
        let player_index = players.len() - 1;

        // The title lists the room name
        let p = base.doc.create_element("p")?;
        let b = base.doc.create_element("b")?;
        b.set_text_content(Some("Room: "));
        let s = base.doc.create_element("span")?;
        s.set_text_content(Some(&room_name));
        p.append_child(&b)?;
        p.append_child(&s)?;
        base.main_div.append_child(&p)?;

        // This div is styled as either 1-3 columns based on screen size
        let game_div = base.doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        game_div.set_id("game");
        base.main_div.append_child(&game_div)?;

        let board = Board::new(&base.doc, &game_div)?;

        let score_col = base.doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        let score_table = base.doc.create_element("table")?
            .dyn_into::<HtmlElement>()?;
        score_table.set_id("scores");
        let tr = base.doc.create_element("tr")?;
        let th = base.doc.create_element("th")?
            .dyn_into::<HtmlTableCellElement>()?;
        th.set_col_span(2);
        th.set_text_content(Some("Player"));
        tr.append_child(&th)?;
        let th = base.doc.create_element("th")?;
        th.set_text_content(Some("Score"));
        tr.append_child(&th)?;
        score_table.append_child(&tr)?;
        score_col.append_child(&score_table)?;
        game_div.append_child(&score_col)?;

        // Create the column for chatting
        let chat_col = base.doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        chat_col.set_id("chat_col");
        let chat_div = base.doc.create_element("div")?
            .dyn_into::<HtmlElement>()?;
        chat_div.set_id("chat");
        chat_col.append_child(&chat_div)?;

        // Name + text input
        let chat_input_div = base.doc.create_element("div")?;
        chat_input_div.set_id("chat_input");

        let chat_name_div = base.doc.create_element("p")?;
        chat_name_div.set_id("chat_name");
        let b = base.doc.create_element("b")?;
        b.set_text_content(Some(&format!("{}:", players[player_index].0)));
        chat_name_div.append_child(&b)?;
        chat_input_div.append_child(&chat_name_div)?;

        let chat_input = base.doc.create_element("input")?
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
                    HANDLE.lock().unwrap().on_send_chat()
                        .expect("Error in callback");
                }
            });

        let mut out = Playing {
            base,
            board,

            chat_input,
            chat_div,
            score_table,
            player_index,
            active_player: active_player,

            _keyup_cb: keyup_cb,
        };

        for ((x, y), p) in in_board.iter() {
            out.board.add_piece(*p, *x, *y)?;
        }
        for p in pieces.into_iter() {
            out.board.add_hand(*p)?;
        }

        for (i, (name, score, connected)) in players.iter().enumerate() {
            out.add_player_row(
                if i == player_index {
                    format!("{} (you)", name)
                } else {
                    name.to_string()
                },
                *score as usize, i == active_player, *connected)?;
        }
        Ok(out)
    }

    fn on_chat(&self, from: &str, msg: &str) -> JsError
    {
        let p = self.base.doc.create_element("p")?;
        p.set_class_name("msg");

        let b = self.base.doc.create_element("b")?;
        b.set_text_content(Some(from));
        p.append_child(&b)?;

        let s =  self.base.doc.create_element("b")?;
        s.set_text_content(Some(":"));
        p.append_child(&s)?;

        let s =  self.base.doc.create_element("span")?;
        s.set_text_content(Some(msg));
        p.append_child(&s)?;

        self.chat_div.append_child(&p)?;
        p.scroll_into_view();
        Ok(())
    }

    fn on_information(&self, msg: &str) -> JsError
    {
        let p = self.base.doc.create_element("p")?;
        p.set_class_name("msg");

        let i = self.base.doc.create_element("i")?;
        i.set_text_content(Some(msg));
        p.append_child(&i)?;
        self.chat_div.append_child(&p)?;
        p.scroll_into_view();
        Ok(())
    }

    fn add_player_row(&self, name: String, score: usize,
                      active: bool, connected: bool)
        -> JsError
    {
        let tr = self.base.doc.create_element("tr")?;
        tr.set_class_name("player-row");
        if active {
            tr.class_list().add_1("active")?;
        }

        let td = self.base.doc.create_element("td")?;
        let i = self.base.doc.create_element("i")?;
        i.set_class_name("fas fa-caret-right");
        td.append_child(&i)?;
        tr.append_child(&td)?;

        let td = self.base.doc.create_element("td")?;
        td.set_text_content(Some(&name));
        tr.append_child(&td)?;

        let td = self.base.doc.create_element("td")?;
        td.set_text_content(Some(&format!("{}", score)));
        tr.append_child(&td)?;

        if !connected {
            tr.class_list().add_1("disconnected")?;
        }

        self.score_table.append_child(&tr)?;

        Ok(())
    }

    fn on_send_chat(&self) -> JsError {
        let i = self.chat_input.value();
        if !i.is_empty() {
            self.chat_input.set_value("");
            self.base.send(ClientMessage::Chat(i))
        } else {
            Ok(())
        }
    }

    fn on_new_player(&self, name: &str) -> JsError {
        // Append a player to the bottom of the scores list
        self.add_player_row(name.to_string(), 0, false, true)?;
        self.on_information(&format!("{} joined the room", name))
    }

    fn on_player_disconnected(&self, index: usize) -> JsError {
        let c = self.score_table.child_nodes()
            .item((index + 1) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?;
        c.class_list().add_1("disconnected")
    }

    fn on_player_turn(&mut self, active_player: usize) -> JsError {
        let children = self.score_table.child_nodes();
        children
            .item((self.active_player + 1) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .remove_1("active")?;

        self.active_player = active_player;
        children
            .item((self.active_player + 1) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .add_1("active")
    }

    fn on_anim(&mut self, t: f64) -> JsError {
        self.board.on_anim(t)
    }

    fn on_pointer_down(&mut self, evt: PointerEvent) -> JsError {
        self.board.on_pointer_down(evt)
    }

    fn on_pointer_move(&mut self, evt: PointerEvent) -> JsError {
        self.board.on_pointer_move(evt)
    }

    fn on_pointer_up(&mut self, evt: PointerEvent) -> JsError {
        self.board.on_pointer_up(evt)
    }
}

////////////////////////////////////////////////////////////////////////////////


fn on_message(msg: ServerMessage) -> JsError {
    use ServerMessage::*;
    console_log!("Got message {:?}", msg);

    let mut state = HANDLE.lock().unwrap();

    match msg {
        UnknownRoom(name) => state.on_unknown_room(&name)?,
        JoinedRoom{room_name, players, active_player, board, pieces} =>
            state.on_joined_room(&room_name, &players, active_player, &board, &pieces)?,
        Chat{from, message} => state.on_chat(&from, &message)?,
        Information(message) => state.on_information(&message)?,
        NewPlayer(name) => state.on_new_player(&name)?,
        PlayerDisconnected(index) => state.on_player_disconnected(index)?,
        PlayerTurn(active_player) => state.on_player_turn(active_player)?,
    }

    Ok(())
}

////////////////////////////////////////////////////////////////////////////////

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> JsError {
    console_error_panic_hook::set_once();

    let doc = web_sys::window()
        .expect("no global `window` exists")
        .document()
        .expect("should have a document on window");
    let body = doc.body().expect("document should have a body");

    // Manufacture the element we're gonna append
    let val = doc.create_element("p")?;
    val.set_text_content(Some("Connecting..."));

    let main_div = doc.create_element("div")?
        .dyn_into::<HtmlElement>()?;
    main_div.set_id("main");
    main_div.append_child(&val)?;
    body.insert_adjacent_element("afterbegin", &main_div)?;

    let hostname = doc.location().unwrap().hostname()?;
    let ws = WebSocket::new(&format!("ws://{}:8080", hostname))?;

    let open_cb = set_event_cb(&ws, "open", move |_: JsValue| {
        HANDLE.lock().unwrap()
            .on_connected()
            .expect("Failed state transition");
    });

    let message_cb = set_event_cb(&ws, "message", move |e: MessageEvent| {
        let msg = serde_json::from_str(&e.data().as_string().unwrap())
            .expect("Failed to decode message");
        on_message(msg)
            .expect("Failed to handle message");
    });

    let close_cb = set_event_cb(&ws, "close", move |_: Event| {
        console_log!("Socket closed");
    });

    let base = Base {
        doc,
        main_div,
        ws,

        _open_cb: open_cb,
        _message_cb: message_cb,
        _close_cb: close_cb,
    };
    *HANDLE.lock().unwrap() = State::Connecting(Connecting { base });

    Ok(())
}
