use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::collections::{HashMap, HashSet};
use std::sync::Mutex;
use web_sys::{
    AddEventListenerOptions,
    Blob,
    Element,
    Event,
    EventTarget,
    FileReader,
    Document,
    KeyboardEvent,
    HtmlButtonElement,
    HtmlElement,
    HtmlInputElement,
    MessageEvent,
    PointerEvent,
    ProgressEvent,
    Request,
    Response,
    SvgGraphicsElement,
    WebSocket,
};

use pont_common::{ClientMessage, ServerMessage, Shape, Color, Piece, Game};

// Minimal logging macro
macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format!($($t)*).into()))
}

type JsResult<T> = Result<T, JsValue>;
type JsError = Result<(), JsValue>;
type JsClosure<T> = Closure<dyn FnMut(T) -> JsError>;

trait DocExt {
    fn create_svg_element(&self, t: &str) -> JsResult<Element>;
}

impl DocExt for Document {
    fn create_svg_element(&self, t: &str) -> JsResult<Element> {
        self.create_element_ns(Some("http://www.w3.org/2000/svg"), t)
    }
}

fn get_time_ms() -> f64 {
    web_sys::window()
        .expect("No global window found")
        .performance()
        .expect("No performance object found")
        .now()
}

////////////////////////////////////////////////////////////////////////////////

macro_rules! methods {
    ($($sub:ident => [$($name:ident($($var:ident: $type:ty),*)),+ $(,)?]),+
       $(,)?) =>
    {
        $($(
        fn $name(&mut self, $($var: $type),* ) -> JsError {
            match self {
                State::$sub(s) => s.$name($($var),*),
                _ => panic!("Invalid state transition"),
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
#[derive(PartialEq)]
struct Dragging {
    target: Element,
    shadow: Element,
    offset: Pos,
    grid_origin: Option<(i32, i32)>,
    hand_index: usize,
}

#[derive(PartialEq)]
struct Panning {
    target: Element,
    pos: Pos,
    pointer_id: i32,
}

#[derive(PartialEq)]
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

#[derive(PartialEq)]
struct DropToGrid {
    anim: TileAnimation,
    shadow: Element,
}

#[derive(PartialEq)]
struct DropManyToGrid(Vec<TileAnimation>);
#[derive(PartialEq)]
struct ReturnToHand(TileAnimation);
#[derive(PartialEq)]
struct ReturnAllToHand(Vec<TileAnimation>);
#[derive(PartialEq)]
struct ConsolidateHand(Vec<TileAnimation>);

#[derive(PartialEq)]
enum DragAnim {
    DropToGrid(DropToGrid),
    DropManyToGrid(DropManyToGrid),
    ReturnToHand(ReturnToHand),
    ReturnAllToHand(ReturnAllToHand),
    ConsolidateHand(ConsolidateHand),
}

#[derive(PartialEq)]
enum BoardState {
    Idle,
    Dragging(Dragging),
    Panning(Panning),
    Animation(DragAnim),
}

enum DropTarget {
    DropToGrid(i32, i32),
    ReturnToGrid(i32, i32),
    Exchange,
    ReturnToHand,
}

enum Move {
    Place(Vec<(Piece, i32, i32)>),
    Swap(Vec<Piece>),
}

pub struct Board {
    doc: Document,
    svg: SvgGraphicsElement,
    svg_div: Element,

    state: BoardState,

    pan_group: Element,
    pan_offset: Pos,

    grid: HashMap<(i32, i32), Piece>,
    tentative: HashMap<(i32, i32), usize>,
    exchange_list: Vec<usize>,
    pieces_remaining: usize,
    hand: Vec<(Piece, Element)>,

    accept_button: HtmlButtonElement,
    reject_button: HtmlButtonElement,
    exchange_div: Element,

    pointer_down_cb: JsClosure<PointerEvent>,
    pointer_move_cb: JsClosure<PointerEvent>,
    pointer_up_cb: JsClosure<PointerEvent>,
    touch_start_cb: JsClosure<Event>,

    pan_move_cb: JsClosure<PointerEvent>,
    pan_end_cb: JsClosure<Event>,

    anim_cb: JsClosure<f64>,
}

impl Board {
    fn new(doc: &Document)
        -> JsResult<Board>
    {
        let pan_rect = doc.get_element_by_id("pan_rect")
            .expect("Could not find pan rect");
        set_event_cb(&pan_rect, "pointerdown", move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pan_start(evt)
        }).forget();

        let accept_button = doc.get_element_by_id("accept_button")
            .expect("Could not find accept_button")
            .dyn_into()?;
        set_event_cb(&accept_button, "click", move |evt: Event| {
            HANDLE.lock().unwrap()
                .on_accept_button(evt)
        }).forget();

        let reject_button = doc.get_element_by_id("reject_button")
            .expect("Could not find reject_button")
            .dyn_into()?;
        set_event_cb(&reject_button, "click", move |evt: Event| {
            HANDLE.lock().unwrap()
                .on_reject_button(evt)
        }).forget();

        let pointer_down_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_down(evt)
        });
        let pointer_move_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_move(evt)
        });
        let pointer_up_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_up(evt)
        });
        let anim_cb = build_cb(move |evt: f64| {
            HANDLE.lock().unwrap()
                .on_anim(evt)
        });
        let pan_move_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pan_move(evt)
        });
        let pan_end_cb = build_cb(move |evt: Event| {
            HANDLE.lock().unwrap()
                .on_pan_end(evt)
        });
        let touch_start_cb = build_cb(move |evt: Event| {
            evt.prevent_default();
            Ok(())
        });

        let svg = doc.get_element_by_id("game_svg")
            .expect("Could not find game svg")
            .dyn_into()?;
        let svg_div = doc.get_element_by_id("svg_div")
            .expect("Could not find svg div");
        let pan_group = doc.get_element_by_id("pan_group")
            .expect("Could not find pan_group");
        let exchange_div = doc.get_element_by_id("exchange_div")
            .expect("Could not find exchange_div");

        let out = Board {
            doc: doc.clone(),
            state: BoardState::Idle,
            svg, svg_div,
            pan_group,
            pan_offset: (0.0, 0.0),
            grid: HashMap::new(),
            tentative: HashMap::new(),
            exchange_list: Vec::new(),
            hand: Vec::new(),
            pointer_down_cb,
            pointer_up_cb,
            pointer_move_cb,
            touch_start_cb,
            pan_move_cb,
            pan_end_cb,
            anim_cb,
            accept_button,
            reject_button,
            exchange_div,
            pieces_remaining: 0,
        };

        Ok(out)
    }

    fn set_my_turn(&mut self, is_my_turn: bool) -> JsError {
        if is_my_turn {
            self.svg_div.class_list().remove_1("nyt")?;
        } else {
            self.svg_div.class_list().add_1("nyt")?;
        }
        self.update_exchange_div(is_my_turn)
    }

    fn get_transform(e: &Element) -> Pos {
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

    fn mouse_pos(&self, evt: &PointerEvent) -> Pos {
        let mat = self.svg.get_screen_ctm().unwrap();
        let x = (evt.client_x() as f32 - mat.e()) / mat.a();
        let y = (evt.client_y() as f32 - mat.f()) / mat.d();
        (x, y)
    }

    fn on_pan_start(&mut self, evt: PointerEvent) -> JsError {
        if self.state != BoardState::Idle {
            return Ok(());
        }
        // No panning before placing the first piece, to prevent griefing by
        // placing the piece far from the visible region.
        if self.grid.is_empty() {
            return Ok(());
        }

        evt.prevent_default();
        let target = evt.target()
            .unwrap()
            .dyn_into::<Element>()?;
        target.set_pointer_capture(evt.pointer_id())?;

        let mut options = AddEventListenerOptions::new();
        options.passive(false);
        let pointer_id = evt.pointer_id();
        target.set_pointer_capture(pointer_id)?;
        target.add_event_listener_with_callback_and_add_event_listener_options(
            "pointermove",
            self.pan_move_cb.as_ref().unchecked_ref(), &options)?;
        target.add_event_listener_with_callback_and_add_event_listener_options(
            "pointerup",
            self.pan_end_cb.as_ref().unchecked_ref(), &options)?;
        self.doc.body()
            .expect("Could not get boby")
            .add_event_listener_with_callback_and_add_event_listener_options(
                "pointermove",
                self.pan_move_cb.as_ref().unchecked_ref(), &options)?;

        let p = self.mouse_pos(&evt);
        self.state = BoardState::Panning(Panning {
            target,
            pointer_id,
            pos: (p.0 - self.pan_offset.0, p.1 - self.pan_offset.1)
        });

        Ok(())
    }

    fn on_pan_move(&mut self, evt: PointerEvent) -> JsError {
        if let BoardState::Panning(d) = &self.state {
            evt.prevent_default();

            let p = self.mouse_pos(&evt);
            self.pan_offset = (p.0 - d.pos.0, p.1 - d.pos.1);
            self.pan_group.set_attribute("transform",
                                   &format!("translate({} {})",
                                   self.pan_offset.0,
                                   self.pan_offset.1))
        } else {
            Err(JsValue::from_str("Invalid state (pan move)"))
        }
    }

    fn on_pan_end(&mut self, evt: Event) -> JsError {
        evt.prevent_default();

        if let BoardState::Panning(d) = &self.state {
            d.target.release_pointer_capture(d.pointer_id)?;
            d.target.remove_event_listener_with_callback("pointermove",
                    self.pan_move_cb.as_ref().unchecked_ref())?;
            d.target.remove_event_listener_with_callback("pointerup",
                    self.pan_end_cb.as_ref().unchecked_ref())?;
            self.doc.body()
                .expect("Could not get boby")
                .remove_event_listener_with_callback(
                    "pointermove",
                    self.pan_move_cb.as_ref().unchecked_ref())?;
            self.state = BoardState::Idle;
            Ok(())
        } else {
            Err(JsValue::from_str("Invalid state (pan end)"))
        }
    }

    fn on_pointer_down(&mut self, evt: PointerEvent) -> JsError {
        // We only drag if nothing else is dragging;
        // no fancy multi-touch dragging here.
        if self.state != BoardState::Idle {
            return Ok(());
        }
        evt.prevent_default();

        let mut target = evt.target()
            .unwrap()
            .dyn_into::<Element>()?;

        // Shadow goes underneath the dragged piece
        let shadow = self.doc.create_svg_element("rect")?;
        shadow.class_list().add_1("shadow")?;
        shadow.set_attribute("width", "9.5")?;
        shadow.set_attribute("height", "9.5")?;
        shadow.set_attribute("x", "0.25")?;
        shadow.set_attribute("y", "0.25")?;
        shadow.set_attribute("visibility", "hidden")?;
        self.pan_group.append_child(&shadow)?;

        // Walk up the tree to find the piece's <g> group,
        // which sets its position with a translation
        while !target.has_attribute("transform") {
            target = target.parent_node().unwrap().dyn_into::<Element>()?;
        }
        let (mx, my) = self.mouse_pos(&evt);
        let (mut tx, mut ty) = Self::get_transform(&target);

        let (hand_index, grid_origin) = if my > 185.0 {
            // Picking from hand
            let i = (tx.round() as i32 - 5) / 15;
            self.svg.remove_child(&target)?;
            (i as usize, None)
        } else {
            // Picking from tentative grid
            let x = tx.round() as i32 / 10;
            let y = ty.round() as i32 / 10;
            self.pan_group.remove_child(&target)?;
            tx += self.pan_offset.0;
            ty += self.pan_offset.1;
            target.set_attribute("transform",
                                   &format!("translate({} {})", tx, ty))?;
            let hand_index = self.tentative.remove(&(x, y)).unwrap();
            if self.tentative.is_empty() {
                self.accept_button.set_disabled(true);
                self.reject_button.set_disabled(true);
            }
            self.update_exchange_div(true)?;
            self.mark_invalid()?;
            (hand_index, Some((x, y)))
        };
        target.class_list().remove_1("invalid")?;

        // Move to the back of the SVG object, so it's on top
        self.svg.append_child(&target)?;

        let mut options = AddEventListenerOptions::new();
        options.passive(false);
        target.set_pointer_capture(evt.pointer_id())?;
        target.add_event_listener_with_callback_and_add_event_listener_options(
            "pointermove",
            self.pointer_move_cb.as_ref().unchecked_ref(), &options)?;
        target.add_event_listener_with_callback_and_add_event_listener_options(
            "pointerup",
            self.pointer_up_cb.as_ref().unchecked_ref(), &options)?;
        self.doc.body()
            .expect("Could not get boby")
            .add_event_listener_with_callback_and_add_event_listener_options(
                "pointermove",
                self.pointer_move_cb.as_ref().unchecked_ref(), &options)?;

        self.state = BoardState::Dragging(Dragging {
            target,
            shadow,
            offset: (mx - tx, my - ty),
            hand_index,
            grid_origin,
        });
        Ok(())
    }

    fn drop_target(&self, evt: &PointerEvent) -> JsResult<(Pos, DropTarget)> {
        if let BoardState::Dragging(d) = &self.state {
            // Get the position of the tile being dragged
            // in SVG frame coordinates (0-200)
            let (mut x, mut y) = self.mouse_pos(&evt);
            x -= d.offset.0;
            y -= d.offset.1;

            // Clamp to the window's bounds
            for c in [&mut x, &mut y].iter_mut() {
                if **c < 0.0 {
                    **c = 0.0;
                } else if **c > 190.0 {
                    **c = 190.0;
                }
            }

            // If we've started exchanging tiles, then prevent folks from
            // dragging onto the grid.
            if !self.exchange_list.is_empty() && y < 175.0 {
                y = 175.0;
            }
            let pos = (x, y);

            // If the tile is off the bottom of the grid, then we propose
            // to return it to the hand.
            if y >= 165.0 {
                if self.tentative.is_empty() && x >= 95.0 && x <= 140.0 &&
                   self.exchange_list.len() < self.pieces_remaining
                {
                    return Ok((pos, DropTarget::Exchange));
                } else {
                    return Ok((pos, DropTarget::ReturnToHand));
                }
            }

            // Otherwise, we shift the tile's coordinates by the panning
            // of the main grid, then check whether we can place it
            x -= self.pan_offset.0;
            y -= self.pan_offset.1;

            let tx = (x / 10.0).round() as i32;
            let ty = (y / 10.0).round() as i32;

            let offboard = {
                let x = tx as f32 * 10.0 + self.pan_offset.0;
                let y = ty as f32 * 10.0 + self.pan_offset.1;
                x < 0.0 || y < 0.0 || y > 165.0 || x >= 190.0
            };

            let overlapping = self.grid.contains_key(&(tx, ty)) ||
                              self.tentative.contains_key(&(tx, ty));
            if !overlapping && !offboard {
                return Ok((pos, DropTarget::DropToGrid(tx, ty)));
            }

            // Otherwise, return to either the hand or the grid
            Ok((pos, match d.grid_origin {
                None => DropTarget::ReturnToHand,
                Some((gx, gy)) => DropTarget::ReturnToGrid(gx, gy),
            }))
        } else {
            Err(JsValue::from_str("Invalid state (drop target)"))
        }
    }

    fn on_pointer_move(&self, evt: PointerEvent) -> JsError {
        if let BoardState::Dragging(d) = &self.state {
            evt.prevent_default();

            let (pos, drop_target) = self.drop_target(&evt)?;
            d.target.set_attribute("transform",
                                   &format!("translate({} {})", pos.0, pos.1))?;
            match drop_target {
                DropTarget::DropToGrid(gx, gy) => {
                    d.shadow.set_attribute(
                        "transform", &format!("translate({} {})",
                             gx as f32 * 10.0, gy as f32 * 10.0))?;
                    d.shadow.set_attribute("visibility", "visible")
                },
                _ => d.shadow.set_attribute("visibility", "hidden")
            }
        } else {
            Err(JsValue::from_str("Invalid state (pointer move)"))
        }
    }

    fn mark_invalid(&self) -> JsResult<bool> {
        let mut b = self.grid.clone();
        for (pos, index) in self.tentative.iter() {
            b.insert(*pos, self.hand[*index].0);
        }
        let mut invalid = Game::invalid(&b);
        if !Game::is_linear(&self.tentative.keys().cloned().collect()) {
            for pos in self.tentative.keys() {
                invalid.insert(*pos);
            }
        }
        for (pos, index) in self.tentative.iter() {
            if invalid.contains(pos) {
                self.hand[*index].1.class_list().add_1("invalid")?;
            } else {
                self.hand[*index].1.class_list().remove_1("invalid")?;
            }
        }
        Ok(invalid.is_empty())
    }

    fn on_pointer_up(&mut self, evt: PointerEvent) -> JsError {
        if let BoardState::Dragging(d) = &self.state {
            evt.prevent_default();

            d.target.release_pointer_capture(evt.pointer_id())?;
            d.target.remove_event_listener_with_callback("pointermove",
                    self.pointer_move_cb.as_ref().unchecked_ref())?;
            d.target.remove_event_listener_with_callback("pointerup",
                    self.pointer_up_cb.as_ref().unchecked_ref())?;
            self.doc.body()
                .expect("Could not get boby")
                .remove_event_listener_with_callback(
                    "pointermove",
                    self.pointer_move_cb.as_ref().unchecked_ref())?;

            let (pos, drop_target) = self.drop_target(&evt)?;
            let drag_anim = match drop_target {
                DropTarget::ReturnToHand => {
                    self.pan_group.remove_child(&d.shadow)?;
                    Some(DragAnim::ReturnToHand(ReturnToHand(
                        TileAnimation {
                            target: d.target.clone(),
                            start: pos,
                            end: ((d.hand_index * 15 + 5) as f32, 185.0),
                            t0: evt.time_stamp()
                        })))
                },
                DropTarget::DropToGrid(gx, gy) |
                DropTarget::ReturnToGrid(gx, gy) => {
                    self.tentative.insert((gx, gy), d.hand_index);
                    let target = d.target.clone();
                    self.svg.remove_child(&target)?;
                    self.pan_group.append_child(&target)?;
                    Some(DragAnim::DropToGrid(DropToGrid{
                        anim: TileAnimation {
                            target,
                            start: (pos.0 - self.pan_offset.0, pos.1 - self.pan_offset.1),
                            end: (gx as f32 * 10.0, gy as f32 * 10.0),
                            t0: evt.time_stamp(),
                        },
                        shadow: d.shadow.clone(),
                    }))
                },
                DropTarget::Exchange => {
                    self.exchange_list.push(d.hand_index);
                    d.target.set_attribute("visibility", "hidden")?;
                    self.update_exchange_div(true)?;
                    self.accept_button.set_disabled(false);
                    self.reject_button.set_disabled(false);

                    // No animation here, because we wait for the server to
                    // send back a MoveAccepted message then consolidate hand
                    None
                },
            };
            self.mark_invalid()?;
            self.update_exchange_div(true)?;
            if let Some(drag) = drag_anim {
                self.state = BoardState::Animation(drag);
                self.request_animation_frame()?;
            } else {
                self.state = BoardState::Idle;
            }
        }
        Ok(())
    }

    fn on_anim(&mut self, t: f64) -> JsError {
        if let BoardState::Animation(drag) = &mut self.state {
            match drag {
                DragAnim::DropToGrid(d) => {
                    if d.anim.run(t)? {
                        self.request_animation_frame()?;
                    } else {
                        self.pan_group.remove_child(&d.shadow)?;
                        self.state = BoardState::Idle;
                        self.accept_button.set_disabled(!self.mark_invalid()?);
                        self.reject_button.set_disabled(false);
                    }
                },
                DragAnim::ReturnToHand(d) => {
                    if d.0.run(t)? {
                        self.request_animation_frame()?;
                    } else {
                        self.state = BoardState::Idle;
                        if !self.tentative.is_empty() {
                            self.accept_button.set_disabled(!self.mark_invalid()?);
                        } else if self.exchange_list.is_empty() {
                            self.accept_button.set_disabled(true);
                            self.reject_button.set_disabled(true);
                        }
                    }
                },
                DragAnim::ReturnAllToHand(d) => {
                    let mut any_running = false;
                    for a in d.0.iter() {
                        any_running |= a.run(t)?;
                    }
                    if any_running {
                        self.request_animation_frame()?;
                    } else {
                        self.state = BoardState::Idle;
                        self.accept_button.set_disabled(true);
                        self.reject_button.set_disabled(true);
                        self.update_exchange_div(true)?;
                    }
                },
                DragAnim::ConsolidateHand(ConsolidateHand(d)) |
                DragAnim::DropManyToGrid(DropManyToGrid(d)) => {
                    let mut any_running = false;
                    for a in d.iter() {
                        any_running |= a.run(t)?;
                    }
                    if any_running {
                        self.request_animation_frame()?;
                    } else {
                        self.state = BoardState::Idle;
                    }
                },
            }
        }
        Ok(())
    }

    fn request_animation_frame(&self) -> JsResult<i32> {
        web_sys::window()
            .expect("no global `window` exists")
            .request_animation_frame(self.anim_cb.as_ref()
                                     .unchecked_ref())
    }

    fn add_hand(&mut self, p: Piece) -> JsResult<Element> {
        let g = self.new_piece(p)?;
        self.svg.append_child(&g)?;
        g.class_list().add_1("piece")?;
        g.set_attribute("transform", &format!("translate({} 185)",
                                              5 + 15 * self.hand.len()))?;

        let mut options = AddEventListenerOptions::new();
        options.passive(false);
        g.add_event_listener_with_callback_and_add_event_listener_options(
            "pointerdown",
            self.pointer_down_cb.as_ref().unchecked_ref(),
            &options)?;
        g.add_event_listener_with_callback_and_add_event_listener_options(
            "touchstart",
            self.touch_start_cb.as_ref().unchecked_ref(),
            &options)?;

        self.hand.push((p, g.clone()));

        Ok(g)
    }

    fn new_piece(&self, p: Piece) -> JsResult<Element> {
        let g = self.doc.create_svg_element("g")?;
        let r = self.doc.create_svg_element("rect")?;
        r.class_list().add_1("tile")?;
        r.set_attribute("width", "9.5")?;
        r.set_attribute("height", "9.5")?;
        r.set_attribute("x", "0.25")?;
        r.set_attribute("y", "0.25")?;
        let s = match p.0 {
            Shape::Circle => {
                let s = self.doc.create_svg_element("circle")?;
                s.set_attribute("r", "3.0")?;
                s.set_attribute("cx", "5.0")?;
                s.set_attribute("cy", "5.0")?;
                s
            },
            Shape::Square => {
                let s = self.doc.create_svg_element("rect")?;
                s.set_attribute("width", "6.0")?;
                s.set_attribute("height", "6.0")?;
                s.set_attribute("x", "2.0")?;
                s.set_attribute("y", "2.0")?;
                s
            }
            Shape::Clover => {
                let s = self.doc.create_svg_element("g")?;
                for (x, y) in &[(5.0, 3.0), (5.0, 7.0), (3.0, 5.0), (7.0, 5.0)]
                {
                    let c = self.doc.create_svg_element("circle")?;
                    c.set_attribute("r", "1.5")?;
                    c.set_attribute("cx", &x.to_string())?;
                    c.set_attribute("cy", &y.to_string())?;
                    s.append_child(&c)?;
                }
                let r = self.doc.create_svg_element("rect")?;
                r.set_attribute("width", "4.0")?;
                r.set_attribute("height", "3.0")?;
                r.set_attribute("x", "3.0")?;
                r.set_attribute("y", "3.5")?;
                s.append_child(&r)?;

                let r = self.doc.create_svg_element("rect")?;
                r.set_attribute("width", "3.0")?;
                r.set_attribute("height", "4.0")?;
                r.set_attribute("x", "3.5")?;
                r.set_attribute("y", "3.0")?;
                s.append_child(&r)?;

                s
            }
            Shape::Diamond => {
                let s = self.doc.create_svg_element("polygon")?;
                s.set_attribute("points", "2,5 5,8 8,5 5,2")?;
                s
            }
            Shape::Cross => {
                let s = self.doc.create_svg_element("polygon")?;
                s.set_attribute("points", "2,2 3.5,5 2,8 5,6.5 8,8 6.5,5 8,2 5,3.5")?;
                s
            }
            Shape::Star => {
                let g = self.doc.create_svg_element("g")?;
                let s = self.doc.create_svg_element("polygon")?;
                s.set_attribute("points", "3,3 4,5 3,7 5,6 7,7 6,5 7,3 5,4")?;
                g.append_child(&s)?;
                let s = self.doc.create_svg_element("polygon")?;
                s.set_attribute("points", "1,5 4,6 5,9 6,6 9,5 6,4 5,1 4,4")?;
                g.append_child(&s)?;
                g
            }
        };
        s.class_list().add_1("color")?;

        g.append_child(&r)?;
        g.append_child(&s)?;
        g.class_list().add_1(match p.1 {
            Color::Orange => "shape-orange",
            Color::Yellow => "shape-yellow",
            Color::Green => "shape-green",
            Color::Red => "shape-red",
            Color::Blue => "shape-blue",
            Color::Purple => "shape-purple",
        })?;

        // Add carets on the corners based on color, to be accessible
        let mut pts = Vec::new();
        if p.1 == Color::Orange || p.1 == Color::Yellow {
            pts.push("0.5,0.5 3,0.5 0.5,3");
        }
        if p.1 == Color::Orange || p.1 == Color::Green {
            pts.push("9.5,9.5 7,9.5 9.5,7");
        }
        if p.1 == Color::Red || p.1 == Color::Blue {
            pts.push("0.5,9.5 3,9.5 0.5,7");
        }
        if p.1 == Color::Red || p.1 == Color::Purple {
            pts.push("9.5,0.5 7,0.5 9.5,3");
        }

        for poly in pts.into_iter() {
            let corner = self.doc.create_svg_element("polygon")?;
            corner.set_attribute("points", poly)?;
            corner.class_list().add_1("corner")?;
            corner.class_list().add_1("color")?;
            g.append_child(&corner)?;
        }


        Ok(g)
    }

    fn add_piece(&mut self, p: Piece, x: i32, y: i32) -> JsResult<Element> {
        self.grid.insert((x, y), p);

        let g = self.new_piece(p)?;
        self.pan_group.append_child(&g)?;
        g.class_list().add_1("placed")?;
        g.set_attribute("transform",
                        &format!("translate({} {})", x * 10, y * 10))?;

        Ok(g)
    }

    fn on_reject_button(&mut self, evt: Event) -> JsError {
        // Don't allow for any tricky business here
        if self.state != BoardState::Idle {
            return Ok(());
        }

        let drag = if !self.tentative.is_empty() {
            let mut tiles = HashMap::new();
            std::mem::swap(&mut self.tentative, &mut tiles);
            // Take every active tile and free them from the tile grid,
            // adjusting their transform so they don't move at all
            for i in tiles.values() {
                let t = &self.hand[*i].1;
                self.pan_group.remove_child(t)?;
                t.class_list().remove_1("invalid")?;
                let (dx, dy) = Self::get_transform(t);
                t.set_attribute(
                    "transform", &format!("translate({} {})",
                            dx - self.pan_offset.0,
                            dy - self.pan_offset.1))?;
                self.svg.append_child(t)?;
            }
            Some(DragAnim::ReturnAllToHand(ReturnAllToHand(
                tiles.drain().map(|((tx, ty), i)|
                    TileAnimation {
                        target: self.hand[i].1.clone(),
                        start: (tx as f32 * 10.0 + self.pan_offset.0,
                                ty as f32 * 10.0 + self.pan_offset.1),
                        end: ((i * 15 + 5) as f32, 185.0),
                        t0: evt.time_stamp()
                    }).collect())))
        } else if !self.exchange_list.is_empty() {
            let mut ex = Vec::new();
            std::mem::swap(&mut ex, &mut self.exchange_list);
            self.update_exchange_div(true)?;
            Some(DragAnim::ReturnAllToHand(ReturnAllToHand(
                ex.drain(0..)
                    .map(|i| {
                        let target = self.hand[i].1.clone();
                        target.set_attribute("visibility", "visible")?;
                        let x = (i * 15 + 5) as f32;
                        Ok(TileAnimation {
                            target,
                            start: (x, 200.0),
                            end:   (x, 185.0),
                            t0: evt.time_stamp()
                        })
                    })
                    .collect::<JsResult<Vec<TileAnimation>>>()?)))
        } else {
            None
        };

        if let Some(drag) = drag {
            self.state = BoardState::Animation(drag);
            self.request_animation_frame()?;
        }
        Ok(())
    }

    /*  Attempts to make the given move.
     *  If the move is valid, returns the indexes of placed pieces
     *  (as hand indexes), which can be passed up to the server. */
    fn make_move(&mut self, _evt: Event) -> JsResult<Move> {
        if self.state != BoardState::Idle {
            return Ok(Move::Place(Vec::new()));
        }

        // Disable everything until we hear back from the server.
        //
        // If this is a one-player game, then it will be our turn again,
        // but we'll let the server tell us that.
        self.accept_button.set_disabled(true);
        self.reject_button.set_disabled(true);

        self.set_my_turn(false)?;

        if !self.tentative.is_empty() {
            Ok(Move::Place(self.tentative.iter()
                .map(|((x, y), i)| (self.hand[*i].0, *x, *y))
                .collect()))
        } else {
            assert!(!self.exchange_list.is_empty());
            Ok(Move::Swap(self.exchange_list.iter()
                .map(|i| self.hand[*i].0)
                .collect()))
        }
    }

    fn on_move_accepted(&mut self, dealt: &[Piece]) -> JsError {
        let mut placed = HashMap::new();
        for ((x, y), i) in self.tentative.drain() {
            placed.insert(i, (x, y));
        }
        let mut exchanged = HashSet::new();
        for i in self.exchange_list.drain(0..) {
            exchanged.insert(i);
        }

        // We're going to shuffle pieces around now!
        let mut prev_hand = Vec::new();
        std::mem::swap(&mut prev_hand, &mut self.hand);
        let mut anims = Vec::new();
        let t0 = get_time_ms();
        for (i, (piece, element)) in prev_hand.into_iter().enumerate() {
            if let Some((x, y)) = placed.remove(&i) {
                element.class_list().remove_1("piece")?;
                element.class_list().add_1("placed")?;
                element.remove_event_listener_with_callback(
                    "pointerdown",
                    self.pointer_down_cb.as_ref().unchecked_ref())?;
                self.grid.insert((x, y), piece);
            } else if exchanged.contains(&i) {
                self.svg.remove_child(&element)?;
            } else {
                if self.hand.len() != i {
                    anims.push(TileAnimation {
                        target: element.clone(),
                        start: (i as f32 * 15.0 + 5.0, 185.0),
                        end: (self.hand.len() as f32 * 15.0 + 5.0, 185.0),
                        t0 });
                }
                self.hand.push((piece, element));
            }
        }
        for d in dealt {
            let x = self.hand.len() as f32 * 15.0 + 5.0;
            let target = self.add_hand(*d)?;
            anims.push(TileAnimation {
                target,
                start: (x, 220.0),
                end: (x, 185.0),
                t0
            })
        }
        self.state = BoardState::Animation(
            DragAnim::ConsolidateHand(ConsolidateHand(anims)));
        self.request_animation_frame()?;

        Ok(())
    }

    fn update_exchange_div(&mut self, my_turn: bool) -> JsError {
        // Special case: if a new user joins while we've got pieces staged
        // to swap, then it's possible that we won't have enough to swap,
        // so we cancel the swap.
        if self.pieces_remaining < self.exchange_list.len() {
            // This will call update_exchange_div again, so we don't
            // need to run any of the code below.
            return self.on_reject_button(Event::new("dummy")?);
        }

        // If there are no pieces remaining, then the box is always disabled
        if self.pieces_remaining == 0 {
            self.exchange_div.set_inner_html("<p>No pieces<br>left in bag</p>");
            self.exchange_div.class_list().add_1("disabled")?;
            return Ok(());
        }

        // If it's not your turn, then we disable the box but leave the normal
        // text (because we know that there are pieces remaining).
        if !my_turn {
            self.exchange_div.set_inner_html("<p>Drag here<br>to swap</p>");
            self.exchange_div.class_list().add_1("disabled")?;
            return Ok(());
        }

        // If it's our turn and there are pieces staged in the grid, then we
        // disable the swapping box.
        if !self.tentative.is_empty() {
            self.exchange_div.set_inner_html("<p>Drag here<br>to swap</p>");
            self.exchange_div.class_list().add_1("disabled")?;
            return Ok(());
        }

        // Otherwise, the box is enabled and we have appropriate text
        self.exchange_div.class_list().remove_1("disabled")?;
        let n = self.exchange_list.len();
        if n == 0 {
            self.exchange_div.set_inner_html("<p>Drag here<br>to swap</p>");
        } else {
            self.exchange_div.set_inner_html(
                &format!("<p>Swap {} piece{}{}</p>",
                         n, if n > 1 { "s" } else { " " },
                         if n == self.pieces_remaining { " (max)" }
                         else { "" }));
        }
        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////

pub struct Base {
    doc: Document,
    ws: WebSocket,
}

impl Base {
    fn send(&self, msg: ClientMessage) -> JsError {
        let encoded = bincode::serialize(&msg)
            .map_err(|e| JsValue::from_str(
                    &format!("Could not encode: {}", e)))?;
        self.ws.send_with_u8_array(&encoded[..])
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
    colorblind_checkbox: HtmlInputElement,
    err_div: HtmlElement,
    err_span: HtmlElement,

    // Callbacks are owned so that it lives as long as the state
    _room_invalid_cb: JsClosure<Event>,
    _input_cb: JsClosure<Event>,
    _submit_cb: JsClosure<Event>,
}

struct Playing {
    base: Base,

    chat_div: HtmlElement,
    chat_input: HtmlInputElement,
    score_table: HtmlElement,
    player_index: usize,
    active_player: usize,
    player_names: Vec<String>,

    board: Board,

    // Callback is owned so that it lives as long as the state
    _keyup_cb: JsClosure<KeyboardEvent>,
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
                           active_player: usize,
                           player_index: usize,
                           board: &[((i32, i32), Piece)],
                           pieces: &[Piece]) -> Playing,
        ],
    );

    methods!(
        Playing => [
            on_pointer_down(evt: PointerEvent),
            on_pointer_up(evt: PointerEvent),
            on_pointer_move(evt: PointerEvent),
            on_pan_start(evt: PointerEvent),
            on_pan_move(evt: PointerEvent),
            on_pan_end(evt: Event),
            on_accept_button(evt: Event),
            on_reject_button(evt: Event),
            on_anim(t: f64),
            on_send_chat(),
            on_chat(from: &str, msg: &str),
            on_information(msg: &str),
            on_new_player(name: &str),
            on_player_disconnected(index: usize),
            on_player_reconnected(index: usize),
            on_player_turn(active_player: usize),
            on_played(pieces: &[(Piece, i32, i32)]),
            on_swapped(count: usize),
            on_move_accepted(dealt: &[Piece]),
            on_move_rejected(),
            on_pieces_remaining(remaining: usize),
            on_player_score(delta: u32, total: u32),
            on_finished(winner: usize),
        ],
        CreateOrJoin => [
            on_room_name_invalid(),
            on_join_inputs_changed(),
            on_join_button(),
            on_join_failed(room: &str),
        ],
    );
}

unsafe impl Send for State { /* YOLO */}

lazy_static::lazy_static! {
    static ref HANDLE: Mutex<State> = Mutex::new(State::Empty);
}
////////////////////////////////////////////////////////////////////////////////

// Boilerplate to wrap and bind a callback.
// The resulting callback must be stored for as long as it may be used.
#[must_use]
fn build_cb<F, T>(f: F) -> JsClosure<T>
    where F: FnMut(T) -> JsError + 'static,
          T: FromWasmAbi + 'static
{
    Closure::wrap(Box::new(f) as Box<dyn FnMut(T) -> JsError>)
}

#[must_use]
fn set_event_cb<E, F, T>(obj: &E, name: &str, f: F) -> JsClosure<T>
    where E: JsCast + Clone + std::fmt::Debug,
          F: FnMut(T) -> JsError + 'static,
          T: FromWasmAbi + 'static
{
    let cb = build_cb(f);
    let target = obj.dyn_ref::<EventTarget>()
        .expect("Could not convert into `EventTarget`");
    target.add_event_listener_with_callback(name, cb.as_ref().unchecked_ref())
        .expect("Could not add event listener");
    cb
}

////////////////////////////////////////////////////////////////////////////////

impl Connecting {
    fn on_connected(self) -> JsResult<CreateOrJoin> {
        self.base.doc.get_element_by_id("disconnected_msg")
            .expect("Could not get disconnected_msg div")
            .dyn_into::<HtmlElement>()?
            .set_text_content(Some("Lost connection to game server"));
        CreateOrJoin::new(self.base)
    }
}

impl CreateOrJoin {
    fn new(base: Base) -> JsResult<CreateOrJoin> {
        let name_input = base.doc.get_element_by_id("name_input")
            .expect("Could not find name_input")
            .dyn_into::<HtmlInputElement>()?;
        let room_input = base.doc.get_element_by_id("room_input")
            .expect("Could not find room_input")
            .dyn_into::<HtmlInputElement>()?;
        let room_invalid_cb = set_event_cb(&room_input, "invalid",
            move |_: Event| {
                HANDLE.lock().unwrap().on_room_name_invalid()
            });
        let input_cb = set_event_cb(&room_input, "input", move |_: Event| {
            HANDLE.lock().unwrap().on_join_inputs_changed()
        });

        let form = base.doc.get_element_by_id("join_form")
            .expect("Could not find join_form");
        let submit_cb = set_event_cb(&form, "submit", move |e: Event| {
            e.prevent_default();
            HANDLE.lock().unwrap().on_join_button()
        });

        let err_div = base.doc.get_element_by_id("err_div")
            .expect("Could not find err_div")
            .dyn_into()?;
        let err_span = base.doc.get_element_by_id("err_span")
            .expect("Could not find err_span")
            .dyn_into()?;

        let play_button = base.doc.get_element_by_id("play_button")
            .expect("Could not find play_button")
            .dyn_into::<HtmlButtonElement>()?;

        play_button.set_text_content(Some(
            if room_input.value().is_empty() {
                "Create new room"
            } else {
                "Join existing room"
            }));
        play_button.class_list().remove_1("disabled")?;

        let colorblind_checkbox = base.doc.get_element_by_id("colorblind")
            .expect("Could not find colorblind checkbox")
            .dyn_into()?;

        Ok(CreateOrJoin {
            base,
            name_input,
            room_input,
            play_button,
            colorblind_checkbox,
            err_div,
            err_span,

            _input_cb: input_cb,
            _submit_cb: submit_cb,
            _room_invalid_cb: room_invalid_cb,
        })
    }

    fn on_join_failed(&self, err: &str) -> JsError {
        self.err_span.set_text_content(Some(err));
        self.err_div.set_hidden(false);
        self.play_button.set_disabled(false);
        Ok(())
    }

    fn on_joined_room(self, room_name: &str, players: &[(String, u32, bool)],
                      active_player: usize, player_index: usize,
                      board: &[((i32, i32), Piece)],
                      pieces: &[Piece]) -> JsResult<Playing>
    {
        self.base.doc.get_element_by_id("join")
            .expect("Could not get join div")
            .dyn_into::<HtmlElement>()?
            .set_hidden(true);
        self.base.doc.get_element_by_id("playing")
            .expect("Could not get playing div")
            .dyn_into::<HtmlElement>()?
            .set_hidden(false);

        let mut p = Playing::new(self.base, room_name, players,
                                 active_player, player_index,
                                 board, pieces)?;
        p.on_information(&format!("Welcome, {}!", players[player_index].0))?;
        p.on_player_turn(active_player)?;
        Ok(p)
    }

    fn on_join_button(&self) -> JsError {
        self.play_button.set_disabled(true);
        let name = self.name_input.value();
        let room = self.room_input.value();
        if self.colorblind_checkbox.checked() {
            self.base.doc.get_element_by_id("playing")
                .ok_or_else(|| JsValue::from_str("No playing box"))?
                .class_list()
                .add_1("colorblind")?;
        }
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
           active_player: usize, player_index: usize,
           in_board: &[((i32, i32), Piece)],
           pieces: &[Piece]) -> JsResult<Playing>
    {
        // The title lists the room name
        let s: HtmlElement = base.doc.get_element_by_id("room_name")
            .expect("Could not get room_name")
            .dyn_into()?;
        s.set_text_content(Some(&room_name));

        let board = Board::new(&base.doc)?;

        let b = base.doc.get_element_by_id("chat_name")
            .expect("Could not get chat_name");
        b.set_text_content(Some(&format!("{}:", players[player_index].0)));

        // If Enter is pressed while focus is in the chat box,
        // send a chat message to the server.
        let chat_input = base.doc.get_element_by_id("chat_input")
            .expect("Could not get chat_input")
            .dyn_into()?;
        let keyup_cb = set_event_cb(&chat_input, "keyup",
            move |e: KeyboardEvent| {
                if e.key_code() == 13 { // Enter key
                    e.prevent_default();
                    HANDLE.lock().unwrap().on_send_chat()
                } else {
                    Ok(())
                }
            });

        let chat_div = base.doc.get_element_by_id("chat_msgs")
            .expect("Could not get chat_div")
            .dyn_into()?;
        let score_table = base.doc.get_element_by_id("score_rows")
            .expect("Could not get score_rows")
            .dyn_into()?;

        let mut out = Playing {
            base,
            board,

            chat_input,
            chat_div,
            score_table,
            player_index,
            active_player,
            player_names: Vec::new(),

            _keyup_cb: keyup_cb,
        };

        for ((x, y), p) in in_board.iter() {
            out.board.add_piece(*p, *x, *y)?;
        }
        for p in pieces.iter() {
            out.board.add_hand(*p)?;
        }

        for (i, (name, score, connected)) in players.iter().enumerate() {
            out.add_player_row(
                if i == player_index {
                    format!("{} (you)", name)
                } else {
                    name.to_string()
                },
                *score as usize, *connected)?;
        }
        Ok(out)
    }

    fn on_chat(&self, from: &str, msg: &str) -> JsError {
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
        self.chat_div.set_scroll_top(self.chat_div.scroll_height());
        Ok(())
    }

    fn on_information(&self, msg: &str) -> JsError {
        let p = self.base.doc.create_element("p")?;
        p.set_class_name("msg");

        let i = self.base.doc.create_element("i")?;
        i.set_text_content(Some(msg));
        p.append_child(&i)?;
        self.chat_div.append_child(&p)?;
        self.chat_div.set_scroll_top(self.chat_div.scroll_height());
        Ok(())
    }

    fn add_player_row(&mut self, name: String, score: usize, connected: bool)
        -> JsError
    {
        let tr = self.base.doc.create_element("tr")?;
        tr.set_class_name("player-row");

        let td = self.base.doc.create_element("td")?;
        let i = self.base.doc.create_element("i")?;
        i.set_class_name("fas fa-caret-right");
        td.append_child(&i)?;
        tr.append_child(&td)?;

        let td = self.base.doc.create_element("td")?;
        td.set_text_content(Some(&name));
        tr.append_child(&td)?;

        let td = self.base.doc.create_element("td")?;
        td.set_text_content(Some(&score.to_string()));
        tr.append_child(&td)?;

        if !connected {
            tr.class_list().add_1("disconnected")?;
        }

        self.score_table.append_child(&tr)?;
        self.player_names.push(name);

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

    fn on_new_player(&mut self, name: &str) -> JsError {
        // Append a player to the bottom of the scores list
        self.add_player_row(name.to_string(), 0, true)?;
        self.on_information(&format!("{} joined the room", name))
    }

    fn on_player_disconnected(&self, index: usize) -> JsError {
        let c = self.score_table.child_nodes()
            .item((index + 3) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?;
        c.class_list().add_1("disconnected")?;
        self.on_information(&format!("{} disconnected",
                                     self.player_names[index]))
    }

    fn on_player_reconnected(&self, index: usize) -> JsError {
        let c = self.score_table.child_nodes()
            .item((index + 3) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?;
        c.class_list().remove_1("disconnected")?;
        self.on_information(&format!("{} reconnected",
                                     self.player_names[index]))
    }

    fn on_player_turn(&mut self, active_player: usize)
        -> JsError
    {
        let children = self.score_table.child_nodes();
        children
            .item((self.active_player + 3) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .remove_1("active")?;

        self.active_player = active_player;
        children
            .item((self.active_player + 3) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .add_1("active")?;

        if self.active_player == self.player_index {
            self.on_information("It's your turn!")
        } else {
            self.on_information(
                &format!("It's {}'s turn!",
                         self.player_names[self.active_player]))
        }?;

        self.board.set_my_turn(active_player == self.player_index)
    }

    fn on_anim(&mut self, t: f64) -> JsError {
        self.board.on_anim(t)
    }

    fn on_pan_start(&mut self, evt: PointerEvent) -> JsError {
        self.board.on_pan_start(evt)
    }

    fn on_pan_move(&mut self, evt: PointerEvent) -> JsError {
        self.board.on_pan_move(evt)
    }

    fn on_pan_end(&mut self, evt: Event) -> JsError {
        self.board.on_pan_end(evt)
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

    fn on_reject_button(&mut self, evt: Event) -> JsError {
        self.board.on_reject_button(evt)
    }

    fn on_accept_button(&mut self, evt: Event) -> JsError {
        match self.board.make_move(evt)? {
            Move::Place(m) => self.base.send(ClientMessage::Play(m)),
            Move::Swap(m) => self.base.send(ClientMessage::Swap(m)),
        }
    }

    fn on_played(&mut self, pieces: &[(Piece, i32, i32)]) -> JsError {
        let mut anims = Vec::new();
        let t0 = get_time_ms();
        for (piece, x, y) in pieces {
            let target = self.board.add_piece(*piece, *x, *y)?;
            anims.push(TileAnimation {
                target,
                start: (225.0, *y as f32 * 10.0),
                end: (*x as f32 * 10.0, *y as f32 * 10.0),
                t0 });
        }
        // If we're panning, we need to cancel the pan state before starting
        // an animation, otherwise a mouse-up will mess things up.
        if let BoardState::Panning(_) = &self.board.state {
            self.board.on_pan_end(Event::new("CancelPan")?)?;
        }
        self.board.state = BoardState::Animation(DragAnim::DropManyToGrid(DropManyToGrid(anims)));
        self.board.request_animation_frame()?;
        Ok(())
    }

    fn active_player_name(&self) -> &str {
        if self.active_player == self.player_index {
            "You"
        } else {
            &self.player_names[self.active_player]
        }
    }

    fn on_swapped(&mut self, count: usize) -> JsError {
        self.on_information(&format!("{} swapped {} piece{}",
            self.active_player_name(), count,
            if count > 1 { "s" } else { "" }))
    }

    fn on_move_accepted(&mut self, dealt: &[Piece]) -> JsError {
        self.board.on_move_accepted(dealt)
    }

    fn on_move_rejected(&mut self) -> JsError {
        Ok(())
    }

    fn on_player_score(&mut self, delta: u32, total: u32) -> JsError {
        self.score_table.child_nodes()
            .item(self.active_player as u32 + 3)
            .expect("Could not get table row")
            .child_nodes()
            .item(2)
            .expect("Could not get score value")
            .set_text_content(Some(&total.to_string()));
        self.on_information(&format!("{} scored {} point{}",
            self.active_player_name(),
            delta,
            if delta == 1 { "" } else { "s" }))
    }

    fn on_finished(&mut self, winner: usize) -> JsError {
        self.board.set_my_turn(false)?;

        let children = self.score_table.child_nodes();
        children
            .item((self.active_player + 3) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .remove_1("active")?;

        if winner == self.player_index {
            self.on_information("You win!")
        } else {
            self.on_information(&format!("{} wins!",
                self.player_names[winner]))
        }
    }

    fn on_pieces_remaining(&mut self, remaining: usize) -> JsError {
        self.board.pieces_remaining = remaining;
        self.board.update_exchange_div(self.active_player == self.player_index)
    }
}

////////////////////////////////////////////////////////////////////////////////


fn on_message(msg: ServerMessage) -> JsError {
    use ServerMessage::*;
    console_log!("Got message {:?}", msg);

    let mut state = HANDLE.lock().unwrap();

    match msg {
        JoinFailed(name) => state.on_join_failed(&name),
        JoinedRoom{room_name, players, active_player, player_index, board, pieces} =>
            state.on_joined_room(&room_name, &players,
                                 active_player, player_index,
                                 &board, &pieces),
        Chat{from, message} => state.on_chat(&from, &message),
        Information(message) => state.on_information(&message),
        NewPlayer(name) => state.on_new_player(&name),
        PlayerDisconnected(index) => state.on_player_disconnected(index),
        PlayerReconnected(index) => state.on_player_reconnected(index),
        PlayerTurn(active_player) => state.on_player_turn(active_player),
        PiecesRemaining(remaining) => state.on_pieces_remaining(remaining),
        Played(pieces) => state.on_played(&pieces),
        Swapped(count) => state.on_swapped(count),
        MoveAccepted(dealt) => state.on_move_accepted(&dealt),
        MoveRejected => state.on_move_rejected(),
        PlayerScore{delta, total} =>
            state.on_player_score(delta, total),
        ItsOver(winner) => state.on_finished(winner),
    }
}

////////////////////////////////////////////////////////////////////////////////

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> JsError {
    console_error_panic_hook::set_once();

    let window = web_sys::window()
        .expect("no global `window` exists");
    let doc = window.document()
        .expect("should have a document on window");

    let location = doc.location()
        .expect("Could not get doc location");
    let href = location.href()?;
    let hostname = location.hostname()?;

    // Request the URL for the websocket server
    let url = format!("{}/ws", href);
    let request = Request::new_with_str(&url)?;
    let fetch = window.fetch_with_request(&request);
    let ws_protocol = if location.protocol()? == "https" {
        "wss"
    } else {
        "ws"
    };

    let text_cb = Closure::wrap(Box::new(move |text: JsValue| {
        console_log!("{:?}", text);
        start(text).expect("Could not start app");
    }) as Box<dyn FnMut(JsValue)>);
    let fetch_cb = Closure::wrap(Box::new(move |e: JsValue| {
        console_log!("{:?}", e);
        let r: Response = e.dyn_into()
            .expect("Could not cast to response");
        if r.ok() {
            let _ = r.text()
                .expect("Could not create text() promise")
                .then(&text_cb);
        } else {
            start(JsValue::from_str(
                    &format!("{}://{}:8080", ws_protocol, hostname)))
                .expect("Could not start with default hostname");
        }
    }) as Box<dyn FnMut(JsValue)>);

    let _ = fetch.then(&fetch_cb);
    fetch_cb.forget();

    Ok(())
}

fn start(text: JsValue) -> JsError {
    let doc = web_sys::window()
        .expect("no global `window` exists")
        .document()
        .expect("should have a document on window");
    let hostname = text.as_string()
        .expect("Could not convert hostname to string");
    console_log!("Connecting to websocket at {}", hostname);
    let ws = WebSocket::new(&hostname)?;
    doc.get_element_by_id(
            if hostname.starts_with("ws://") {
                "conn-insecure"
            } else {
                "conn-secure"
            })
        .expect("Could not find header")
        .dyn_into::<HtmlElement>()?
        .set_hidden(false);

    // The websocket callbacks are long-lived, so we forget them here
    set_event_cb(&ws, "open", move |_: JsValue| {
        HANDLE.lock().unwrap()
            .on_connected()
    }).forget();
    let on_decoded_cb = Closure::wrap(Box::new(move |e: ProgressEvent| {
        let target = e.target().expect("Could not get target");
        let reader: FileReader = target.dyn_into().expect("Could not cast");
        let result = reader.result().expect("Could not get result");
        let buf = js_sys::Uint8Array::new(&result);
        let mut data = vec![0; buf.length() as usize];
        buf.copy_to(&mut data[..]);
        let msg = bincode::deserialize(&data[..])
            .map_err(|e| JsValue::from_str(
                    &format!("Failed to deserialize: {}", e)))
            .expect("Could not decode message");
        on_message(msg)
            .expect("Message decoding failed")
    }) as Box<dyn FnMut(ProgressEvent)>);
    set_event_cb(&ws, "message", move |e: MessageEvent| {
        let blob = e.data().dyn_into::<Blob>()?;
        let fr = FileReader::new()?;
        fr.add_event_listener_with_callback("load",
                &on_decoded_cb.as_ref().unchecked_ref())?;
        fr.read_as_array_buffer(&blob)?;
        Ok(())
    }).forget();
    set_event_cb(&ws, "close", move |_: Event| -> JsError {
        let doc = web_sys::window()
            .expect("no global `window` exists")
            .document()
            .expect("should have a document on window");
        for d in ["join", "playing"].iter() {
            doc.get_element_by_id(d)
                .expect("Could not get major div")
                .dyn_into::<HtmlElement>()?
                .set_hidden(true);
        }
        doc.get_element_by_id("disconnected")
            .expect("Could not get disconnected div")
            .dyn_into::<HtmlElement>()?
            .set_hidden(false);
        Ok(())
    }).forget();

    let rev = doc.get_element_by_id("revhash")
        .expect("Could not find rev");
    rev.set_text_content(Some(env!("VERGEN_SHA_SHORT")));

    let base = Base { doc, ws };
    base.doc.get_element_by_id("play_button")
        .expect("Could not get loading div")
        .dyn_into::<HtmlElement>()?
        .set_text_content(Some("Connecting..."));

    *HANDLE.lock().unwrap() = State::Connecting(Connecting { base });

    Ok(())
}
