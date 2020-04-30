use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::collections::HashMap;
use std::sync::Mutex;
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

enum DropTarget {
    DropToGrid(i32, i32),
    ReturnToGrid(i32, i32),
    ReturnToHand,
}

struct DropManyToGrid(Vec<TileAnimation>);
struct ReturnToHand(TileAnimation);
struct DragPan(Pos);
struct ReturnAllToHand(Vec<TileAnimation>);
struct ConsolidateHand(Vec<TileAnimation>);

enum DragState {
    Idle,
    Dragging(Dragging),
    DropToGrid(DropToGrid),
    DropManyToGrid(DropManyToGrid),
    ReturnToHand(ReturnToHand),
    DragPan(DragPan),
    ReturnAllToHand(ReturnAllToHand),
    ConsolidateHand(ConsolidateHand),
}

pub struct Board {
    doc: Document,
    svg: SvgGraphicsElement,

    drag: DragState,

    pan_group: Element,
    pan_offset: Pos,

    grid: HashMap<(i32, i32), Piece>,
    tentative: HashMap<(i32, i32), usize>,
    hand: Vec<(Piece, Element)>,

    accept_button: HtmlButtonElement,
    reject_button: HtmlButtonElement,

    pointer_down_cb: JsClosure<PointerEvent>,
    pointer_move_cb: JsClosure<PointerEvent>,
    pointer_up_cb: JsClosure<PointerEvent>,

    pan_move_cb: JsClosure<PointerEvent>,
    pan_end_cb: JsClosure<PointerEvent>,

    anim_cb: JsClosure<f64>,
}

impl Board {
    fn new(doc: &Document, game_div: &HtmlElement) -> JsResult<Board> {
        let dummy = doc.create_svg_element("svg")?
            .dyn_into::<SvgGraphicsElement>()?;
        dummy.set_attribute("viewBox", "0 0 200 200")?;
        dummy.set_id("dummy");

        // Add an SVG
        let svg = doc.create_svg_element("svg")?
            .dyn_into::<SvgGraphicsElement>()?;
        svg.set_id("game");
        svg.set_attribute("width", "100")?;
        svg.set_attribute("hight", "100")?;
        svg.set_attribute("viewBox", "0 0 200 200")?;


        // Add a clipping rectangle so that tiles don't drag outside
        // the main playing area
        let clip_defs = doc.create_svg_element("defs")?;
        let clip_path = doc.create_svg_element("clipPath")?;
        clip_path.set_id("clip_rect");
        let clip_rect = doc.create_svg_element("rect")?
            .dyn_into::<Element>()?;
        clip_rect.set_attribute("width", "200")?;
        clip_rect.set_attribute("height", "175")?;
        clip_rect.set_attribute("x", "0")?;
        clip_rect.set_attribute("y", "0")?;
        clip_path.append_child(&clip_rect)?;
        clip_defs.append_child(&clip_path)?;
        svg.append_child(&clip_defs)?;

        // Add a transparent background rectangle which is used for panning
        let pan_rect = doc.create_svg_element("rect")?
            .dyn_into::<Element>()?;
        pan_rect.set_attribute("width", "200")?;
        pan_rect.set_attribute("height", "175")?;
        pan_rect.set_attribute("x", "0")?;
        pan_rect.set_attribute("y", "0")?;
        svg.append_child(&pan_rect)?;
        pan_rect.set_id("transparent");
        set_event_cb(&pan_rect, "pointerdown", move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pan_start(evt)
        }).forget();

        let clip_group = doc.create_svg_element("g")?;
        clip_group.set_attribute("clip-path", "url(#clip_rect)")?;
        let pan_group = doc.create_svg_element("g")?
            .dyn_into::<Element>()?;
        clip_group.append_child(&pan_group)?;
        svg.append_child(&clip_group)?;
        let pan_offset = (5.0, 7.5);
        pan_group.set_attribute(
            "transform",
            &format!("translate({} {})", pan_offset.0, pan_offset.1))?;

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
        accept_button.set_disabled(true);
        svg_div.append_child(&accept_button)?;
        set_event_cb(&accept_button, "click", move |evt: Event| {
            HANDLE.lock().unwrap()
                .on_accept_button(evt)
        }).forget();

        let reject_button = doc.create_element("button")?
            .dyn_into::<HtmlButtonElement>()?;
        reject_button.set_inner_html("<i class=\"fas fa-times\"></i>");
        reject_button.set_id("reject_button");
        reject_button.set_class_name("gameplay");
        reject_button.set_disabled(true);
        svg_div.append_child(&reject_button)?;
        set_event_cb(&reject_button, "click", move |evt: Event| {
            HANDLE.lock().unwrap()
                .on_reject_button(evt)
        }).forget();

        svg_div.append_child(&svg)?;

        game_div.append_child(&svg_div)?;

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
        let pan_end_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pan_end(evt)
        });

        let out = Board {
            doc: doc.clone(),
            drag: DragState::Idle,
            svg,
            pan_group, pan_offset,
            grid: HashMap::new(),
            tentative: HashMap::new(),
            hand: Vec::new(),
            pointer_down_cb,
            pointer_up_cb,
            pointer_move_cb,
            pan_move_cb,
            pan_end_cb,
            anim_cb,
            accept_button,
            reject_button,
        };

        Ok(out)
    }

    fn set_my_turn(&mut self, is_my_turn: bool) -> JsError {
        if is_my_turn {
            self.svg.class_list().remove_1("nyt")
        } else {
            self.svg.class_list().add_1("nyt")
        }
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
        match self.drag {
            DragState::Idle => (),
            _ => return Ok(()),
        }
        // No panning before placing the first piece, to prevent griefing by
        // placing the piece far from the visible region.
        if self.grid.is_empty() {
            return Ok(());
        }

        evt.prevent_default();

        let p = self.mouse_pos(&evt);
        self.drag = DragState::DragPan(DragPan(
            (p.0 - self.pan_offset.0, p.1 - self.pan_offset.1)));

        let target = evt.target()
            .unwrap()
            .dyn_into::<Element>()?;

        target.set_pointer_capture(evt.pointer_id())?;
        target.add_event_listener_with_callback("pointermove",
                self.pan_move_cb.as_ref().unchecked_ref())?;
        target.add_event_listener_with_callback("pointerup",
                self.pan_end_cb.as_ref().unchecked_ref())?;
        Ok(())
    }

    fn on_pan_move(&mut self, evt: PointerEvent) -> JsError {
        if let DragState::DragPan(d) = &self.drag {
            evt.prevent_default();

            let p = self.mouse_pos(&evt);
            self.pan_offset = (p.0 - (d.0).0, p.1 - (d.0).1);
            self.pan_group.set_attribute("transform",
                                   &format!("translate({} {})",
                                   self.pan_offset.0,
                                   self.pan_offset.1))
        } else {
            Err(JsValue::from_str("Invalid state"))
        }
    }

    fn on_pan_end(&mut self, evt: PointerEvent) -> JsError {
        evt.prevent_default();

        let target = evt.target()
            .unwrap()
            .dyn_into::<Element>()?;
        target.release_pointer_capture(evt.pointer_id())?;
        target.remove_event_listener_with_callback("pointermove",
                self.pan_move_cb.as_ref().unchecked_ref())?;
        target.remove_event_listener_with_callback("pointerup",
                self.pan_end_cb.as_ref().unchecked_ref())?;
        self.drag = DragState::Idle;
        Ok(())
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
            self.mark_invalid()?;
            (hand_index, Some((x, y)))
        };
        target.class_list().remove_1("invalid")?;

        // Move to the back of the SVG object, so it's on top
        self.svg.append_child(&target)?;

        target.set_pointer_capture(evt.pointer_id())?;
        target.add_event_listener_with_callback("pointermove",
                self.pointer_move_cb.as_ref().unchecked_ref())?;
        target.add_event_listener_with_callback("pointerup",
                self.pointer_up_cb.as_ref().unchecked_ref())?;

        self.drag = DragState::Dragging(Dragging {
            target,
            shadow,
            offset: (mx - tx, my - ty),
            hand_index,
            grid_origin,
        });
        Ok(())
    }

    fn drop_target(&self, evt: &PointerEvent) -> JsResult<(Pos, DropTarget)> {
        if let DragState::Dragging(d) = &self.drag {
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
            let pos = (x, y);

            // If the tile is off the bottom of the grid, then we propose
            // to return it to the hand.
            if y >= 165.0 {
                return Ok((pos, DropTarget::ReturnToHand))
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
            Err(JsValue::from_str("Invalid state"))
        }
    }

    fn on_pointer_move(&self, evt: PointerEvent) -> JsError {
        if let DragState::Dragging(d) = &self.drag {
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
            Err(JsValue::from_str("Invalid state"))
        }
    }

    fn mark_invalid(&self) -> JsResult<bool> {
        let mut b = self.grid.clone();
        for (pos, index) in self.tentative.iter() {
            b.insert(*pos, self.hand[*index].0);
        }
        let invalid = Game::invalid(&b);
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
        if let DragState::Dragging(d) = &self.drag {
            evt.prevent_default();

            d.target.release_pointer_capture(evt.pointer_id())?;
            d.target.remove_event_listener_with_callback("pointermove",
                    self.pointer_move_cb.as_ref().unchecked_ref())?;
            d.target.remove_event_listener_with_callback("pointerup",
                    self.pointer_up_cb.as_ref().unchecked_ref())?;

            let (pos, drop_target) = self.drop_target(&evt)?;
            self.drag = match drop_target {
                DropTarget::ReturnToHand => {
                    self.pan_group.remove_child(&d.shadow)?;
                    DragState::ReturnToHand(ReturnToHand(
                        TileAnimation {
                            target: d.target.clone(),
                            start: pos,
                            end: ((d.hand_index * 15 + 5) as f32, 185.0),
                            t0: evt.time_stamp()
                        }))
                },
                DropTarget::DropToGrid(gx, gy) |
                DropTarget::ReturnToGrid(gx, gy) => {
                    self.tentative.insert((gx, gy), d.hand_index);
                    let target = d.target.clone();
                    self.svg.remove_child(&target)?;
                    self.pan_group.append_child(&target)?;
                    DragState::DropToGrid(DropToGrid{
                        anim: TileAnimation {
                            target,
                            start: (pos.0 - self.pan_offset.0, pos.1 - self.pan_offset.1),
                            end: (gx as f32 * 10.0, gy as f32 * 10.0),
                            t0: evt.time_stamp(),
                        },
                        shadow: d.shadow.clone(),
                    })
                }
            };
            self.mark_invalid()?;
            self.request_animation_frame()?;
        }
        Ok(())
    }

    fn on_anim(&mut self, t: f64) -> JsError {
        match &mut self.drag {
            DragState::DropToGrid(d) => {
                if d.anim.run(t)? {
                    self.request_animation_frame()?;
                } else {
                    self.pan_group.remove_child(&d.shadow)?;
                    self.drag = DragState::Idle;
                    self.accept_button.set_disabled(!self.mark_invalid()?);
                    self.reject_button.set_disabled(false);
                }
            },
            DragState::ReturnToHand(d) => {
                if d.0.run(t)? {
                    self.request_animation_frame()?;
                } else {
                    self.drag = DragState::Idle;
                    if self.tentative.is_empty() {
                        self.accept_button.set_disabled(true);
                        self.reject_button.set_disabled(true);
                    } else {
                        self.accept_button.set_disabled(!self.mark_invalid()?);
                    }
                }
            },
            DragState::ReturnAllToHand(d) => {
                let mut any_running = false;
                for a in d.0.iter() {
                    any_running |= a.run(t)?;
                }
                if any_running {
                    self.request_animation_frame()?;
                } else {
                    self.drag = DragState::Idle;
                    self.accept_button.set_disabled(true);
                    self.reject_button.set_disabled(true);
                }
            },
            DragState::ConsolidateHand(ConsolidateHand(d)) |
            DragState::DropManyToGrid(DropManyToGrid(d)) => {
                let mut any_running = false;
                for a in d.iter() {
                    any_running |= a.run(t)?;
                }
                if any_running {
                    self.request_animation_frame()?;
                } else {
                    self.drag = DragState::Idle;
                }
            },
            DragState::Dragging(_) | DragState::DragPan(_) | DragState::Idle =>
                panic!("Invalid drag state"),
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
        g.add_event_listener_with_callback("pointerdown",
            self.pointer_down_cb.as_ref().unchecked_ref())?;
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
        match self.drag {
            DragState::Idle => (),
            _ => return Ok(()),
        }

        let mut tiles = HashMap::new();
        std::mem::swap(&mut self.tentative, &mut tiles);
        for (_, i) in &tiles {
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
        self.drag = DragState::ReturnAllToHand(ReturnAllToHand(
            tiles.drain().map(|((tx, ty), i)|
                TileAnimation {
                    target: self.hand[i].1.clone(),
                    start: (tx as f32 * 10.0 + self.pan_offset.0,
                            ty as f32 * 10.0 + self.pan_offset.1),
                    end: ((i * 15 + 5) as f32, 185.0),
                    t0: evt.time_stamp()
                }).collect()));
        self.request_animation_frame()?;
        Ok(())
    }

    /*  Attempts to make the given move.
     *  If the move is valid (TODO), returns the indexes of placed pieces
     *  (as hand indexes), which can be passed up to the server. */
    fn make_move(&mut self, _evt: Event) -> JsResult<Vec<(Piece, i32, i32)>> {
        match self.drag {
            DragState::Idle => (),
            _ => return Ok(Vec::new()),
        }

        // Disable everything until we hear back from the server.
        //
        // If this is a one-player game, then it will be our turn again,
        // but we'll let the server tell us that.
        self.accept_button.set_disabled(true);
        self.reject_button.set_disabled(true);
        self.set_my_turn(false)?;

        Ok(self.tentative.iter()
            .map(|((x, y), i)| (self.hand[*i].0.clone(), *x, *y))
            .collect())
    }

    fn on_move_accepted(&mut self, dealt: &[Piece]) -> JsError {
        let mut placed = HashMap::new();
        for ((x, y), i) in self.tentative.drain() {
            placed.insert(i, (x, y));
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
                target: target,
                start: (x, 220.0),
                end: (x, 185.0),
                t0
            })
        }
        self.drag = DragState::ConsolidateHand(ConsolidateHand(anims));
        self.request_animation_frame()?;

        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////

pub struct Base {
    doc: Document,
    main_div: HtmlElement,
    ws: WebSocket,
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
                           active_players: usize,
                           board: &Vec<((i32, i32), Piece)>,
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
            on_pan_end(evt: PointerEvent),
            on_accept_button(evt: Event),
            on_reject_button(evt: Event),
            on_anim(t: f64),
            on_send_chat(),
            on_chat(from: &str, msg: &str),
            on_information(msg: &str),
            on_new_player(name: &str),
            on_player_disconnected(index: usize),
            on_player_turn(active_player: usize),
            on_played(pieces: &[(Piece, i32, i32)]),
            on_move_accepted(dealt: &[Piece]),
            on_move_rejected(),
            on_player_score(index: usize, delta: u32, total: u32),
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
        // Remove the "Connecting..." message
        self.base.clear_main_div()?;

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
        });
        let submit_cb = set_event_cb(&form, "submit", move |e: Event| {
            e.prevent_default();
            HANDLE.lock().unwrap().on_join_button()
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
                      active_player: usize, board: &Vec<((i32, i32), Piece)>,
                      pieces: &[Piece]) -> JsResult<Playing>
    {
        self.base.clear_main_div()?;
        let mut p = Playing::new(self.base, room_name, players,
                                 active_player, board, pieces)?;
        p.on_information(&format!("Welcome, {}!", players.last().unwrap().0))?;
        p.on_player_turn(active_player)?;
        Ok(p)
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
           active_player: usize, in_board: &Vec<((i32, i32), Piece)>,
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
                } else {
                    Ok(())
                }
            });

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
            .item((index + 1) as u32)
            .unwrap()
            .dyn_into::<HtmlElement>()?;
        c.class_list().add_1("disconnected")?;
        self.on_information(&format!("{} disconnected",
                                     self.player_names[index]))
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

    fn on_pan_end(&mut self, evt: PointerEvent) -> JsError {
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
        let m = self.board.make_move(evt)?;
        self.base.send(ClientMessage::Play(m))?;
        Ok(())
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
        self.board.drag = DragState::DropManyToGrid(DropManyToGrid(anims));
        self.board.request_animation_frame()?;
        Ok(())
    }

    fn on_move_accepted(&mut self, dealt: &[Piece]) -> JsError {
        self.board.on_move_accepted(dealt)
    }

    fn on_move_rejected(&mut self) -> JsError {
        Ok(())
    }

    fn on_player_score(&mut self, index: usize, delta: u32, total: u32)
        -> JsError
    {
        self.score_table.child_nodes()
            .item(index as u32 + 1)
            .unwrap()
            .child_nodes()
            .item(2)
            .unwrap()
            .set_text_content(Some(&total.to_string()));
        if index == self.player_index {
            self.on_information(&format!("You scored {} points", delta))
        } else {
            self.on_information(&format!("{} scored {} points",
                                         self.player_names[index], delta))
        }
    }
}

////////////////////////////////////////////////////////////////////////////////


fn on_message(msg: ServerMessage) -> JsError {
    use ServerMessage::*;
    console_log!("Got message {:?}", msg);

    let mut state = HANDLE.lock().unwrap();

    match msg {
        UnknownRoom(name) => state.on_unknown_room(&name),
        JoinedRoom{room_name, players, active_player, board, pieces} =>
            state.on_joined_room(&room_name, &players, active_player, &board, &pieces),
        Chat{from, message} => state.on_chat(&from, &message),
        Information(message) => state.on_information(&message),
        NewPlayer(name) => state.on_new_player(&name),
        PlayerDisconnected(index) => state.on_player_disconnected(index),
        PlayerTurn(active_player) => state.on_player_turn(active_player),
        Played(pieces) => state.on_played(&pieces),
        MoveAccepted(dealt) => state.on_move_accepted(&dealt),
        MoveRejected => state.on_move_rejected(),
        PlayerScore{index, delta, total} =>
            state.on_player_score(index, delta, total),
    }
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

    // The websocket callbacks are long-lived, so we forget them here
    set_event_cb(&ws, "open", move |_: JsValue| {
        HANDLE.lock().unwrap()
            .on_connected()
    }).forget();
    set_event_cb(&ws, "message", move |e: MessageEvent| {
        let msg = serde_json::from_str(&e.data().as_string().unwrap())
            .map_err(|e| JsValue::from_str(
                    &format!("Failed to deserialize: {}", e)))?;
        on_message(msg)
    }).forget();
    set_event_cb(&ws, "close", move |_: Event| -> JsError {
        console_log!("Socket closed");
        Ok(())
    }).forget();

    let base = Base { doc, main_div, ws };
    *HANDLE.lock().unwrap() = State::Connecting(Connecting { base });

    Ok(())
}
