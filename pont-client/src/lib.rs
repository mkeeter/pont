use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::sync::{Arc, Mutex};
use web_sys::{
    Event,
    EventTarget,
    Document,
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

fn clear_err_span() {
    let (doc, div) = doc_div();
    if let Some(err) = doc.get_element_by_id("error") {
        div.remove_child(&err);
    }
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

    fn on_message(&mut self, msg: ServerMessage) -> Result<(), JsValue> {
        match msg {
            ServerMessage::UnknownRoom(name) => self.on_unknown_room(name),
            _ => Ok(()),
        }
    }

    fn on_connected(&mut self) -> Result<(), JsValue> {
        assert!(self.state == State::Connecting);
        self.state = State::CreateOrJoin;

        let (doc, div) = doc_div();
        div.remove_child(&div.child_nodes().item(0).expect("div should have one child"))?;

        // When any of the text fields change, check to see whether
        // the "Join" button should be enabled
        let p = doc.create_element("p")?;
        p.set_text_content(Some("Name: "));
        let name_input = doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        name_input.set_id("name_input");
        name_input.set_attribute("placeholder", "John Smith")?;
        p.append_child(&name_input)?;
        div.append_child(&p)?;

        let p = doc.create_element("p")?;
        p.set_text_content(Some("Room: "));
        let room_input = doc.create_element("input")?
            .dyn_into::<HtmlInputElement>()?;
        room_input.set_attribute("placeholder", "Create new room")?;
        room_input.set_id("room_input");
        p.append_child(&room_input)?;
        div.append_child(&p)?;

        let p = doc.create_element("p")?;
        let button = doc.create_element("button")?
            .dyn_into::<HtmlButtonElement>()?;
        button.set_text_content(Some("Play!"));
        button.set_id("play_button");
        p.append_child(&button)?;
        button.set_disabled(true);
        div.append_child(&p)?;

        let validate_join_inputs = {
            let button = button.clone();
            let name_input = name_input.clone();
            let room_input = room_input.clone();
            // Whenever either text field changes, call this validation
            // function and enable / disable the Join button
            move |_: Event| {
                button.set_disabled({
                    let name = name_input.value();
                    let room = room_input.value();
                    if name.is_empty() {
                        true
                    } else if room.is_empty() {
                        false
                    } else {
                        room.trim().chars().filter(|c| *c == ' ').count() != 2
                    }
                });
            }
        };
        set_event_cb(&name_input, "input", validate_join_inputs.clone());
        set_event_cb(&room_input, "input", validate_join_inputs);

        // todo: use https://stackoverflow.com/a/24245592 to make Enter work

        let ws = self.ws.clone();
        set_event_cb(&button.clone(), "click", move |_: Event| {
            button.set_disabled(true);
            let name = name_input.value();
            let room = room_input.value();
            let msg = if room.is_empty() {
                ClientMessage::CreateRoom(name)
            } else {
                ClientMessage::JoinRoom(name, room)
            };
            let encoded = serde_json::to_string(&msg)
                .expect("Failed to encode");
            ws.send_with_str(&encoded)
                .expect("Could not send message");
        });

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

    set_event_cb(&ws, "close", move |e: Event| {
        panic!("WS closed?!");
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
