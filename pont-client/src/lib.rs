use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::sync::{Arc, Mutex};
use web_sys::{WebSocket, Event, Element, EventTarget, MessageEvent, Document, HtmlElement, HtmlButtonElement, HtmlInputElement};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
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

fn validate_join_inputs() {
    let doc = document();
    let name_input = doc.get_element_by_id("name_input")
         .expect("Could not find `name_input` element")
         .dyn_into::<HtmlInputElement>()
         .expect("Failed to convert into `HtmlInputElement`");
    let room_input = doc.get_element_by_id("room_input")
         .expect("Could not find `room_input` element")
         .dyn_into::<HtmlInputElement>()
         .expect("Failed to convert into `HtmlInputElement`");
     log(&format!("got {}, {}", name_input.value(),
                                room_input.value()));

    let play_button = doc.get_element_by_id("play_button")
         .expect("Could not find `name_input` element")
         .dyn_into::<HtmlButtonElement>()
         .expect("Failed to convert into `HtmlButtonElement`");
     play_button.set_disabled(name_input.value().is_empty());
}

impl Handle {
    fn on_message(&mut self, msg: pont_common::ServerMessage) -> Result<(), JsValue> {
        log(&format!("{:?}", msg));

        Ok(())
    }

    fn on_connected(&mut self) -> Result<(), JsValue> {
        assert!(self.state == State::Connecting);
        self.state = State::CreateOrJoin;

        let (doc, div) = doc_div();
        div.remove_child(&div.child_nodes().item(0).expect("div should have one child"))?;
        log("removed child");
        log(&format!("{:?}", div.child_nodes().item(0)));

        // When any of the text fields change, check to see whether
        // the "Join" button should be enabled
        let bind_input_validator = |input: &Element| {
            let et = input.clone().dyn_into::<EventTarget>()
                    .expect("Could not convert input into `EventTarget`");
            set_cb(move |_: Event| validate_join_inputs(),
                |cb| {
                    et.add_event_listener_with_callback("input", cb.unwrap())
                        .expect("Could not add event listener");
                });
        };
        let p = doc.create_element("p")?;
        p.set_inner_html("Name: ");
        let input = doc.create_element("input")?;
        input.set_id("name_input");
        input.set_attribute("placeholder", "John Smith")?;
        p.append_child(&input)?;
        bind_input_validator(&input);
        div.append_child(&p)?;

        let p = doc.create_element("p")?;
        p.set_inner_html("Room: ");
        let input = doc.create_element("input")?;
        input.set_attribute("placeholder", "Create new room")?;
        input.set_id("room_input");
        bind_input_validator(&input);
        p.append_child(&input)?;
        div.append_child(&p)?;

        let p = doc.create_element("p")?;
        let button = doc.create_element("button")?;
        button.set_inner_html("Play!");
        button.set_id("play_button");
        p.append_child(&button)?;
        button.clone().dyn_into::<HtmlButtonElement>()?.set_disabled(true);
        div.append_child(&p)?;

        // todo: use https://stackoverflow.com/a/24245592 to make Enter work

        let ws = self.ws.clone();
        let button = button.dyn_into::<HtmlElement>().unwrap();
        set_cb(move |_: Event| {
            let msg = pont_common::ClientMessage::JoinRoom("HI".to_string(), "THERE".to_string());
            let encoded = serde_json::to_string(&msg).expect("Failed to encode");
            ws.send_with_str(&encoded).expect("Could not send message");
            log("HIIIIII");
            log(&format!("msg: {:?}", encoded));
        },
        |cb| button.set_onclick(cb));

        Ok(())
    }
}

// Boilerplate to wrap, bind, and forget a callback
fn set_cb<F, T, G>(f: F, mut g: G)
    where F: FnMut(T) + 'static,
          T: FromWasmAbi + 'static,
          G: FnMut(Option<&js_sys::Function>)
{
    let cb = Closure::wrap(Box::new(f) as Box<dyn FnMut(T)>);
    g(Some(cb.as_ref().unchecked_ref()));
    cb.forget();
}

////////////////////////////////////////////////////////////////////////////////

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    let doc = document();
    let body = doc.body().expect("document should have a body");

    // Manufacture the element we're gonna append
    let val = doc.create_element("p")?;
    val.set_inner_html("Connecting...");

    let div = doc.create_element("div")?;
    div.set_id("main");
    div.append_child(&val)?;

    body.append_child(&div)?;

    let ws = WebSocket::new("ws://0.0.0.0:8080")?;

    let handle = Arc::new(Mutex::new(Handle {
        ws: ws.clone(),
        state: State::Connecting,
    }));

    let handle_ = handle.clone();
    set_cb(move |_: JsValue| {
        let mut state = handle_.lock().unwrap();
        state.on_connected().expect("Failed to connect");
    }, |cb| ws.set_onopen(cb));

    let handle_ = handle.clone();
    set_cb(move |e: MessageEvent| {
        let msg = serde_json::from_str(&e.data().as_string().unwrap())
            .unwrap();
        let mut state = handle_.lock().unwrap();
        state.on_message(msg).expect("Failed to handle message");
    }, |cb| ws.set_onmessage(cb));

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
