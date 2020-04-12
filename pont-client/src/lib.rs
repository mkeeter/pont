use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::sync::{Arc, Mutex};
use web_sys::{WebSocket, MessageEvent};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

////////////////////////////////////////////////////////////////////////////////

struct State {
    ws: WebSocket,
}
impl State {
    fn on_message(&self, msg: pont_common::Command) {
        log(&format!("{:?}", msg));
    }

    fn start(&self) {
        self.ws.send_with_str("ping").expect("Failed to send ping");
    }
}


fn set_cb<F, T, G>(f: F, mut g: G)
    where F: FnMut(T) + 'static,
          T: FromWasmAbi + 'static,
          G: FnMut(Option<&js_sys::Function>)
{
    let cb = Closure::wrap(Box::new(f) as Box<dyn FnMut(T)>);
    g(Some(cb.as_ref().unchecked_ref()));
    cb.forget();
}

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let body = document.body().expect("document should have a body");

    // Manufacture the element we're gonna append
    let val = document.create_element("p")?;
    val.set_inner_html("Hello from Rust!");

    body.append_child(&val)?;

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

    let ws = WebSocket::new("ws://0.0.0.0:8080")?;

    let handle = Arc::new(Mutex::new(State {
        ws: ws.clone()
    }));

    let handle_ = handle.clone();
    set_cb(move |_: JsValue| {
        let state = handle_.lock().unwrap();
        state.start();
    }, |cb| ws.set_onopen(cb));

    let handle_ = handle.clone();
    set_cb(move |e: MessageEvent| {
        let msg = serde_json::from_str(&e.data().as_string().unwrap())
            .unwrap();
        let state = handle_.lock().unwrap();
        state.on_message(msg);
    }, |cb| ws.set_onmessage(cb));

    Ok(())
}
