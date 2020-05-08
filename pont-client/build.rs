fn main() {
    let flags = vergen::ConstantsFlags::all();
    vergen::generate_cargo_keys(flags)
        .expect("Unable to generate the cargo keys!");
}
