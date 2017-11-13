#![feature(try_from)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;

fn main() {
    use std::io::Read;
    let name = std::env::args().skip(1).next().unwrap();
    let mut d = String::new();
    std::fs::File::open(name)
        .unwrap()
        .read_to_string(&mut d)
        .unwrap();
    let doc = parser::Document::parse(&d);
    println!("{:?}", doc);
}
