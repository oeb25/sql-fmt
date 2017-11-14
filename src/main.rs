#![feature(try_from)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;
mod format;
mod ast;

use format::Format;

fn prettify(s: &str, _options: Option<format::Options>) -> Result<String, parser::ParseError> {
    let ctx = format::Context::default();
    Ok(parser::parse(&s)?.format(&ctx))
}

fn main() {
    use std::io::Read;
    let name = std::env::args().skip(1).next().unwrap();
    let mut d = String::new();
    std::fs::File::open(name)
        .unwrap()
        .read_to_string(&mut d)
        .unwrap();

    let a = prettify(&d, None).unwrap();
    println!("{}", a);

    let b = prettify(&a, None).unwrap();
    println!("{}", b);

    assert_eq!(a, b);
}
