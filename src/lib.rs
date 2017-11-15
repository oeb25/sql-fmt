#![feature(try_from)]
#![feature(match_default_bindings)]

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate rayon;

mod keywords;
mod parser;
mod format;
mod ast;

use format::Format;
use std::sync::Arc;

pub fn prettify(s: &str, _options: Option<format::Options>) -> Result<String, parser::ParseError> {
    let ctx = Arc::new(format::Context::default());
    Ok(parser::parse(&s)?.format(&ctx))
}
