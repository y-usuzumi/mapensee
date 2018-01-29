#![feature(never_type)]

extern crate bytes;
extern crate futures;
extern crate tokio_io;
extern crate tokio_proto;
extern crate tokio_service;
extern crate tokio_playground;

fn main() {
    let x = 1;
    let y = 2;
    println!("{}", x + y);
}
