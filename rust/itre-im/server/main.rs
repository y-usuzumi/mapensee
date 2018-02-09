extern crate futures;
extern crate tokio_proto;
extern crate tokio_service;
extern crate itre_im;
extern crate itre;

use std::env;
use futures::future;
use tokio_service::Service;
use tokio_proto::TcpServer;
use itre::Message;
use itre_im::protocol::ItreProto;


struct ItreService;

impl Service for ItreService {
    type Request = itre::Message;
    type Response = itre::Message;
    type Error = std::io::Error;
    type Future = Box<future::Future<Item = Self::Response, Error = Self::Error>>;

    fn call(&self, req: Self::Request) -> Self::Future {
        println!("{:?}", req);
        Box::new(future::ok(itre::Message::Nop))
    }
}


fn main() {
    let mut args = env::args();
    let addr_str = args.nth(1).unwrap_or(String::from("127.0.0.1:9876"));
    let addr = addr_str.parse().unwrap();
    let server = TcpServer::new(ItreProto, addr);
    server.serve(|| Ok(ItreService))
}
