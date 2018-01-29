use std::io;
use std::convert;

pub enum Error {
    
}

impl convert::From<io::Error> for Error {
    fn from(_: io::Error) -> Self {
        panic!("Not implemented")
    }
}