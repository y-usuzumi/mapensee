use std;

#[derive(Debug)]
pub enum Error {
    InvalidTypeCode(u8),
    InvalidEmoCode(u8),
    IOError(std::io::Error)
}