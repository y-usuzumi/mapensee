use std;
use std::str;
use std::io::{Cursor, Read};
use bytes::{BytesMut};
use byteorder::{BigEndian, ReadBytesExt};
use super::error::Error;

use super::{TEXT_OVERFLOW_FLAG, TEXT_SLICE_MAX_LENGTH_S};

type Result<T> = std::result::Result<T, Error>;

pub trait Decoder {
    fn decode_from(buf: &BytesMut) -> Result<Self> where Self: Sized;
}

impl Decoder for String {
    fn decode_from(buf: &BytesMut) -> Result<Self> {
        let mut cursor = Cursor::new(buf);
        let mut result = String::new();
        loop {
            let opt_len = cursor.read_u32::<BigEndian>();
            match opt_len {
                Ok(len) => {
                    if len == TEXT_OVERFLOW_FLAG {
                        let mut slice = [0; TEXT_SLICE_MAX_LENGTH_S];
                        let _ = cursor.read(&mut slice);
                        let s = str::from_utf8(&slice).unwrap();
                        result.push_str(s);
                    } else {
                        let mut slice = vec![0; len as usize];
                        let _ = cursor.read(&mut slice);
                        let s = str::from_utf8(&slice).unwrap();
                        result.push_str(s);
                        break
                    }
                },
                Err(_) => break
            }
        };
        return Ok(result);
    }
}
