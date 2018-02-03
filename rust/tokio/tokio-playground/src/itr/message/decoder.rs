use std;
use std::str;
use std::io::{Cursor, Read, Seek, SeekFrom};
use bytes::{BytesMut};
use byteorder::{BigEndian, ReadBytesExt};
use super::{Message};
use super::error::Error;

use super::consts;

type Result<T> = std::result::Result<T, Error>;

pub trait Decoder {
    fn decode_from(buf: &mut BytesMut) -> Result<Self> where Self: Sized;
}

impl Decoder for String {
    fn decode_from(buf: &mut BytesMut) -> Result<Self> {
        let mut cursor = Cursor::new(buf);
        let mut result = String::new();
        loop {
            let opt_len = cursor.read_u32::<BigEndian>();
            match opt_len {
                Ok(len) => {
                    if len == consts::TEXT_OVERFLOW_FLAG {
                        let mut slice = vec![0; consts::TEXT_SLICE_MAX_LENGTH_S];
                        let _ = cursor.read(&mut slice);
                        let s = str::from_utf8(&slice).unwrap();
                        result.push_str(s);
                    } else {
                        let mut slice = vec![0; len as usize];
                        if let Err(e) = cursor.read(&mut slice) {
                            return Err(Error::IOError(e));
                        }
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

impl Decoder for Message {
    fn decode_from(buf: &mut BytesMut) -> Result<Self> {
        let type_code = buf[0];
        buf.advance(1);
        let msg = match type_code {
            consts::MESSAGE_NOP_TYPE_CODE => Message::Nop,
            consts::MESSAGE_TEXT_TYPE_CODE => {
                let s = try!(String::decode_from(buf));
                Message::Text(s)
            },
            consts::MESSAGE_EMO_TYPE_CODE => {
                Message::Nop
            },
            consts::MESSAGE_IMAGE_TYPE_CODE => {
                Message::Nop
            },
            consts::MESSAGE_COMPOUND_TYPE_CODE => {
                Message::Nop
            },
            _ => return Err(Error::InvalidTypeCode(type_code))
        };
        return Ok(msg);
    }
}

#[cfg(test)]
mod tests {
    use bytes::{BytesMut};
    use super::{Message, Decoder};

    #[test]
    fn decode_text() {
        let mut bm = BytesMut::from(
            &b"\x80\x00\x00\x00\x10ITRE\xe8\xa7\xa3\xe7\xa0\x81\xe6\xb5\x8b\xe8\xaf\x95"[..]
        );
        let msg = Message::decode_from(&mut bm);
        assert!(match msg {
            Ok(_) => true,
            Err(_) => false
        });
        assert!(match msg.unwrap() {
            Message::Text(s) => s == String::from("ITRE解码测试"),
            _ => false
        });
    }
}
