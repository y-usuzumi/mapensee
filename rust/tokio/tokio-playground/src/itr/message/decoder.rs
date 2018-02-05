use std;
use std::str;
use std::io::{Cursor, Read};
use bytes::{BytesMut};
use byteorder::{BigEndian, ReadBytesExt};
use super::{Message, Emo};
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
            let opt_len = cursor.read_u16::<BigEndian>();
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

impl Decoder for Emo {
    fn decode_from(buf: &mut BytesMut) -> Result<Self> {
        let emo_code = buf[0];
        buf.advance(1);
        let msg = match emo_code {
            consts::MESSAGE_EMO_CODE_NOP => Emo::Nop,
            consts::MESSAGE_EMO_CODE_LAUGH => Emo::Laugh,
            consts::MESSAGE_EMO_CODE_CRY => Emo::Cry,
            _ => return Err(Error::InvalidEmoCode(emo_code))
        };
        Ok(msg)
    }
}

impl Decoder for Message {
    fn decode_from(buf: &mut BytesMut) -> Result<Self> {
        let type_code = buf[0];
        buf.advance(1);
        let msg = match type_code {
            consts::MESSAGE_TYPE_CODE_NOP => Message::Nop,
            consts::MESSAGE_TYPE_CODE_TEXT => {
                let s = try!(String::decode_from(buf));
                Message::Text(s)
            },
            consts::MESSAGE_TYPE_CODE_EMO => {
                let s = try!(Emo::decode_from(buf));
                Message::Emo(s)
            },
            consts::MESSAGE_TYPE_CODE_IMAGE => {
                panic!("Not implemented yet");
            },
            consts::MESSAGE_TYPE_CODE_COMPOUND => {
                let mut length = buf[0];
                buf.advance(1);
                let mut msgs = Vec::new();
                while length == consts::COMPOUND_OVERFLOW_FLAG {
                    for _ in 0..consts::COMPOUND_SLICE_MAX_LENGTH_S {
                        msgs.push(try!(Self::decode_from(buf)));
                    }
                    length = buf[0];
                    buf.advance(1);
                }
                for _ in 0..length {
                    msgs.push(try!(Self::decode_from(buf)));
                }
                Message::Compound(msgs)
            },
            _ => return Err(Error::InvalidTypeCode(type_code))
        };
        Ok(msg)
    }
}

#[cfg(test)]
mod tests {
    use bytes::{BytesMut};
    use super::{Message, Emo, Decoder};

    #[test]
    fn decode_text() {
        let mut bm = BytesMut::from(
            &b"\x80\x00\x10ITRE\xe8\xa7\xa3\xe7\xa0\x81\xe6\xb5\x8b\xe8\xaf\x95"[..]
        );
        let msg = Message::decode_from(&mut bm);
        assert_eq!(
            msg.unwrap(),
            Message::Text(String::from("ITRE解码测试"))
        );
    }

    #[test]
    fn decode_emo() {
        {
            let mut bm = BytesMut::from(&b"\x82\x00"[..]);
            let msg = Message::decode_from(&mut bm);
            assert_eq!(
                msg.unwrap(),
                Message::Emo(Emo::Nop)
            );
        }
        {
            let mut bm = BytesMut::from(&b"\x82\x01"[..]);
            let msg = Message::decode_from(&mut bm);
            assert_eq!(
                msg.unwrap(),
                Message::Emo(Emo::Laugh)
            );
        }
        {
            let mut bm = BytesMut::from(&b"\x82\x02"[..]);
            let msg = Message::decode_from(&mut bm);
            assert_eq!(
                msg.unwrap(),
                Message::Emo(Emo::Cry)
            );
        }
    }

    #[test]
    fn decode_message() {
        let mut bm = BytesMut::from(
            &b"\xFA\x04\
            \x80\x00\x10ITRE\xe8\xa7\xa3\xe7\xa0\x81\xe6\xb5\x8b\xe8\xaf\x95
            \x82\x01
            \x80\x00\x10ITRE\xe8\xa7\xa3\xe7\xa0\x81\xe6\xb5\x8b\xe8\xaf\x95
            \x82\x02"[..]
        );
        let msg = Message::decode_from(&mut bm);
        assert_eq!(
            msg.unwrap(),
            Message::Compound(vec![
                Message::Text(String::from("ITRE解码测试")),
                Message::Emo(Emo::Laugh),
                Message::Text(String::from("ITRE解码测试")),
                Message::Emo(Emo::Cry)
            ])
        );
    }
}
