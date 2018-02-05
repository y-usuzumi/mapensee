use bytes::{BytesMut, BufMut};
use byteorder::{BigEndian};
use super::consts;
use super::{Message, Emo};

pub trait Encoder {
    fn encode_into(&mut self, buf: &mut BytesMut);
}

impl Encoder for String {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        let len = self.len();
        if len > consts::TEXT_SLICE_MAX_LENGTH_S {
            buf.put_u32::<BigEndian>(consts::TEXT_OVERFLOW_FLAG);
            buf.extend(self[..consts::TEXT_SLICE_MAX_LENGTH_S-1].bytes());
            String::from(&self[consts::TEXT_SLICE_MAX_LENGTH_S..]).encode_into(buf);
        } else {
            buf.put_u32::<BigEndian>(self.len() as u32);
            buf.extend(self.as_bytes());
        }
    }
}

impl Encoder for Emo {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        match *self {
            Emo::Nop => {
                buf.put_u8(consts::MESSAGE_EMO_CODE_NOP);
            },
            Emo::Laugh => {
                buf.put_u8(consts::MESSAGE_EMO_CODE_LAUGH);
            },
            Emo::Cry => {
                buf.put_u8(consts::MESSAGE_EMO_CODE_CRY);
            },
            Emo::Custom(_) => {
                panic!("Not implemented yet");
            }
        }
    }
}

impl Encoder for Message {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        // Type code
        buf.put_u8(match *self {
            Message::Nop => consts::MESSAGE_TYPE_CODE_NOP,
            Message::Text(_) => consts::MESSAGE_TYPE_CODE_TEXT,
            Message::Emo(_) => consts::MESSAGE_TYPE_CODE_EMO,
            Message::Image(_, _) => consts::MESSAGE_TYPE_CODE_IMAGE,
            Message::Compound(_) => consts::MESSAGE_TYPE_CODE_COMPOUND,
        });
        match *self {
            Message::Text(ref mut t) => t.encode_into(buf),
            Message::Emo(ref mut e) => e.encode_into(buf),
            Message::Image(t, ref d) => {
                buf.put_u8(t);
                buf.extend(d);
            },
            Message::Compound(ref mut msgs) => {
                let len = msgs.len();
                if len > consts::COMPOUND_SLICE_MAX_LENGTH_S {
                    let mut start = 0;
                    let start_finish_point = len - consts::COMPOUND_SLICE_MAX_LENGTH_S;
                    while start < start_finish_point {
                        buf.put_u8(consts::COMPOUND_OVERFLOW_FLAG);
                        for msg in &mut msgs[start..start+consts::COMPOUND_SLICE_MAX_LENGTH_S] {
                            msg.encode_into(buf);
                        }
                    }
                    buf.put_u8((len - start) as u8);
                    for msg in &mut msgs[start..] {
                        msg.encode_into(buf);
                    }
                } else {
                    buf.put_u8(len as u8);
                    for msg in msgs {
                        msg.encode_into(buf);
                    }
                }
            },
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use bytes::BytesMut;
    use super::{Message, Emo, Encoder};

    #[test]
    fn encode_text() {
        let mut msg = Message::Text(String::from("Hello"));
        {
            let mut buf = BytesMut::new();
            msg.encode_into(&mut buf);
            // 1 byte type (Text: 128)
            // 4 byte text length 0005
            // 5 byte Hello
            assert_eq!(&buf[..], b"\x80\x00\x00\x00\x05Hello");
        }
    }

    #[test]
    fn encode_emo() {
        {
            let mut msg = Message::Emo(Emo::Nop);
            let mut buf = BytesMut::new();
            msg.encode_into(&mut buf);
            assert_eq!(&buf[..], b"\x82\x00");
        }
        {
            let mut msg = Message::Emo(Emo::Laugh);
            let mut buf = BytesMut::new();
            msg.encode_into(&mut buf);
            assert_eq!(&buf[..], b"\x82\x01");
        }
        {
            let mut msg = Message::Emo(Emo::Cry);
            let mut buf = BytesMut::new();
            msg.encode_into(&mut buf);
            assert_eq!(&buf[..], b"\x82\x02");
        }
        {  // TODO: Custom emo
        }
    }

    #[test]
    fn encode_img() {
        // TODO
    }

    #[test]
    fn encode_compound() {
        let mut msg = Message::Compound(
            vec![
                Message::Text(String::from("Hello")),
                Message::Emo(Emo::Laugh),
                Message::Text(String::from("world!"))
            ]
        );
        let mut buf = BytesMut::new();
        msg.encode_into(&mut buf);
        assert_eq!(&buf[..], b"\
        \xfa\x03\
        \x80\x00\x00\x00\x05Hello\
        \x82\x01\
        \x80\x00\x00\x00\x06world!");
    }
}