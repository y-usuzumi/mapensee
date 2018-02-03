use bytes::{BytesMut, BufMut};
use byteorder::{BigEndian};
use super::{
    TEXT_OVERFLOW_FLAG,
    TEXT_SLICE_MAX_LENGTH_S,
    COMPOUND_OVERFLOW_FLAG,
    COMPOUND_SLICE_MAX_LENGTH_S,
    Message,
    Emo
};

pub trait Encoder {
    fn encode_into(&mut self, buf: &mut BytesMut);
}

impl Encoder for String {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        let len = self.len();
        if len > TEXT_SLICE_MAX_LENGTH_S {
            buf.put_u32::<BigEndian>(TEXT_OVERFLOW_FLAG);
            buf.extend(self[..TEXT_SLICE_MAX_LENGTH_S-1].bytes());
            String::from(&self[TEXT_SLICE_MAX_LENGTH_S..]).encode_into(buf);
        } else {
            buf.put_u32::<BigEndian>(self.len() as u32);
            buf.extend(self.as_bytes());
        }
    }
}

impl Encoder for Message {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        // Type code
        buf.put_u8(match *self {
            Message::Nop => 0,
            Message::Text(_) => 128,
            Message::Emo(_) => 130,
            Message::Image(_, _) => 140,
            Message::Compound(_) => 250,
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
                if len > COMPOUND_SLICE_MAX_LENGTH_S {
                    let mut start = 0;
                    let start_finish_point = len - COMPOUND_SLICE_MAX_LENGTH_S;
                    while start < start_finish_point {
                        buf.put_u8(COMPOUND_OVERFLOW_FLAG);
                        for msg in &mut msgs[start..start+COMPOUND_SLICE_MAX_LENGTH_S] {
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

impl Encoder for Emo {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        match *self {
            Emo::Nop => {
                buf.put_u8(0);
            },
            Emo::Laugh => {
                buf.put_u8(1);
            },
            Emo::Cry => {
                buf.put_u8(2);
            },
            Emo::Custom(_) => {
                panic!("Not implemented yet");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use bytes::BytesMut;
    use super::{Message, Emo, Encoder};

    #[test]
    fn bitwise_op() {
        assert_eq!(2u32.pow(8), 256);
    }

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