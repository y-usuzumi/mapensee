use bytes::{BytesMut, BufMut};
use byteorder::{LittleEndian};

pub trait Encoder {
    fn encode_into(&mut self, buf: &mut BytesMut);
}

impl Encoder for String {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        buf.put_u32::<LittleEndian>(self.len() as u32);
        buf.extend(self.as_bytes());
    }
}

pub enum Emo {
    Nop,             // 0
    Laugh,           // 1
    Cry,             // 2
    Custom(Vec<u8>)  // 10
}

impl Encoder for Emo {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        match self {
            &mut Emo::Nop => {
                buf.put_u8(0);
            },
            &mut Emo::Laugh => {
                buf.put_u8(1);
            },
            &mut Emo::Cry => {
                buf.put_u8(2);
            },
            &mut Emo::Custom(ref d) => {
                buf.put_u8(10);
                buf.extend(d)
            }
        }
    }
}

pub enum Message {
    Nop,                    // 0
    Open,                   // 1
    Close,                  // 2
    Text(String),           // 10
    Emo(Emo),               // 11
    Image(u8, Vec<u8>),     // 12
    Compound(Vec<Message>)  // 20
}

impl Encoder for Message {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        // Type code
        buf.put_u8(match *self {
            Message::Nop => 0,
            Message::Open => 1,
            Message::Close => 2,
            Message::Text(_) => 10,
            Message::Emo(_) => 11,
            Message::Image(_, _) => 12,
            Message::Compound(_) => 20
        });
        match *self {
            Message::Text(ref mut t) => t.encode_into(buf),
            Message::Emo(ref mut e) => e.encode_into(buf),
            Message::Image(t, ref d) => {
                buf.put_u8(t);
                buf.extend(d);
            },
            Message::Compound(_) => {
                panic!("Not implemented yet")
            },
            _ => {}
        }
    }
}
