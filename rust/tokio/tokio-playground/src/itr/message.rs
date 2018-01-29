use bytes::{BytesMut, BufMut};

trait Encoder {
    fn encode_into(&mut self, buf: &mut BytesMut);
}

pub enum Emo {
    Nop,             // 0
    Laugh,           // 1
    Cry,             // 2
    Custom(Vec<u8>)  // 10
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
            Emo::Custom(d) => {
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

impl Message {
    pub fn header_code(&self) -> u8 {
        match *self {
            Message::Nop => 0,
            Message::Open => 1,
            Message::Close => 2,
            Message::Text(_) => 10,
            Message::Emo(_) => 11,
            Message::Image(_, _) => 12,
            Message::Compound(_) => 20
        }
    }
}