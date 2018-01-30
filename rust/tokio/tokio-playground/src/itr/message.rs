/// ITRE (Itr Encoding) Version 0.1
/// 
/// ITRE is a variable-length encoding designed for use in instant messaging applications.
/// Basic format:
/// 
///     |Type code|Type specific encoding|
///     | 1 byte  | ...                  |
/// 
/// ITRE format categorizes messages into two types: control messages and ordinary messages.
/// The type code of control messages ranges from 0(0x0) ~ 127(0x7F)
/// The type code of ordinary messages ranges from 128(0x80) ~ 255(0xFF)
/// 
/// Control messages:
/// Nop (0(0x0)): Do nothing
/// 
/// Ordinary messages:
/// Text (128(0x80)): UTF-8 encoded text string
/// Emo (130(0x82)): Emoticon
/// Image (140(0x8C)): Image
/// Compound (250(0xFA)): Array of nested messages
/// 
/// Type-specific encoding:
/// Control messages do not bear extra information and thus require no type-specific encoding.
/// For ordinary messages:
///     Text: |Total text length|Text content|
///           | 4 bytes         | ...        |
///           However, if the total text length is greater than 4294967294 (2^32-2),
///           the encoding result will be:
///           | 2^32-1 | the type-specific encoding of the first 2^32-2 bytes
///           followed by:
///           | the type-specific encoding of the remaining text |
///           Note that the above rule is applied recursively.
/// 
///     Emo:  |Emo code|Emo-specific encoding|
///           | 1 byte | undefined |
///           Emo-specific encoding:
///             Nop (0(0x0))
///             Laugh (1(0x1))
///             Cry (2(0x2))
///             Custom(10(0xF0)): Reserved for future purpose
///     Image: TODO
///     Compound: |Length of nested messages|Encoding of nested messages|
///               | 1 byte                  | ... |
///               Same as text, if the total length of nested messages is greater than 254 (2^8-2),
///               the encoding result will be:
///               | 2^8-1 | the type-specific encoding of the first 2^8-2 messages
///               followed by:
///               | the type-specific encoding of the remaining messages as a compound message |
///               The above rule is also applied recursively.

use bytes::{BytesMut, BufMut};
use byteorder::{LittleEndian};

const TEXT_SLICE_MAX_LENGTH: u32 = 0xfffffffe;
const TEXT_SLICE_MAX_LENGTH_S: usize = TEXT_SLICE_MAX_LENGTH as usize;
const TEXT_OVERFLOW_FLAG: u32 = 0xffffffff;
const COMPOUND_SLICE_MAX_LENGTH: u8 = 0xfe;
const COMPOUND_SLICE_MAX_LENGTH_S: usize = COMPOUND_SLICE_MAX_LENGTH as usize;
const COMPOUND_OVERFLOW_FLAG: u8 = 0xff;

pub trait Encoder {
    fn encode_into(&mut self, buf: &mut BytesMut);
}

impl Encoder for str {
    fn encode_into(&mut self, buf: &mut BytesMut) {
        let len = self.len();
        if len > TEXT_SLICE_MAX_LENGTH_S {
            buf.put_u32::<LittleEndian>(TEXT_OVERFLOW_FLAG);
            buf.extend(self[..TEXT_SLICE_MAX_LENGTH_S-1].bytes());
            self[TEXT_SLICE_MAX_LENGTH_S..].encode_into(buf);
        } else {
            buf.put_u32::<LittleEndian>(self.len() as u32);
            buf.extend(self.as_bytes());
        }
    }
}

pub enum Emo {
    Nop,             // 0
    Laugh,           // 1
    Cry,             // 2
    Custom(Vec<u8>)  // 240
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
            &mut Emo::Custom(_) => {
                panic!("Not implemented yet");
            }
        }
    }
}

pub enum Message {
    Nop,                    // 0
    Text(String),           // 128
    Emo(Emo),               // 130
    Image(u8, Vec<u8>),     // 140
    Compound(Vec<Message>)  // 250
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
                    buf.put_u8(COMPOUND_SLICE_MAX_LENGTH);
                    panic!("TODO");
                } else {
                    buf.put_u8(len as u8);
                    for mut msg in msgs {
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
    #[test]
    fn bitwise_op() {
        assert_eq!(2u32.pow(8), 256);
    }
}