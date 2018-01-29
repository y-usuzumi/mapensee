use bytes::{BytesMut, BufMut};
use byteorder::{LittleEndian};
use tokio_io::codec::{Encoder, Decoder};
use super::message::{Message, Emo};
use super::error;
use super::result;

pub struct ItrCodec;

impl Emo {
    fn encode(&self) -> &[u8] {
        panic!("Not implemented")
    }
}

impl Encoder for ItrCodec {
    type Item = Message;
    type Error = error::Error;
    fn encode(&mut self, item: Self::Item, buf: &mut BytesMut) -> result::Result<()> {
        buf.put(item.header_code());
        match item {
            // Control messages do not have length
            Message::Nop | Message::Open | Message::Close => {
                buf.put_u8(0);
            },
            Message::Text(s) => {
                // FIXME: length could be greater than u32
                buf.put_u32::<LittleEndian>(s.len() as u32);
            },
            Message::Emo(e) => {
                buf.put_u8(e.header_code());
                buf.put(e.encode());
            },
            Message::Image(t, d) => {
                buf.put_u8(t);
            }
        };
        Ok(())
    }
}

impl Decoder for ItrCodec {
    type Item = Message;
    type Error = error::Error;
    fn decode(&mut self, buf: &mut BytesMut) -> result::Result<Option<Self::Item>> {
        Ok(Some(Message::Close))
    }
}