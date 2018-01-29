use bytes::BytesMut;
use tokio_io::codec::{Encoder, Decoder};
use super::message::{Encoder as MessageEncoder, Message};
use super::error;
use super::result;

pub struct ItrCodec;

impl Encoder for ItrCodec {
    type Item = Message;
    type Error = error::Error;
    fn encode(&mut self, mut item: Self::Item, buf: &mut BytesMut) -> result::Result<()> {
        item.encode_into(buf);
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
