pub mod codec {
    use std;
    use bytes::BytesMut;
    use tokio_io::codec::{Encoder, Decoder};
    use itre::Message as ItreMessage;
    use itre::encoder::Encoder as ItreEncoder;
    use itre::decoder::Decoder as ItreDecoder;

    pub struct ItreCodec;

    impl Encoder for ItreCodec {
        type Item = ItreMessage;
        type Error = std::io::Error;

        fn encode(&mut self, msg: Self::Item, buf: &mut BytesMut) -> std::io::Result<()> {
            msg.encode_into(buf);
            Ok(())
        }
    }

    impl Decoder for ItreCodec {
        type Item = ItreMessage;
        type Error = std::io::Error;

        fn decode(&mut self, buf: &mut BytesMut) -> std::io::Result<Option<Self::Item>> {
            match Self::Item::decode_from(buf) {
                Ok(msg) => Ok(Some(msg)),
                Err(_) => panic!("Not implemented yet")
            }
        }
    }
}

use std;
use tokio_proto::pipeline::ServerProto;
use tokio_io::{AsyncRead, AsyncWrite};
use tokio_io::codec::Framed;
use itre::Message as ItreMessage;

pub struct ItreProto;

impl<T: AsyncRead + AsyncWrite + 'static> ServerProto<T> for ItreProto {
    type Request = ItreMessage;
    type Response = ItreMessage;
    type Transport = Framed<T, codec::ItreCodec>;
    type BindTransport = Result<Self::Transport, std::io::Error>;

    fn bind_transport(&self, io: T) -> Self::BindTransport {
        Ok(io.framed(codec::ItreCodec))
    }
}
