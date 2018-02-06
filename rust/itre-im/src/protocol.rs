pub mod codec {
    use std;
    use bytes::BytesMut;
    use tokio_io::codec::{Encoder, Decoder};
    use itre::Message as ItreMessage;
    use itre::encoder::Encoder as ItreEncoder;
    use itre::decoder::Decoder as ItreDecoder;
    use itre::error::Error as ItreError;

    pub struct ItreCodec;

    pub enum ItreCodecError {
        ItreError(ItreError),
        IOError(std::io::Error)
    }
    pub type ItreCodecResult<T> = Result<T, ItreCodecError>;

    impl From<std::io::Error> for ItreCodecError {
        fn from(e: std::io::Error) -> Self {
            ItreCodecError::IOError(e)
        }
    }

    impl From<ItreError> for ItreCodecError {
        fn from(e: ItreError) -> Self {
            ItreCodecError::ItreError(e)
        }
    }

    impl Encoder for ItreCodec {
        type Item = ItreMessage;
        type Error = ItreCodecError;

        fn encode(&mut self, msg: Self::Item, buf: &mut BytesMut) -> ItreCodecResult<()> {
            msg.encode_into(buf);
            Ok(())
        }
    }

    impl Decoder for ItreCodec {
        type Item = ItreMessage;
        type Error = ItreCodecError;

        fn decode(&mut self, buf: &mut BytesMut) -> ItreCodecResult<Option<Self::Item>> {
            match Self::Item::decode_from(buf) {
                Ok(msg) => Ok(Some(msg)),
                Err(e) => Err(ItreCodecError::from(e))
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
    type BindTransport = Result<Self::Transport, codec::ItreCodecError>;

    fn bind_transport(&self, io: T) -> Self::BindTransport {
        Ok(io.framed(codec::ItreCodec));
    }
}