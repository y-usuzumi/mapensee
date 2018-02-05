//! ITRE (Itr Encoding) Version 0.1
//!
//! ITRE is a variable-length encoding designed for use in instant messaging applications.
//! Basic format:
//! <pre>
//!     |Type code|Type specific encoding|
//!     | 1 byte  | ...                  |
//! </pre>
//!
//! ITRE format categorizes messages into two types: control messages and ordinary messages.
//! The type code of control messages ranges from 0(0x0) ~ 127(0x7F)
//! The type code of ordinary messages ranges from 128(0x80) ~ 255(0xFF)
//!
//! Control messages:
//! Nop (0(0x0)): Do nothing
//!
//! Ordinary messages:
//! Text (128(0x80)): UTF-8 encoded text string
//! Emo (130(0x82)): Emoticon
//! Image (140(0x8C)): Image
//! Compound (250(0xFA)): Array of nested messages
//!
//! Type-specific encoding:
//! Control messages do not bear extra information and thus require no type-specific encoding.
//! For ordinary messages:
//!   Text:
//!     <pre>
//!         |Total text length|Text content|
//!         | 2 bytes         | ...        |
//!     </pre>
//!     However, if the total text length is greater than 4294967294 (2^32-2),
//!     the encoding result will be:
//!     <pre>
//!         | 2^32-1 | the type-specific encoding of the first 2^32-2 bytes
//!     </pre>
//!     followed by:
//!     <pre>
//!         | the type-specific encoding of the remaining text |
//!     Note that the above rule is applied recursively.
//!
//!   Emo:
//!     <pre>
//!         |Emo code|Emo-specific encoding|
//!         | 1 byte | undefined |
//!     </pre>
//!     Emo-specific encoding:
//!       Nop (0(0x0))
//!       Laugh (1(0x1))
//!       Cry (2(0x2))
//!       Custom(10(0xF0)): Reserved for future purpose
//!   Image: TODO
//!   Compound:
//!     <pre>
//!         |Length of nested messages|Encoding of nested messages|
//!         | 1 byte                  | ... |
//!     </pre>
//!     Same as text, if the total length of nested messages is greater than 254 (2^8-2),
//!     the encoding result will be:
//!     <pre>
//!         | 2^8-1 | the type-specific encoding of the first 2^8-2 messages
//!     </pre>
//!     followed by:
//!     <pre>
//!         | the type-specific encoding of the remaining messages as a compound message |
//!     </pre>
//!     The above rule is also applied recursively.

mod encoder;
mod decoder;
pub mod error;
mod consts;

pub use self::encoder::Encoder;
pub use self::decoder::Decoder;

#[derive(Debug, PartialEq)]
pub enum Emo {
    Nop,             // 0
    Laugh,           // 1
    Cry,             // 2
    Custom(Vec<u8>)  // 240
}

#[derive(Debug, PartialEq)]
pub enum Message {
    Nop,                    // 0
    Text(String),           // 128
    Emo(Emo),               // 130
    Image(u8, Vec<u8>),     // 140
    Compound(Vec<Message>)  // 250
}
