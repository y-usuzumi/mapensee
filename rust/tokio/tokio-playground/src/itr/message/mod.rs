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

const TEXT_SLICE_MAX_LENGTH: u32 = 0xfffffffe;
const TEXT_SLICE_MAX_LENGTH_S: usize = TEXT_SLICE_MAX_LENGTH as usize;
const TEXT_OVERFLOW_FLAG: u32 = 0xffffffff;
const COMPOUND_SLICE_MAX_LENGTH: u8 = 0xfe;
const COMPOUND_SLICE_MAX_LENGTH_S: usize = COMPOUND_SLICE_MAX_LENGTH as usize;
const COMPOUND_OVERFLOW_FLAG: u8 = 0xff;

mod encoder;
mod decoder;
pub mod error;

pub use self::encoder::Encoder;
pub use self::decoder::Decoder;

#[derive(Debug)]
pub enum Emo {
    Nop,             // 0
    Laugh,           // 1
    Cry,             // 2
    Custom(Vec<u8>)  // 240
}

#[derive(Debug)]
pub enum Message {
    Nop,                    // 0
    Text(String),           // 128
    Emo(Emo),               // 130
    Image(u8, Vec<u8>),     // 140
    Compound(Vec<Message>)  // 250
}
