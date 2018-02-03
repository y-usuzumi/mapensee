pub const TEXT_SLICE_MAX_LENGTH: u32             = 0xfffffffe;
pub const TEXT_SLICE_MAX_LENGTH_S: usize         = TEXT_SLICE_MAX_LENGTH as usize;
pub const TEXT_OVERFLOW_FLAG: u32                = 0xffffffff;
pub const COMPOUND_SLICE_MAX_LENGTH: u8          = 0xfe;
pub const COMPOUND_SLICE_MAX_LENGTH_S: usize     = COMPOUND_SLICE_MAX_LENGTH as usize;
pub const COMPOUND_OVERFLOW_FLAG: u8             = 0xff;

pub const MESSAGE_NOP_TYPE_CODE: u8              = 0;
pub const MESSAGE_TEXT_TYPE_CODE: u8             = 128;
pub const MESSAGE_EMO_TYPE_CODE: u8              = 130;
pub const MESSAGE_IMAGE_TYPE_CODE: u8            = 140;
pub const MESSAGE_COMPOUND_TYPE_CODE: u8         = 250;