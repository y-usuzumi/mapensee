pub const TEXT_SLICE_MAX_LENGTH: u32             = 0xfffffffe;
pub const TEXT_SLICE_MAX_LENGTH_S: usize         = TEXT_SLICE_MAX_LENGTH as usize;
pub const TEXT_OVERFLOW_FLAG: u32                = 0xffffffff;
pub const COMPOUND_SLICE_MAX_LENGTH: u8          = 0xfe;
pub const COMPOUND_SLICE_MAX_LENGTH_S: usize     = COMPOUND_SLICE_MAX_LENGTH as usize;
pub const COMPOUND_OVERFLOW_FLAG: u8             = 0xff;

pub const MESSAGE_TYPE_CODE_NOP: u8              = 0;
pub const MESSAGE_TYPE_CODE_TEXT: u8             = 128;
pub const MESSAGE_TYPE_CODE_EMO: u8              = 130;
pub const MESSAGE_TYPE_CODE_IMAGE: u8            = 140;
pub const MESSAGE_TYPE_CODE_COMPOUND: u8         = 250;

pub const MESSAGE_EMO_CODE_NOP: u8               = 0;
pub const MESSAGE_EMO_CODE_LAUGH: u8             = 1;
pub const MESSAGE_EMO_CODE_CRY: u8               = 2;