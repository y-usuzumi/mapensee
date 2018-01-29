use std;
use super::error;

pub type Result<T> = std::result::Result<T, error::Error>;