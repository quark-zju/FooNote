//! i18n utilities

use std::fmt;
/// Describe a language id.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum LangId {
    Cn,
    En,
}

pub static mut CURRENT_LANG_ID: LangId = LangId::En;

#[macro_export]
macro_rules! t {
    (cn=$c:expr, en=$e:expr $(,)?) => {
        match unsafe { $crate::lang::CURRENT_LANG_ID } {
            $crate::lang::LangId::Cn => $c,
            $crate::lang::LangId::En => $e,
        }
    };

    (cn=$c:literal, en=$e:literal, $($args:tt)*) => {
        match unsafe { $crate::lang::CURRENT_LANG_ID } {
            $crate::lang::LangId::Cn => format!($c, $($args)*),
            $crate::lang::LangId::En => format!($e, $($args)*),
        }
    };
}

impl fmt::Display for LangId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            LangId::Cn => "cn",
            LangId::En => "en",
        };
        f.write_str(name)
    }
}
