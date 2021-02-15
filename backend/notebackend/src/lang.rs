//! i18n utilities

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
    (cn: $c:expr, en: $e:expr,) => {
        match unsafe { $crate::lang::CURRENT_LANG_ID } {
            $crate::lang::LangId::Cn => $c,
            $crate::lang::LangId::En => $e,
        }
    };
}
