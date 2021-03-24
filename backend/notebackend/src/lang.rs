//! i18n utilities

use notebackend_types::log;
use std::fmt;
/// Describe a language id.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum LangId {
    Cn,
    En,
}

pub static mut CURRENT_LANG_ID: LangId = LangId::En;

impl LangId {
    /// Get language Id from system locale.
    pub fn from_system() -> Self {
        let loc = locale_config::Locale::user_default();
        let loc_str = loc.to_string().to_lowercase();
        let lang = if loc_str.contains("cn") || loc_str.starts_with("zh") {
            LangId::Cn
        } else {
            LangId::En
        };
        log::info!("locale: {} => {}", &loc, &lang);
        lang
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lang_from_system() {
        let _ = LangId::from_system();
    }
}
