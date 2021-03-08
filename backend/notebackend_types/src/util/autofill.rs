use std::convert::TryFrom;
use std::convert::TryInto;

/// Suggest the next item for a sequence.
pub(crate) fn autofill(seq: &[&str]) -> Option<String> {
    if seq.is_empty() {
        return None;
    }
    // Try without common prefix / suffix.
    if let Some(predict) = autofill_no_prefix_suffix(seq) {
        log::debug!("autofill {:?} => {:?}", seq, &predict);
        return Some(predict);
    }
    // Remove common prefix / suffix and try again.
    if seq.len() == 1 {
        return None;
    }
    let prefix_len = common_prefix_len(seq);
    let suffix_len = common_suffix_len(seq);
    if prefix_len + suffix_len >= seq.iter().map(|s| s.len()).min().unwrap() {
        // A short item has overlapped prefix + suffix.
        return None;
    }
    let short_seq = seq
        .iter()
        .map(|s| &s[prefix_len..(s.len() - suffix_len)])
        .collect::<Vec<_>>();

    // Run autofill on the stripped sequence.
    autofill_no_prefix_suffix(&short_seq).map(|s| {
        let prefix = &seq[0][0..prefix_len];
        let suffix = &seq[0][seq[0].len() - suffix_len..];
        let result = format!("{}{}{}", prefix, s, suffix);
        log::debug!("autofill {:?} => {:?}", seq, &result);
        result
    })
}

fn autofill_no_prefix_suffix(seq: &[&str]) -> Option<String> {
    const WORD_SEQ_LIST: &[&[&str]] = &[
        &["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
        &[
            "Sunday",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday",
            "Sunday",
        ],
        &[
            "周日", "周一", "周二", "周三", "周四", "周五", "周六", "周日",
        ],
        &[
            "星期日",
            "星期一",
            "星期二",
            "星期三",
            "星期四",
            "星期五",
            "星期六",
            "星期日",
        ],
        &[
            "日曜日",
            "月曜日",
            "火曜日",
            "水曜日",
            "木曜日",
            "金曜日",
            "土曜日",
            "日曜日",
        ],
        &[
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
        ],
        &[
            "January",
            "February",
            "March",
            "April",
            "May",
            "Jun",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December",
        ],
        &[
            "一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "十一", "十二",
        ],
        &["壹", "贰", "叁", "肆", "伍", "陆", "柒", "捌", "玖", "拾"],
        &["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"],
        &[
            "子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥",
        ],
    ];

    IntSeq
        .predict(seq)
        .or_else(|| CharSeq.predict(seq))
        .or_else(|| WORD_SEQ_LIST.iter().filter_map(|l| l.predict(seq)).next())
}
trait Sequence {
    /// Distance of 2 values represented in strings.
    fn distance(&self, a: &str, b: &str) -> Option<i64>;

    /// Figure out b, so distance(a, b) is d.
    fn apply_distance(&self, a: &str, d: i64) -> Option<String>;

    /// Minimal predict length.
    fn min_predict_length(&self) -> usize {
        2
    }

    /// Default distance for single-item sequence.
    fn default_delta(&self) -> i64 {
        1
    }

    /// Predict the next item.
    fn predict(&self, seq: &[&str]) -> Option<String> {
        if seq.len() < self.min_predict_length() {
            return None;
        }
        if let Some(mut deltas) = (1..seq.len())
            .map(|i| self.distance(seq[i - 1], seq[i]))
            .collect::<Option<Vec<i64>>>()
        {
            if deltas.is_empty() {
                deltas.push(self.default_delta());
            }
            deltas.dedup();
            if deltas.len() == 1 {
                return self.apply_distance(seq.last().unwrap(), deltas[0]);
            }
        }
        None
    }
}

struct IntSeq;
impl Sequence for IntSeq {
    fn distance(&self, a: &str, b: &str) -> Option<i64> {
        if let (Some(a), Some(b)) = (a.parse::<i64>().ok(), b.parse::<i64>().ok()) {
            b.checked_sub(a)
        } else {
            None
        }
    }

    fn apply_distance(&self, a: &str, b: i64) -> Option<String> {
        a.parse::<i64>()
            .ok()
            .and_then(|i| i.checked_add(b))
            .map(|i| i.to_string())
    }

    fn min_predict_length(&self) -> usize {
        1
    }
}

struct CharSeq;
impl Sequence for CharSeq {
    fn distance(&self, a: &str, b: &str) -> Option<i64> {
        let a = a.chars().take(2).collect::<Vec<_>>();
        let b = b.chars().take(2).collect::<Vec<_>>();
        if a.len() == 1 && b.len() == 1 {
            // Only support abs(distance) <= 1.
            let d = (b[0] as i64) - (a[0] as i64);
            if d.abs() <= 1 {
                Some(d)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn apply_distance(&self, a: &str, d: i64) -> Option<String> {
        let a = a.chars().take(2).collect::<Vec<_>>();
        if d.abs() <= 1 && a.len() == 1 {
            (a[0] as i64)
                .checked_add(d)
                .and_then(|i| u32::try_from(i).ok())
                .and_then(|i| std::char::from_u32(i))
                .map(|c| c.to_string())
        } else {
            None
        }
    }
}

impl Sequence for [&'static str] {
    fn distance(&self, a: &str, b: &str) -> Option<i64> {
        let a = self.iter().position(|s| *s == a)?;
        let b = self.iter().position(|s| *s == b)?;
        (b as i64).checked_sub(a as i64)
    }

    fn apply_distance(&self, a: &str, d: i64) -> Option<String> {
        let a = self.iter().position(|s| *s == a)?;
        let i: usize = i64::try_from(a).ok()?.checked_add(d)?.try_into().ok()?;
        let result = self.get(i).map(|s| s.to_string());
        if result.as_ref().map(|s| s.as_str()) == Some("") {
            None
        } else {
            result
        }
    }

    fn min_predict_length(&self) -> usize {
        1
    }
}

/// Common prefix length, in utf-8 bytes.
fn common_prefix_len(seq: &[&str]) -> usize {
    if seq.is_empty() {
        0
    } else {
        let mut iters = seq.iter().map(|s| s.chars()).collect::<Vec<_>>();
        let (first, rest) = iters.split_first_mut().unwrap();
        first
            .take_while(|&c| rest.iter_mut().all(|r| r.next() == Some(c)))
            .map(|c| c.len_utf8())
            .sum()
    }
}

/// Common suffix length, in utf-8 bytes.
fn common_suffix_len(seq: &[&str]) -> usize {
    if seq.is_empty() {
        0
    } else {
        let mut iters = seq.iter().map(|s| s.chars().rev()).collect::<Vec<_>>();
        let (first, rest) = iters.split_first_mut().unwrap();
        first
            .take_while(|&c| rest.iter_mut().all(|r| r.next() == Some(c)))
            .map(|c| c.len_utf8())
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::util::autofill::*;

    #[test]
    fn test_common_prefix_len() {
        assert_eq!(common_prefix_len(&[]), 0);
        assert_eq!(common_prefix_len(&["abc"]), 3);
        assert_eq!(common_prefix_len(&["112", "113", "111"]), 2);
        assert_eq!(common_prefix_len(&["11", "111", "1112"]), 2);
        assert_eq!(common_prefix_len(&["11", "111", "2"]), 0);
        assert_eq!(common_prefix_len(&["前缀11", "前缀12"]), 7);
    }

    #[test]
    fn test_common_suffix_len() {
        assert_eq!(common_suffix_len(&[]), 0);
        assert_eq!(common_suffix_len(&["abc"]), 3);
        assert_eq!(common_suffix_len(&["112", "113", "111"]), 0);
        assert_eq!(common_suffix_len(&["211", "311", "111"]), 2);
        assert_eq!(common_suffix_len(&["11", "111", "2111"]), 2);
        assert_eq!(common_suffix_len(&["11", "111", "2"]), 0);
        assert_eq!(common_suffix_len(&["后缀", "新后缀"]), 6);
    }

    #[test]
    fn test_int_sequence() {
        assert_eq!(IntSeq.predict(&[]), None);
        assert_eq!(IntSeq.predict(&["10"]), Some("11".to_string()));
        assert_eq!(IntSeq.predict(&["10a"]), None);
        assert_eq!(IntSeq.predict(&["0", "1", "2"]), Some("3".to_string()));
        assert_eq!(IntSeq.predict(&["1", "1", "1"]), Some("1".to_string()));
        assert_eq!(IntSeq.predict(&["10", "9", "8"]), Some("7".to_string()));
        assert_eq!(IntSeq.predict(&["10", "9", "7"]), None);
        assert_eq!(IntSeq.predict(&["10", "5", "0"]), Some("-5".to_string()));
    }

    #[test]
    fn test_char_sequence() {
        assert_eq!(CharSeq.predict(&[]), None);
        assert_eq!(CharSeq.predict(&["a"]), None);
        assert_eq!(CharSeq.predict(&["a", "b", "c"]), Some("d".to_string()));
        assert_eq!(CharSeq.predict(&["A", "B"]), Some("C".to_string()));
        assert_eq!(CharSeq.predict(&["A", "C"]), None);
        assert_eq!(CharSeq.predict(&["z", "y"]), Some("x".to_string()));
        assert_eq!(CharSeq.predict(&["㊄", "㊅"]), Some("㊆".to_string()));
    }

    #[test]
    fn test_slice_sequence() {
        let seq = ["I", "II", "III", "IV", "V", "VI", "VII"];
        assert_eq!(seq.predict(&["III"]), Some("IV".to_string()));
        assert_eq!(seq.predict(&["III", "IV"]), Some("V".to_string()));
    }

    #[test]
    fn test_autofill() {
        assert_eq!(autofill(&["a.01", "a.02"]), Some("a.03".to_string()));
    }
}
