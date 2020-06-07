pub fn is_letter(ch: &char) -> bool {
    ch.is_ascii_alphabetic() || *ch == '_'
}

pub fn is_digit(ch: &char) -> bool {
    ch.is_numeric()
}
