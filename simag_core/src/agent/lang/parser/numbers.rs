use std::str::FromStr;

use super::*;
use nom::character::is_digit;

// number = -?[0-9\.]+
#[derive(Debug, PartialEq, Clone, Copy)]
pub(in crate::agent) enum Number {
    SignedFloat(f32),
    UnsignedFloat(f32),
    SignedInteger(i32),
    UnsignedInteger(u32),
}

pub(super) fn number(input: &[u8]) -> IResult<&[u8], Number> {
    let mut float = false;
    let mut idx = 0_usize;
    let rest = if (input[0] == b'-') | (input[0] == b'+') {
        &input[1..]
    } else {
        input
    };
    for (x, c) in rest.iter().enumerate() {
        if is_digit(*c) | (*c == b'.') {
            if *c == b'.' {
                float = true;
            }
            idx = x + 1;
        } else if idx > 0 {
            break;
        } else {
            return Err(nom::Err::Error(ParseErrB::NotNumber(input)));
        }
    }
    if float && (input[0] == b'-') {
        Ok((
            &input[idx + 1..],
            Number::SignedFloat(
                <f32>::from_str(str::from_utf8(&input[0..=idx]).expect("should be valid utf8"))
                    .unwrap(),
            ),
        ))
    } else if !float && (input[0] == b'-') {
        Ok((
            &input[idx + 1..],
            Number::SignedInteger(
                <i32>::from_str(str::from_utf8(&input[0..=idx]).expect("should be valid utf8"))
                    .unwrap(),
            ),
        ))
    } else if float {
        Ok((
            &input[idx..],
            Number::UnsignedFloat(
                <f32>::from_str(str::from_utf8(&input[0..idx]).expect("should be valid utf8"))
                    .unwrap(),
            ),
        ))
    } else {
        Ok((
            &input[idx..],
            Number::UnsignedInteger(
                <u32>::from_str(str::from_utf8(&input[0..idx]).expect("should be valid utf8"))
                    .unwrap(),
            ),
        ))
    }
}
