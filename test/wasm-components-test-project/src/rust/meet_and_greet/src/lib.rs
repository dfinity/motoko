wit_bindgen::generate!({
    path: "meet_and_greet.wit",
    world: "meet-and-greet",
});

struct MeetAndGreet;
export!(MeetAndGreet);

use crate::exports::api::Guest;
use crate::exports::api::C;
use crate::exports::api::V1;

impl Guest for MeetAndGreet {
    // Basic greeting functionality.
    fn say_hello(guest_name: String) -> String {
        format!("Hello {}!", guest_name)
    }

    fn say_bye(guest_name: String, formal: bool) -> String {
        if formal {
            format!("Goodbye {}!", guest_name)
        } else {
            format!("Bye {}!", guest_name)
        }
    }

    // Various primitive types in arguments and return values.
    fn concat0() -> Vec<u8> {
        "concat0".into()
    }
    fn concat2(a: Vec<u8>, b: Vec<u8>) -> Vec<u8> {
        format!(
            "concat2: {} {}",
            String::from_utf8_lossy(&a),
            String::from_utf8_lossy(&b)
        )
        .into()
    }

    fn prim_bool(a: bool) -> bool {
        a
    }
    fn prim_char(a: char) -> char {
        a
    }
    fn prim_u8(a: u8) -> u8 {
        a
    }
    fn prim_u16(a: u16) -> u16 {
        a
    }
    fn prim_u32(a: u32) -> u32 {
        a
    }
    fn prim_u64(a: u64) -> u64 {
        a
    }
    fn prim_i8(a: i8) -> i8 {
        a
    }
    fn prim_i16(a: i16) -> i16 {
        a
    }
    fn prim_i32(a: i32) -> i32 {
        a
    }
    fn prim_i64(a: i64) -> i64 {
        a
    }
    fn prim_f64(a: f64) -> f64 {
        a
    }
    fn prim_string_in(a: String) -> i32 {
        a.len() as i32
    }
    fn prim_string_out() -> String {
        "Hello; emoji: ☃❄🌨; FooBär☃!".to_string()
    }
    fn prim_string(a: String) -> String {
        a + "!"
    }

    fn vec_u16(a: Vec<u16>) -> String {
        format!(
            "vec_u16: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_string(a: Vec<String>) -> String {
        format!(
            "vec_string: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_i8(a: Vec<i8>) -> String {
        format!(
            "vec_i8: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_u8_as_blob(a: Vec<u8>) -> String {
        format!(
            "vec_u8_as_blob: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_u32(a: Vec<u32>) -> String {
        format!(
            "vec_u32: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_i32(a: Vec<i32>) -> String {
        format!(
            "vec_i32: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_i64(a: Vec<i64>) -> String {
        format!(
            "vec_i64: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_bool(a: Vec<bool>) -> String {
        format!(
            "vec_bool: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn vec_string_nested(a: Vec<Vec<String>>) -> String {
        let inner = a
            .iter()
            .map(|inner| inner.join("|"))
            .collect::<Vec<String>>()
            .join("; ");
        format!("vec_string_nested: {}", inner)
    }
    fn vec_char(a: Vec<char>) -> String {
        let s: String = a.into_iter().collect();
        format!("vec_char: {}", s)
    }
    fn vec_f64(a: Vec<f64>) -> String {
        format!(
            "vec_f64: {}",
            a.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }

    fn to_vec_bool(i: i32, b: bool) -> Vec<bool> {
        let mut v = Vec::new();
        v.push(i > 0);
        v.push(!b);
        v.push(true);
        v.push(false);
        v
    }
    fn to_vec_char(u: u8, c: char) -> Vec<char> {
        let mut v = Vec::new();
        v.push((u / 2u8) as char);
        v.push(c);
        v
    }
    fn to_vec_i8(u: u16, c: char) -> Vec<i8> {
        let mut v = Vec::new();
        v.push((u / 2u16) as i8);
        v.push(c as i8);
        v
    }
    fn to_vec_u8_as_blob(u: u16, c: char) -> Vec<u8> {
        let mut v = Vec::new();
        v.push((u / 2u16) as u8);
        v.push(c as u8);
        v
    }
    fn to_vec_i16(u: u16, i: i16) -> Vec<i16> {
        let mut v = Vec::new();
        v.push((u / 2u16) as i16);
        v.push(i / 2);
        v
    }
    fn to_vec_u32(u: u16, i: u32) -> Vec<u32> {
        let mut v = Vec::new();
        v.push((u / 2u16) as u32);
        v.push(i / 2);
        v
    }
    fn to_vec_i64(u: u16, i: i64) -> Vec<i64> {
        let mut v = Vec::new();
        v.push((u / 2u16) as i64);
        v.push(i / 2);
        v
    }
    fn to_vec_f64(u: u16, f: f64) -> Vec<f64> {
        let mut v = Vec::new();
        v.push((u / 2u16) as f64);
        v.push(f / 2.0);
        v
    }
    fn to_vec_string(s1: String, s2: String) -> Vec<String> {
        let mut v = Vec::new();
        v.push(s1 + "!");
        v.push(s2 + "!");
        v
    }
    fn to_vec_vec_simple() -> Vec<Vec<u64>> {
        let mut v = Vec::new();
        v.push(vec![0, 0]);
        v.push(vec![1, 1]);
        v
    }
    fn to_vec_vec_u64(u: u64) -> Vec<Vec<u64>> {
        let mut v = Vec::new();
        v.push(vec![u, u]);
        v.push(vec![u + 1, u + 1]);
        v
    }
    fn to_vec_vec(vec: Vec<u64>) -> Vec<Vec<u64>> {
        let mut v = Vec::new();
        v.push(vec.clone());
        v.push(vec.iter().map(|x| x + 1).collect());
        v
    }

    fn variant_in11(v: V1) -> String {
        match v {
            V1::Abc => "#abc".to_string(),
            V1::Def => "#def".to_string(),
            V1::Gh => "#gh".to_string(),
        }
    }
    fn variant_in12(v1: V1, v2: V1) -> String {
        format!("{}, {}", Self::variant_in11(v1), Self::variant_in11(v2))
    }
    fn variant_array_in(v: Vec<V1>) -> String {
        v.iter()
            .map(|x| Self::variant_in11(*x))
            .collect::<Vec<String>>()
            .join(", ")
    }
    fn variant_result_same_in(v: Result<u16, u16>) -> String {
        match v {
            Result::Ok(u) => format!("ok({})", u),
            Result::Err(u) => format!("err({})", u),
        }
    }
    fn variant_result_in(v: Result<u16, String>) -> String {
        match v {
            Result::Ok(u) => format!("ok({})", u),
            Result::Err(s) => format!("err({})", s),
        }
    }
    fn variant_string_in(v: C) -> String {
        match v {
            C::C(s) => format!("c({})", s),
        }
    }
    fn variant_array_result_same_in(v: Vec<Result<u16, u16>>) -> String {
        v.iter()
            .map(|x| match x {
                Result::Ok(u) => format!("ok({})", u),
                Result::Err(u) => format!("err({})", u),
            })
            .collect::<Vec<String>>()
            .join(", ")
    }
    fn variant_array_result_in(v: Vec<Result<u16, String>>) -> String {
        v.iter()
            .map(|x| match x {
                Result::Ok(u) => format!("ok({})", u),
                Result::Err(s) => format!("err({})", s),
            })
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn variant11(v: V1) -> V1 {
        match v {
            V1::Abc => V1::Def,
            V1::Def => V1::Gh,
            V1::Gh => V1::Abc,
        }
    }
    fn variant12(v1: V1, v2: V1) -> V1 {
        match v1 {
            V1::Abc => Self::variant11(v2),
            V1::Def => Self::variant11(Self::variant11(v2)),
            V1::Gh => Self::variant11(Self::variant11(Self::variant11(v2))),
        }
    }
    fn variant_array(v: Vec<V1>) -> Vec<V1> {
        v.into_iter().map(Self::variant11).collect()
    }
    fn variant_result_same(v: Result<u16, u16>) -> Result<u16, u16> {
        match v {
            Result::Ok(u) => Result::Ok(u + 1),
            Result::Err(u) => Result::Err(u + 1),
        }
    }
    fn variant_result(v: Result<u16, String>) -> Result<u16, String> {
        match v {
            Result::Ok(u) => Result::Ok(u + 1),
            Result::Err(s) => Result::Err(format!("{}!", s)),
        }
    }
    fn variant_string(v: C) -> C {
        match v {
            C::C(s) => C::C(format!("{}!", s)),
        }
    }
    fn variant_array_result_same(v: Vec<Result<u16, u16>>) -> Vec<Result<u16, u16>> {
        v.into_iter().map(Self::variant_result_same).collect()
    }
    fn variant_array_result(v: Vec<Result<u16, String>>) -> Vec<Result<u16, String>> {
        v.into_iter().map(Self::variant_result).collect()
    }

    fn nested_variant1(v: Result<Result<C, String>, V1>) -> Result<Result<C, String>, V1> {
        match v {
            Result::Ok(Result::Ok(v)) => Result::Ok(Result::Ok(Self::variant_string(v))),
            Result::Ok(Result::Err(s)) => Result::Ok(Result::Err(format!("{}!", s))),
            Result::Err(v) => Result::Err(Self::variant11(v)),
        }
    }
    fn nested_variant2(v: Result<Result<V1, String>, C>) -> Result<Result<V1, String>, C> {
        match v {
            Result::Ok(Result::Ok(v)) => Result::Ok(Result::Ok(Self::variant11(v))),
            Result::Ok(Result::Err(s)) => Result::Ok(Result::Err(format!("{}!", s))),
            Result::Err(v) => Result::Err(Self::variant_string(v)),
        }
    }

    fn option_string(v: Option<String>) -> Option<String> {
        match v {
            Some(s) => Some(format!("{}!", s)),
            None => None,
        }
    }
    fn options_array(v: Option<Vec<Option<String>>>) -> Option<Vec<Option<String>>> {
        match v {
            Some(v) => Some(
                v.into_iter()
                    .map(|x| x.map(|s| format!("{}!", s)))
                    .collect(),
            ),
            None => None,
        }
    }

    fn tuple_string_u64(v: (String, u64)) -> (String, u64) {
        (format!("{}!", v.0), v.1 + 1)
    }
    fn tuple_variant_array_result(
        v: (V1, Vec<String>, Result<u16, String>),
    ) -> (V1, Vec<String>, Result<u16, String>) {
        (
            Self::variant11(v.0),
            v.1.iter().map(|s| format!("{}!", s)).collect(),
            Self::variant_result(v.2),
        )
    }
    fn tuples_nested1(v1: (bool, (u8, u16)), v2: ((u8, u16), u32)) -> ((bool, u32), (u8, u32)) {
        let (b, (x1, x2)) = v1;
        let ((y, z1), z2) = v2;
        ((!b, x1 as u32 + x2 as u32), (y + 1, z1 as u32 + z2))
    }
    fn tuples_nested(v1: (bool, (u8, u16)), v2: ((u8, u16), u64)) -> ((bool, u32), (u8, u64)) {
        let (b, (x1, x2)) = v1;
        let ((y, z1), z2) = v2;
        ((!b, x1 as u32 + x2 as u32), (y + 1, z1 as u64 + z2))
    }

    fn unit() {}
    fn unit_result(v: Result<(), ()>) -> Result<(), ()> {
        match v {
            Result::Ok(()) => Result::Ok(()),
            Result::Err(()) => Result::Ok(()),
        }
    }
    fn unit_result_er(v: Result<(), String>) -> Result<(), String> {
        match v {
            Result::Ok(()) => Result::Ok(()),
            Result::Err(s) => Result::Err(format!("{}!", s)),
        }
    }
    fn unit_result_ok(v: Result<String, ()>) -> Result<String, ()> {
        match v {
            Result::Ok(s) => Result::Ok(format!("{}!", s)),
            Result::Err(()) => Result::Err(()),
        }
    }
}
