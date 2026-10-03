
use std::borrow::Cow;
use std::collections::HashMap;

use oxc_allocator::{Allocator, Vec as AVec};

use super::flatten::Flattened;
use crate::semantic::{BinOp, Expr, ExprList, ExprRef, OpcodeMap, UnaryOp, Value};

#[derive(Clone)]
enum Cv<'a> {
    Num(f64),
    Str(Cow<'a, str>),
    Bool(bool),
    Null,
    Undef,
    Window,
    Fn(&'a str),
    Builtin(&'static str),
}

pub fn run<'a>(alloc: &'a Allocator, ops: &OpcodeMap<'a>, flat: Flattened<'a>) -> Flattened<'a> {
    let mut f = Folder {
        alloc,
        ops,
        folds: 0,
        consts: HashMap::new(),
    };
    let mut stmts = flat.stmts;
    for st in stmts.iter_mut() {
        use super::flatten::Stmt;
        match st {
            Stmt::Bind(name, e) => {
                let (ne, cv) = f.fold(e);
                *e = ne;
                if let (Expr::Const(_), Some(cv)) = (ne, cv) {
                    f.consts.insert(name, (ne, cv));
                }
            }
            Stmt::Effect(e) => *e = f.fold(e).0,
        }
    }
    Flattened {
        stmts,
        unhandled: flat.unhandled,
        folds: f.folds,
        forces: flat.forces,
    }
}

struct Folder<'a, 'o> {
    alloc: &'a Allocator,
    ops: &'o OpcodeMap<'a>,
    folds: u32,
    consts: HashMap<&'a str, (ExprRef<'a>, Cv<'a>)>,
}

impl<'a, 'o> Folder<'a, 'o> {
    fn mk(&self, e: Expr<'a>) -> ExprRef<'a> {
        self.alloc.alloc(e)
    }

    fn fold_list(&mut self, items: &ExprList<'a>) -> ExprList<'a> {
        let mut l = AVec::with_capacity_in(items.len(), self.alloc);
        for el in items.iter() {
            l.push(self.fold(el).0);
        }
        l
    }

    fn fold(&mut self, e: ExprRef<'a>) -> (ExprRef<'a>, Option<Cv<'a>>) {
        match e {
            Expr::Const(v) => (e, cv_of_value(v)),
            Expr::Global(name) if *name == "window" => (e, Some(Cv::Window)),

            Expr::Local(name) => match self.consts.get(name) {
                Some((c, cv)) => (c, Some(cv.clone())),
                None => (e, None),
            },

            Expr::PartialApp {
                opcode,
                args,
                locals,
            } => {
                let nargs = self.fold_list(args);
                let mut nlocals = AVec::with_capacity_in(locals.len(), self.alloc);
                for (name, val) in locals.iter() {
                    nlocals.push((*name, self.fold(val).0));
                }
                let cv = self.fn_src(*opcode, args.len());
                (
                    self.mk(Expr::PartialApp {
                        opcode: *opcode,
                        args: nargs,
                        locals: nlocals,
                    }),
                    cv,
                )
            }

            Expr::BinOp(op, l, r) => {
                let (lf, lc) = self.fold(l);
                let (rf, rc) = self.fold(r);
                let cv = combine_bin(*op, lc.as_ref(), rc.as_ref());
                if let Some(lit) = cv.as_ref().and_then(|c| self.lit(c)) {
                    self.folds += 1;
                    return (lit, cv);
                }
                (self.mk(Expr::BinOp(*op, lf, rf)), cv)
            }

            Expr::UnaryOp(op, a) => {
                let (af, ac) = self.fold(a);
                let cv = combine_un(*op, ac.as_ref());
                if let Some(lit) = cv.as_ref().and_then(|c| self.lit(c)) {
                    self.folds += 1;
                    return (lit, cv);
                }
                (self.mk(Expr::UnaryOp(*op, af)), cv)
            }

            Expr::Index(o, p) => {
                let (of, oc) = self.fold(o);
                let (pf, pc) = self.fold(p);
                if let (Some(Cv::Str(s)), Some(idx)) =
                    (oc.as_ref(), pc.as_ref().and_then(num_index))
                {
                    if let Some(ch) = s.chars().nth(idx) {
                        self.folds += 1;
                        let owned = self.alloc.alloc_str(&ch.to_string());
                        let lit = self.mk(Expr::Const(Value::String(owned)));
                        return (lit, Some(Cv::Str(Cow::Borrowed(owned))));
                    }
                }
                if let (Some(obj), Some(Cv::Str(key))) = (oc.as_ref(), pc.as_ref()) {
                    if let Some(b) = builtin_member(obj, key) {
                        return (self.mk(Expr::Index(of, pf)), Some(Cv::Builtin(b)));
                    }
                }
                (self.mk(Expr::Index(of, pf)), None)
            }

            Expr::Cond { test, then, alt } => {
                let (tf, tc) = self.fold(test);
                let (thf, _) = self.fold(then);
                let (af, _) = self.fold(alt);
                if let Some(b) = tc.as_ref().map(truthy) {
                    self.folds += 1;
                    return (if b { thf } else { af }, None);
                }
                (
                    self.mk(Expr::Cond {
                        test: tf,
                        then: thf,
                        alt: af,
                    }),
                    None,
                )
            }

            Expr::Force(a) => {
                let af = self.fold(a).0;
                (self.mk(Expr::Force(af)), None)
            }
            Expr::Thunk(a) => {
                let af = self.fold(a).0;
                (self.mk(Expr::Thunk(af)), None)
            }
            Expr::Call(c, args) => {
                let (cf, cc) = self.fold(c);
                let mut nargs = AVec::with_capacity_in(args.len(), self.alloc);
                let mut argvs: Vec<Option<Cv<'a>>> = Vec::with_capacity(args.len());
                for el in args.iter() {
                    let (ef, ev) = self.fold(el);
                    nargs.push(ef);
                    argvs.push(ev);
                }
                if let Some(Cv::Builtin(name)) = cc {
                    if let Some(cv) = call_builtin(name, &argvs) {
                        if let Some(lit) = self.lit(&cv) {
                            self.folds += 1;
                            return (lit, Some(cv));
                        }
                    }
                }
                (self.mk(Expr::Call(cf, nargs)), None)
            }
            Expr::New(c, args) => {
                let cf = self.fold(c).0;
                let nargs = self.fold_list(args);
                (self.mk(Expr::New(cf, nargs)), None)
            }
            Expr::Apply(name, args) => {
                let nargs = self.fold_list(args);
                (self.mk(Expr::Apply(name, nargs)), None)
            }
            Expr::Assign(t, v) => {
                let tf = self.fold(t).0;
                let vf = self.fold(v).0;
                (self.mk(Expr::Assign(tf, vf)), None)
            }
            Expr::Seq(list) => {
                let l = self.fold_list(list);
                (self.mk(Expr::Seq(l)), None)
            }
            Expr::While { test, body } => {
                let test = self.fold(test).0;
                let body = self.fold(body).0;
                (self.mk(Expr::While { test, body }), None)
            }
            Expr::ForIn { iter, body } => {
                let iter = self.fold(iter).0;
                let body = self.fold(body).0;
                (self.mk(Expr::ForIn { iter, body }), None)
            }
            Expr::TryCatch {
                try_body,
                catch_body,
            } => {
                let try_body = self.fold(try_body).0;
                let catch_body = self.fold(catch_body).0;
                (
                    self.mk(Expr::TryCatch {
                        try_body,
                        catch_body,
                    }),
                    None,
                )
            }

            _ => (e, None),
        }
    }

    fn fn_src(&self, opcode: u32, nargs: usize) -> Option<Cv<'a>> {
        let op = self.ops.ops.get(&opcode)?;
        let levels = &op.source_levels;
        if levels.is_empty() {
            return None;
        }
        let idx = nargs.min(levels.len() - 1);
        Some(Cv::Fn(levels[idx]))
    }

    fn lit(&self, cv: &Cv<'a>) -> Option<ExprRef<'a>> {
        let v = match cv {
            Cv::Num(n) => Value::Number(*n),
            Cv::Bool(b) => Value::Bool(*b),
            Cv::Null => Value::Null,
            Cv::Undef => Value::Undefined,
            Cv::Str(s) => Value::String(self.alloc.alloc_str(s)),
            Cv::Window | Cv::Fn(_) | Cv::Builtin(_) => return None,
        };
        Some(self.mk(Expr::Const(v)))
    }
}

fn cv_of_value<'a>(v: &Value<'a>) -> Option<Cv<'a>> {
    Some(match v {
        Value::Number(n) => Cv::Num(*n),
        Value::Bool(b) => Cv::Bool(*b),
        Value::Null => Cv::Null,
        Value::Undefined => Cv::Undef,
        Value::String(s) => Cv::Str(Cow::Borrowed(s)),
        Value::Window => Cv::Window,
        Value::Fn(s) => Cv::Fn(s),
    })
}

fn to_primitive<'a>(cv: &Cv<'a>) -> Cv<'a> {
    match cv {
        Cv::Window => Cv::Str(Cow::Borrowed("[object Window]")),
        Cv::Fn(s) => Cv::Str(Cow::Borrowed(s)),
        other => other.clone(),
    }
}

fn is_str(cv: &Cv) -> bool {
    matches!(cv, Cv::Str(_))
}

fn to_string(cv: &Cv) -> String {
    match cv {
        Cv::Str(s) => s.to_string(),
        Cv::Bool(b) => (if *b { "true" } else { "false" }).to_string(),
        Cv::Null => "null".to_string(),
        Cv::Undef => "undefined".to_string(),
        Cv::Window => "[object Window]".to_string(),
        Cv::Fn(s) => s.to_string(),
        Cv::Builtin(_) => "function () { [native code] }".to_string(),
        Cv::Num(n) => num_to_string(*n),
    }
}

fn num_to_string(n: f64) -> String {
    if n.is_nan() {
        "NaN".to_string()
    } else if n.is_infinite() {
        if n > 0.0 { "Infinity" } else { "-Infinity" }.to_string()
    } else if n.fract() == 0.0 && n.abs() < 9e15 {
        format!("{}", n as i64)
    } else {
        format!("{n}")
    }
}

fn to_number(cv: &Cv) -> f64 {
    match cv {
        Cv::Num(n) => *n,
        Cv::Bool(b) => {
            if *b {
                1.0
            } else {
                0.0
            }
        }
        Cv::Null => 0.0,
        Cv::Undef => f64::NAN,
        Cv::Window | Cv::Fn(_) | Cv::Builtin(_) => f64::NAN,
        Cv::Str(s) => {
            let t = s.trim();
            if t.is_empty() {
                0.0
            } else {
                t.parse::<f64>().unwrap_or(f64::NAN)
            }
        }
    }
}

fn num_index(cv: &Cv) -> Option<usize> {
    match cv {
        Cv::Num(n) if *n >= 0.0 && n.fract() == 0.0 && n.is_finite() => Some(*n as usize),
        Cv::Bool(_) | Cv::Null => Some(to_number(cv) as usize),
        _ => None,
    }
}

fn truthy(cv: &Cv) -> bool {
    match cv {
        Cv::Num(n) => *n != 0.0 && !n.is_nan(),
        Cv::Str(s) => !s.is_empty(),
        Cv::Bool(b) => *b,
        Cv::Null | Cv::Undef => false,
        Cv::Window | Cv::Fn(_) | Cv::Builtin(_) => true,
    }
}

fn combine_bin<'a>(op: BinOp, l: Option<&Cv<'a>>, r: Option<&Cv<'a>>) -> Option<Cv<'a>> {
    let (l, r) = (l?, r?);
    Some(match op {
        BinOp::Add => {
            let lp = to_primitive(l);
            let rp = to_primitive(r);
            if is_str(&lp) || is_str(&rp) {
                Cv::Str(Cow::Owned(to_string(&lp) + &to_string(&rp)))
            } else {
                Cv::Num(to_number(&lp) + to_number(&rp))
            }
        }
        BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem | BinOp::Exp => {
            let a = to_number(l);
            let b = to_number(r);
            Cv::Num(match op {
                BinOp::Sub => a - b,
                BinOp::Mul => a * b,
                BinOp::Div => a / b,
                BinOp::Rem => a % b,
                BinOp::Exp => a.powf(b),
                _ => unreachable!(),
            })
        }
        BinOp::Shl | BinOp::Shr | BinOp::ShrU | BinOp::BitOr | BinOp::BitXor | BinOp::BitAnd => {
            let a = to_number(l) as i64 as i32;
            let b = to_number(r) as i64 as i32;
            let sh = (b as u32) & 31;
            Cv::Num(match op {
                BinOp::Shl => a.wrapping_shl(sh) as f64,
                BinOp::Shr => a.wrapping_shr(sh) as f64,
                BinOp::ShrU => (a as u32).wrapping_shr(sh) as f64,
                BinOp::BitOr => (a | b) as f64,
                BinOp::BitXor => (a ^ b) as f64,
                BinOp::BitAnd => (a & b) as f64,
                _ => unreachable!(),
            })
        }
        _ => return None,
    })
}

fn combine_un<'a>(op: UnaryOp, a: Option<&Cv<'a>>) -> Option<Cv<'a>> {
    let a = a?;
    Some(match op {
        UnaryOp::Neg => Cv::Num(-to_number(a)),
        UnaryOp::Plus => Cv::Num(to_number(a)),
        UnaryOp::BitNot => Cv::Num(!(to_number(a) as i64 as i32) as f64),
        UnaryOp::Not => Cv::Bool(!truthy(a)),
        UnaryOp::TypeOf => Cv::Str(Cow::Borrowed(match a {
            Cv::Num(_) => "number",
            Cv::Str(_) => "string",
            Cv::Bool(_) => "boolean",
            Cv::Undef => "undefined",
            Cv::Null | Cv::Window => "object",
            Cv::Fn(_) | Cv::Builtin(_) => "function",
        })),
        UnaryOp::Void => Cv::Undef,
        UnaryOp::Delete => return None,
    })
}

fn builtin_member(obj: &Cv, key: &str) -> Option<&'static str> {
    match obj {
        Cv::Window => match key {
            "btoa" => Some("btoa"),
            "atob" => Some("atob"),
            "parseInt" => Some("parseInt"),
            "String" => Some("String"),
            _ => None,
        },
        Cv::Str(_) if key == "constructor" => Some("String"),
        Cv::Num(_) if key == "constructor" => Some("Number"),
        Cv::Bool(_) if key == "constructor" => Some("Boolean"),
        Cv::Builtin("String") => match key {
            "fromCharCode" => Some("String.fromCharCode"),
            _ => None,
        },
        _ => None,
    }
}

fn call_builtin<'a>(name: &str, args: &[Option<Cv<'a>>]) -> Option<Cv<'a>> {
    let arg = |i: usize| args.get(i).and_then(|a| a.as_ref());
    match name {
        "btoa" => {
            let s = to_string(arg(0)?);
            let bytes: Vec<u8> = s.chars().map(|c| c as u32 as u8).collect();
            Some(Cv::Str(Cow::Owned(b64_encode(&bytes))))
        }
        "atob" => {
            let s = to_string(arg(0)?);
            let bytes = b64_decode(&s)?;
            Some(Cv::Str(Cow::Owned(
                bytes.iter().map(|&b| b as char).collect(),
            )))
        }
        "String" => Some(Cv::Str(Cow::Owned(match arg(0) {
            Some(a) => to_string(a),
            None => String::new(),
        }))),
        "String.fromCharCode" => {
            let mut out = String::new();
            for a in args {
                let code = to_number(a.as_ref()?) as i64 as u32 & 0xffff;
                out.push(char::from_u32(code)?);
            }
            Some(Cv::Str(Cow::Owned(out)))
        }
        "parseInt" => {
            let s = to_string(arg(0)?);
            let t = s.trim();
            let n: i64 = t.parse().ok()?;
            Some(Cv::Num(n as f64))
        }
        _ => None,
    }
}

const B64: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn b64_encode(input: &[u8]) -> String {
    let mut out = String::with_capacity(input.len().div_ceil(3) * 4);
    for chunk in input.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = *chunk.get(1).unwrap_or(&0) as u32;
        let b2 = *chunk.get(2).unwrap_or(&0) as u32;
        let n = (b0 << 16) | (b1 << 8) | b2;
        out.push(B64[(n >> 18 & 63) as usize] as char);
        out.push(B64[(n >> 12 & 63) as usize] as char);
        out.push(if chunk.len() > 1 {
            B64[(n >> 6 & 63) as usize] as char
        } else {
            '='
        });
        out.push(if chunk.len() > 2 {
            B64[(n & 63) as usize] as char
        } else {
            '='
        });
    }
    out
}

fn b64_decode(input: &str) -> Option<Vec<u8>> {
    let val = |c: u8| -> Option<u32> {
        match c {
            b'A'..=b'Z' => Some((c - b'A') as u32),
            b'a'..=b'z' => Some((c - b'a' + 26) as u32),
            b'0'..=b'9' => Some((c - b'0' + 52) as u32),
            b'+' => Some(62),
            b'/' => Some(63),
            _ => None,
        }
    };
    let cleaned: Vec<u8> = input
        .bytes()
        .filter(|&b| b != b'=' && !b.is_ascii_whitespace())
        .collect();
    let mut out = Vec::with_capacity(cleaned.len() / 4 * 3);
    for chunk in cleaned.chunks(4) {
        let mut n = 0u32;
        for (i, &c) in chunk.iter().enumerate() {
            n |= val(c)? << (18 - 6 * i);
        }
        out.push((n >> 16 & 0xff) as u8);
        if chunk.len() > 2 {
            out.push((n >> 8 & 0xff) as u8);
        }
        if chunk.len() > 3 {
            out.push((n & 0xff) as u8);
        }
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    const RFC4648: &[(&str, &str)] = &[
        ("", ""),
        ("f", "Zg=="),
        ("fo", "Zm8="),
        ("foo", "Zm9v"),
        ("foob", "Zm9vYg=="),
        ("fooba", "Zm9vYmE="),
        ("foobar", "Zm9vYmFy"),
    ];

    #[test]
    fn b64_encode_matches_rfc4648_vectors() {
        for (plain, encoded) in RFC4648 {
            assert_eq!(b64_encode(plain.as_bytes()), *encoded, "encode {plain:?}");
        }
    }

    #[test]
    fn b64_decode_matches_rfc4648_vectors() {
        for (plain, encoded) in RFC4648 {
            assert_eq!(b64_decode(encoded).as_deref(), Some(plain.as_bytes()), "decode {encoded:?}");
        }
    }

    #[test]
    fn b64_round_trips_every_byte_value() {
        let all: Vec<u8> = (0..=255).collect();
        for len in 0..=all.len() {
            let slice = &all[..len];
            assert_eq!(b64_decode(&b64_encode(slice)).as_deref(), Some(slice), "len {len}");
        }
    }

    #[test]
    fn b64_decode_ignores_whitespace() {
        assert_eq!(b64_decode("Zm9v\nYmFy").as_deref(), Some(&b"foobar"[..]));
    }

    #[test]
    fn b64_decode_rejects_invalid_characters() {
        assert_eq!(b64_decode("Zm9v!"), None);
    }
}
