
use std::rc::Rc;

use oxc_allocator::{Allocator, Vec as AVec};

use crate::processing::ingest::Triplet;
use crate::semantic::{
    BinOp, Expr, ExprList, ExprRef, InitialSlot, OpcodeMap, SymbolTable, UnaryOp, Value,
};

#[derive(Debug)]
pub enum Stmt<'a> {
    Bind(&'a str, ExprRef<'a>),
    Effect(ExprRef<'a>),
}

pub struct Flattened<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub unhandled: u32,
    pub folds: u32,
    pub forces: u32,
}

struct Partial<'a> {
    opcode: u32,
    args: Vec<AbsVal<'a>>,
    locals: Vec<(&'a str, ExprRef<'a>)>,
}

#[derive(Clone)]
enum AbsVal<'a> {
    Opcode(u32),
    Partial(Rc<Partial<'a>>),
    Expr(ExprRef<'a>),
    Unknown,
}

fn partial<'a>(
    opcode: u32,
    args: Vec<AbsVal<'a>>,
    locals: Vec<(&'a str, ExprRef<'a>)>,
) -> AbsVal<'a> {
    AbsVal::Partial(Rc::new(Partial {
        opcode,
        args,
        locals,
    }))
}

pub fn run<'a, 'o>(
    alloc: &'a Allocator,
    ops: &'o OpcodeMap<'a>,
    triplets: &[Triplet],
) -> Flattened<'a> {
    let mut f = Flattener::new(alloc, ops, triplets);
    for t in triplets {
        let func = f.regs[t.operator as usize].clone();
        let arg = f.regs[t.operand as usize].clone();
        let result = f.apply(func, arg);
        f.regs[t.dest as usize] = result;
    }
    Flattened {
        forces: f.forces,
        stmts: f.stmts,
        unhandled: f.unhandled,
        folds: 0,
    }
}

struct Flattener<'a, 'o> {
    alloc: &'a Allocator,
    ops: &'o OpcodeMap<'a>,
    regs: Vec<AbsVal<'a>>,
    stmts: Vec<Stmt<'a>>,
    invoke: Option<AbsVal<'a>>,
    tmp: u32,
    unhandled: u32,
    force_depth: u32,
    forces: u32,
    sinks: Vec<Vec<Stmt<'a>>>,
    inline: u32,
    red_depth: u32,
}

const MAX_FORCE_DEPTH: u32 = 1500;
const MAX_RED_DEPTH: u32 = 900;

impl<'a, 'o> Flattener<'a, 'o> {
    fn new(alloc: &'a Allocator, ops: &'o OpcodeMap<'a>, triplets: &[Triplet]) -> Self {
        let mut size = ops.initial.len();
        for &id in ops.ops.keys() {
            size = size.max(id as usize + 1);
        }
        for t in triplets {
            size = size
                .max(t.dest as usize + 1)
                .max(t.operator as usize + 1)
                .max(t.operand as usize + 1);
        }

        let mut regs = vec![AbsVal::Unknown; size];
        for (i, slot) in ops.initial.iter().enumerate() {
            regs[i] = match slot {
                InitialSlot::Null => AbsVal::Expr(alloc.alloc(Expr::Const(Value::Null))),
                InitialSlot::EmptyArray => AbsVal::Expr(alloc.alloc(Expr::Slot(i as u32))),
            };
        }
        for &id in ops.ops.keys() {
            regs[id as usize] = AbsVal::Opcode(id);
        }

        Flattener {
            alloc,
            ops,
            regs,
            stmts: Vec::new(),
            invoke: None,
            tmp: 0,
            unhandled: 0,
            force_depth: 0,
            forces: 0,
            sinks: Vec::new(),
            inline: 0,
            red_depth: 0,
        }
    }

    fn emit_stmt(&mut self, s: Stmt<'a>) {
        match self.sinks.last_mut() {
            Some(top) => top.push(s),
            None => self.stmts.push(s),
        }
    }

    fn reduce_block(
        &mut self,
        e: ExprRef<'a>,
        args: &[AbsVal<'a>],
        sym: &SymbolTable<'a>,
        locals: &[(&'a str, ExprRef<'a>)],
    ) -> ExprRef<'a> {
        if self.red_depth >= MAX_RED_DEPTH {
            return self.eval_deferred(e, args, sym, locals);
        }
        self.red_depth += 1;
        self.inline += 1;
        self.sinks.push(Vec::new());
        let saved_invoke = self.invoke.take();
        let val = self.eval(e, args, sym, locals);
        self.invoke = saved_invoke;
        let effects = self.sinks.pop().unwrap();
        self.inline -= 1;
        self.red_depth -= 1;

        let vexpr = self.render(&val);
        if effects.is_empty() {
            return vexpr;
        }
        let mut l = AVec::with_capacity_in(effects.len() + 1, self.alloc);
        for s in effects {
            match s {
                Stmt::Effect(x) => l.push(x),
                Stmt::Bind(name, x) => {
                    let lhs = self.mk(Expr::Local(name));
                    l.push(self.mk(Expr::Assign(lhs, x)));
                }
            }
        }
        l.push(vexpr);
        self.mk(Expr::Seq(l))
    }

    fn force_thunk(&mut self, inner: ExprRef<'a>, arg: AbsVal<'a>) -> AbsVal<'a> {
        if self.force_depth >= MAX_FORCE_DEPTH {
            self.unhandled += 1;
            let argr = self.render(&arg);
            let thunk = self.mk(Expr::Thunk(inner));
            return AbsVal::Expr(self.mk(Expr::Call(thunk, self.list1(argr))));
        }
        self.forces += 1;
        self.force_depth += 1;
        let prev = self.invoke.take();
        self.invoke = Some(arg);
        let empty = SymbolTable::new();
        let r = self.eval(inner, &[], &empty, &[]);
        self.invoke = prev;
        self.force_depth -= 1;
        r
    }

    fn mk(&self, e: Expr<'a>) -> ExprRef<'a> {
        self.alloc.alloc(e)
    }

    fn fresh(&mut self) -> &'a str {
        let n = self.tmp;
        self.tmp += 1;
        self.alloc.alloc_str(&format!("t{n}"))
    }

    fn undef(&self) -> AbsVal<'a> {
        AbsVal::Expr(self.mk(Expr::Const(Value::Undefined)))
    }

    fn list1(&self, e: ExprRef<'a>) -> ExprList<'a> {
        let mut l = AVec::with_capacity_in(1, self.alloc);
        l.push(e);
        l
    }

    fn render(&self, av: &AbsVal<'a>) -> ExprRef<'a> {
        match av {
            AbsVal::Expr(e) => e,
            AbsVal::Opcode(id) => self.mk(Expr::PartialApp {
                opcode: *id,
                args: AVec::new_in(self.alloc),
                locals: AVec::new_in(self.alloc),
            }),
            AbsVal::Partial(p) => {
                let mut l = AVec::with_capacity_in(p.args.len(), self.alloc);
                for a in &p.args {
                    l.push(self.render(a));
                }
                let mut lc = AVec::with_capacity_in(p.locals.len(), self.alloc);
                for (name, val) in &p.locals {
                    lc.push((*name, *val));
                }
                self.mk(Expr::PartialApp {
                    opcode: p.opcode,
                    args: l,
                    locals: lc,
                })
            }
            AbsVal::Unknown => self.mk(Expr::Unknown("absval")),
        }
    }

    fn apply(&mut self, f: AbsVal<'a>, x: AbsVal<'a>) -> AbsVal<'a> {
        match f {
            AbsVal::Opcode(id) => self.apply_partial(id, Vec::new(), Vec::new(), x),
            AbsVal::Partial(p) => {
                let args = p.args.clone();
                let locals = p.locals.clone();
                self.apply_partial(p.opcode, args, locals, x)
            }
            AbsVal::Expr(e) => match e {
                Expr::PartialApp {
                    opcode,
                    args,
                    locals,
                } => {
                    let mut a: Vec<AbsVal<'a>> = Vec::with_capacity(args.len());
                    for el in args.iter() {
                        a.push(self.eval_value(el));
                    }
                    let lc: Vec<(&'a str, ExprRef<'a>)> = locals.iter().copied().collect();
                    self.apply_partial(*opcode, a, lc, x)
                }
                Expr::Thunk(inner) => self.force_thunk(inner, x),
                _ => {
                    let xr = self.render(&x);
                    AbsVal::Expr(self.mk(Expr::Call(e, self.list1(xr))))
                }
            },
            AbsVal::Unknown => {
                self.unhandled += 1;
                AbsVal::Unknown
            }
        }
    }

    fn apply_partial(
        &mut self,
        opcode: u32,
        mut args: Vec<AbsVal<'a>>,
        mut locals: Vec<(&'a str, ExprRef<'a>)>,
        x: AbsVal<'a>,
    ) -> AbsVal<'a> {
        args.push(x);
        let n = args.len();

        let opmap = self.ops;
        let Some(op) = opmap.ops.get(&opcode) else {
            self.unhandled += 1;
            return AbsVal::Unknown;
        };
        let depth = op.depth as usize;

        for i in 0..op.pre.len() {
            if op.pre[i].depth as usize != n {
                continue;
            }
            let pname = op.pre[i].name;
            let pval = op.pre[i].value;
            let av = self.eval(pval, &args, &op.symbols, &locals);
            let r = self.render(&av);
            if pname == "_eff" {
                if has_effect(r) {
                    self.emit_stmt(Stmt::Effect(r));
                }
            } else if has_effect(r) && self.inline == 0 {
                let t = self.fresh();
                self.emit_stmt(Stmt::Bind(t, r));
                locals.push((pname, self.mk(Expr::Local(t))));
            } else {
                locals.push((pname, r));
            }
        }

        if n < depth {
            partial(opcode, args, locals)
        } else {
            self.saturate(opcode, &args, &locals)
        }
    }

    fn saturate(
        &mut self,
        opcode: u32,
        args: &[AbsVal<'a>],
        locals: &[(&'a str, ExprRef<'a>)],
    ) -> AbsVal<'a> {
        let opmap = self.ops;
        let op = opmap.ops.get(&opcode).expect("checked in apply_partial");

        match op.body {
            Expr::Thunk(inner) => {
                let d = self.reduce_block(inner, args, &op.symbols, locals);
                AbsVal::Expr(self.mk(Expr::Thunk(d)))
            }
            Expr::HashRoutine => {
                let mut l = AVec::with_capacity_in(args.len(), self.alloc);
                for a in args {
                    l.push(self.render(a));
                }
                let name = self.alloc.alloc_str(&format!("__op{opcode}"));
                let call = self.mk(Expr::Call(self.mk(Expr::Global(name)), l));
                if self.inline > 0 {
                    AbsVal::Expr(call)
                } else {
                    let t = self.fresh();
                    self.emit_stmt(Stmt::Bind(t, call));
                    AbsVal::Expr(self.mk(Expr::Local(t)))
                }
            }
            other => {
                let av = self.eval(other, args, &op.symbols, locals);
                match av {
                    AbsVal::Expr(e) => match e {
                        Expr::Assign(target, _) => {
                            self.emit_stmt(Stmt::Effect(e));
                            AbsVal::Expr(target)
                        }
                        _ if has_effect(e) => {
                            let t = self.fresh();
                            self.emit_stmt(Stmt::Bind(t, e));
                            AbsVal::Expr(self.mk(Expr::Local(t)))
                        }
                        _ => AbsVal::Expr(e),
                    },
                    other_av => other_av,
                }
            }
        }
    }

    fn eval_value(&mut self, e: ExprRef<'a>) -> AbsVal<'a> {
        let empty = SymbolTable::new();
        self.eval(e, &[], &empty, &[])
    }

    fn eval(
        &mut self,
        e: ExprRef<'a>,
        args: &[AbsVal<'a>],
        sym: &SymbolTable<'a>,
        locals: &[(&'a str, ExprRef<'a>)],
    ) -> AbsVal<'a> {
        match e {
            Expr::Arg(name) => match sym.lookup(name) {
                Some(i) => args[i as usize].clone(),
                None => {
                    self.unhandled += 1;
                    AbsVal::Expr(e)
                }
            },
            Expr::Local(name) => match locals.iter().rev().find(|(k, _)| k == name) {
                Some((_, v)) => AbsVal::Expr(v),
                None => AbsVal::Expr(e),
            },
            Expr::InvokeArgs => self.invoke.clone().unwrap_or(AbsVal::Expr(e)),

            Expr::Const(_)
            | Expr::Slot(_)
            | Expr::Global(_)
            | Expr::This
            | Expr::IterVar
            | Expr::CatchVar
            | Expr::HashRoutine
            | Expr::Reg { .. }
            | Expr::Unknown(_) => AbsVal::Expr(e),

            Expr::PartialApp {
                opcode,
                args: a,
                locals: lc,
            } => {
                if a.is_empty() && lc.is_empty() {
                    AbsVal::Opcode(*opcode)
                } else {
                    let mut v = Vec::with_capacity(a.len());
                    for el in a.iter() {
                        v.push(self.eval_value(el));
                    }
                    partial(*opcode, v, lc.iter().copied().collect())
                }
            }

            Expr::Force(inner) => {
                let av = self.eval(inner, args, sym, locals);
                if is_callable(&av) {
                    let u = self.undef();
                    self.apply(av, u)
                } else if let AbsVal::Expr(Expr::Thunk(t)) = av {
                    let u = self.undef();
                    self.force_thunk(t, u)
                } else {
                    let r = self.render(&av);
                    AbsVal::Expr(self.mk(Expr::Force(r)))
                }
            }

            Expr::Apply(name, cargs) => {
                let mut av = match sym.lookup(name) {
                    Some(i) => args[i as usize].clone(),
                    None => AbsVal::Expr(self.mk(Expr::Local(name))),
                };
                for c in cargs.iter() {
                    let cv = self.eval(c, args, sym, locals);
                    av = self.apply(av, cv);
                }
                av
            }

            Expr::Call(callee, cargs) => {
                let cv = self.eval(callee, args, sym, locals);
                if is_callable(&cv) {
                    if cargs.is_empty() {
                        let u = self.undef();
                        self.apply(cv, u)
                    } else {
                        let mut acc = cv;
                        for c in cargs.iter() {
                            let a = self.eval(c, args, sym, locals);
                            acc = self.apply(acc, a);
                        }
                        acc
                    }
                } else if let AbsVal::Expr(Expr::Thunk(t)) = cv {
                    let arg = if cargs.is_empty() {
                        self.undef()
                    } else {
                        self.eval(&cargs[0], args, sym, locals)
                    };
                    self.force_thunk(t, arg)
                } else {
                    let callee_r = self.render(&cv);
                    let mut l = AVec::with_capacity_in(cargs.len(), self.alloc);
                    for c in cargs.iter() {
                        let a = self.eval(c, args, sym, locals);
                        l.push(self.render(&a));
                    }
                    AbsVal::Expr(self.mk(Expr::Call(callee_r, l)))
                }
            }

            Expr::New(callee, cargs) => {
                let cv = self.eval(callee, args, sym, locals);
                let callee_r = self.render(&cv);
                let mut l = AVec::with_capacity_in(cargs.len(), self.alloc);
                for c in cargs.iter() {
                    let a = self.eval(c, args, sym, locals);
                    l.push(self.render(&a));
                }
                AbsVal::Expr(self.mk(Expr::New(callee_r, l)))
            }

            Expr::BinOp(op, l, r) => {
                let lv = self.eval(l, args, sym, locals);
                let rv = self.eval(r, args, sym, locals);
                let lr = self.render(&lv);
                let rr = self.render(&rv);
                let folded = self.mk(Expr::BinOp(*op, lr, rr));
                match as_const(folded) {
                    Some(n) => AbsVal::Expr(self.mk(Expr::Const(Value::Number(n)))),
                    None => AbsVal::Expr(folded),
                }
            }

            Expr::UnaryOp(op, a) => {
                let av = self.eval(a, args, sym, locals);
                let ar = self.render(&av);
                let folded = self.mk(Expr::UnaryOp(*op, ar));
                match as_const(folded) {
                    Some(n) => AbsVal::Expr(self.mk(Expr::Const(Value::Number(n)))),
                    None => AbsVal::Expr(folded),
                }
            }

            Expr::Index(obj, prop) => {
                let ov = self.eval(obj, args, sym, locals);
                let pv = self.eval(prop, args, sym, locals);
                let or = self.render(&ov);
                let pr = self.render(&pv);
                AbsVal::Expr(self.mk(Expr::Index(or, pr)))
            }

            Expr::Cond { test, then, alt } => {
                let tv = self.eval(test, args, sym, locals);
                if let AbsVal::Expr(te) = &tv {
                    if let Some(n) = as_const(te) {
                        return if n != 0.0 {
                            self.eval(then, args, sym, locals)
                        } else {
                            self.eval(alt, args, sym, locals)
                        };
                    }
                }
                let tr = self.render(&tv);
                let thr = {
                    let v = self.eval(then, args, sym, locals);
                    self.render(&v)
                };
                let ar = {
                    let v = self.eval(alt, args, sym, locals);
                    self.render(&v)
                };
                AbsVal::Expr(self.mk(Expr::Cond {
                    test: tr,
                    then: thr,
                    alt: ar,
                }))
            }

            Expr::Assign(target, value) => {
                let tv = self.eval(target, args, sym, locals);
                let vv = self.eval(value, args, sym, locals);
                let tr = self.render(&tv);
                let vr = self.render(&vv);
                AbsVal::Expr(self.mk(Expr::Assign(tr, vr)))
            }

            Expr::Seq(list) => {
                if list.is_empty() {
                    return self.undef();
                }
                let last = list.len() - 1;
                for el in &list[..last] {
                    let ev = self.eval(el, args, sym, locals);
                    let r = self.render(&ev);
                    if has_effect(r) {
                        self.emit_stmt(Stmt::Effect(r));
                    }
                }
                self.eval(list[last], args, sym, locals)
            }

            Expr::Thunk(inner) => {
                let d = self.reduce_block(inner, args, sym, locals);
                AbsVal::Expr(self.mk(Expr::Thunk(d)))
            }

            Expr::While { test, body } => {
                let t = self.reduce_block(test, args, sym, locals);
                let b = self.reduce_block(body, args, sym, locals);
                AbsVal::Expr(self.mk(Expr::While { test: t, body: b }))
            }
            Expr::ForIn { iter, body } => {
                let it = self.reduce_block(iter, args, sym, locals);
                let b = self.reduce_block(body, args, sym, locals);
                AbsVal::Expr(self.mk(Expr::ForIn { iter: it, body: b }))
            }
            Expr::TryCatch {
                try_body,
                catch_body,
            } => {
                let t = self.reduce_block(try_body, args, sym, locals);
                let c = self.reduce_block(catch_body, args, sym, locals);
                AbsVal::Expr(self.mk(Expr::TryCatch {
                    try_body: t,
                    catch_body: c,
                }))
            }
            Expr::Lambda { .. } | Expr::Let { .. } => AbsVal::Expr(e),
        }
    }

    fn eval_deferred(
        &mut self,
        e: ExprRef<'a>,
        args: &[AbsVal<'a>],
        sym: &SymbolTable<'a>,
        locals: &[(&'a str, ExprRef<'a>)],
    ) -> ExprRef<'a> {
        match e {
            Expr::Arg(name) => match sym.lookup(name) {
                Some(i) => self.render(&args[i as usize].clone()),
                None => e,
            },
            Expr::Local(name) => match locals.iter().rev().find(|(k, _)| k == name) {
                Some((_, v)) => v,
                None => e,
            },
            Expr::Const(_)
            | Expr::Slot(_)
            | Expr::Global(_)
            | Expr::This
            | Expr::IterVar
            | Expr::CatchVar
            | Expr::InvokeArgs
            | Expr::HashRoutine
            | Expr::Reg { .. }
            | Expr::PartialApp { .. }
            | Expr::Unknown(_) => e,

            Expr::Force(inner) => {
                let i = self.eval_deferred(inner, args, sym, locals);
                self.mk(Expr::Force(i))
            }
            Expr::Thunk(inner) => {
                let i = self.eval_deferred(inner, args, sym, locals);
                self.mk(Expr::Thunk(i))
            }
            Expr::UnaryOp(op, a) => {
                let a = self.eval_deferred(a, args, sym, locals);
                self.mk(Expr::UnaryOp(*op, a))
            }
            Expr::BinOp(op, l, r) => {
                let l = self.eval_deferred(l, args, sym, locals);
                let r = self.eval_deferred(r, args, sym, locals);
                self.mk(Expr::BinOp(*op, l, r))
            }
            Expr::Index(o, p) => {
                let o = self.eval_deferred(o, args, sym, locals);
                let p = self.eval_deferred(p, args, sym, locals);
                self.mk(Expr::Index(o, p))
            }
            Expr::Assign(t, v) => {
                let t = self.eval_deferred(t, args, sym, locals);
                let v = self.eval_deferred(v, args, sym, locals);
                self.mk(Expr::Assign(t, v))
            }
            Expr::Apply(name, cargs) => {
                let callee = match sym.lookup(name) {
                    Some(i) => self.render(&args[i as usize].clone()),
                    None => self.mk(Expr::Local(name)),
                };
                let l = self.deferred_list(cargs, args, sym, locals);
                self.mk(Expr::Call(callee, l))
            }
            Expr::Call(callee, cargs) => {
                let callee = self.eval_deferred(callee, args, sym, locals);
                let l = self.deferred_list(cargs, args, sym, locals);
                self.mk(Expr::Call(callee, l))
            }
            Expr::New(callee, cargs) => {
                let callee = self.eval_deferred(callee, args, sym, locals);
                let l = self.deferred_list(cargs, args, sym, locals);
                self.mk(Expr::New(callee, l))
            }
            Expr::Seq(list) => {
                let l = self.deferred_list(list, args, sym, locals);
                self.mk(Expr::Seq(l))
            }
            Expr::Cond { test, then, alt } => {
                let test = self.eval_deferred(test, args, sym, locals);
                let then = self.eval_deferred(then, args, sym, locals);
                let alt = self.eval_deferred(alt, args, sym, locals);
                self.mk(Expr::Cond { test, then, alt })
            }
            Expr::While { test, body } => {
                let test = self.eval_deferred(test, args, sym, locals);
                let body = self.eval_deferred(body, args, sym, locals);
                self.mk(Expr::While { test, body })
            }
            Expr::ForIn { iter, body } => {
                let iter = self.eval_deferred(iter, args, sym, locals);
                let body = self.eval_deferred(body, args, sym, locals);
                self.mk(Expr::ForIn { iter, body })
            }
            Expr::TryCatch {
                try_body,
                catch_body,
            } => {
                let try_body = self.eval_deferred(try_body, args, sym, locals);
                let catch_body = self.eval_deferred(catch_body, args, sym, locals);
                self.mk(Expr::TryCatch {
                    try_body,
                    catch_body,
                })
            }
            Expr::Lambda { .. } | Expr::Let { .. } => e,
        }
    }

    fn deferred_list(
        &mut self,
        items: &ExprList<'a>,
        args: &[AbsVal<'a>],
        sym: &SymbolTable<'a>,
        locals: &[(&'a str, ExprRef<'a>)],
    ) -> ExprList<'a> {
        let mut l = AVec::with_capacity_in(items.len(), self.alloc);
        for el in items.iter() {
            l.push(self.eval_deferred(el, args, sym, locals));
        }
        l
    }
}

fn is_callable(av: &AbsVal) -> bool {
    matches!(
        av,
        AbsVal::Opcode(_)
            | AbsVal::Partial(_)
            | AbsVal::Expr(Expr::PartialApp { .. })
    )
}

fn has_effect(e: ExprRef) -> bool {
    match e {
        Expr::Call(..)
        | Expr::New(..)
        | Expr::Apply(..)
        | Expr::Force(_)
        | Expr::Assign(..)
        | Expr::Seq(_)
        | Expr::Let { .. }
        | Expr::While { .. }
        | Expr::ForIn { .. }
        | Expr::TryCatch { .. } => true,
        Expr::BinOp(_, a, b) | Expr::Index(a, b) => has_effect(a) || has_effect(b),
        Expr::UnaryOp(_, a) => has_effect(a),
        Expr::Cond { test, then, alt } => has_effect(test) || has_effect(then) || has_effect(alt),
        _ => false,
    }
}

fn as_const(e: ExprRef) -> Option<f64> {
    match e {
        Expr::Const(Value::Number(n)) => Some(*n),
        Expr::Const(Value::Bool(b)) => Some(if *b { 1.0 } else { 0.0 }),
        Expr::Const(Value::Null) => Some(0.0),
        Expr::UnaryOp(UnaryOp::Neg, x) => as_const(x).map(|v| -v),
        Expr::UnaryOp(UnaryOp::Plus, x) => as_const(x),
        Expr::UnaryOp(UnaryOp::BitNot, x) => as_const(x).map(|v| !(v as i64 as i32) as f64),
        Expr::UnaryOp(UnaryOp::Not, x) => as_const(x).map(|v| if v == 0.0 { 1.0 } else { 0.0 }),
        Expr::BinOp(op, l, r) => {
            let a = as_const(l)?;
            let b = as_const(r)?;
            let ai = a as i64 as i32;
            let bi = b as i64 as i32;
            let sh = (b as i64 as u32) & 31;
            Some(match op {
                BinOp::Add => a + b,
                BinOp::Sub => a - b,
                BinOp::Mul => a * b,
                BinOp::Div => a / b,
                BinOp::Shl => ai.wrapping_shl(sh) as f64,
                BinOp::Shr => ai.wrapping_shr(sh) as f64,
                BinOp::ShrU => (a as i64 as u32).wrapping_shr(sh) as f64,
                BinOp::BitOr => (ai | bi) as f64,
                BinOp::BitAnd => (ai & bi) as f64,
                BinOp::BitXor => (ai ^ bi) as f64,
                _ => return None,
            })
        }
        _ => None,
    }
}

pub fn dump(flat: &Flattened) -> String {
    let mut s = String::new();
    for st in &flat.stmts {
        match st {
            Stmt::Bind(name, e) => {
                s.push_str(name);
                s.push_str(" = ");
                fmt_expr(e, &mut s);
                s.push_str(";\n");
            }
            Stmt::Effect(e) => {
                fmt_expr(e, &mut s);
                s.push_str(";\n");
            }
        }
    }
    s
}

fn fmt_list(items: &ExprList, out: &mut String) {
    for (i, e) in items.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        fmt_expr(e, out);
    }
}

fn fmt_expr(e: ExprRef, out: &mut String) {
    match e {
        Expr::HashRoutine => out.push_str("__hash"),
        Expr::Const(v) => fmt_value(v, out),
        Expr::Local(n) | Expr::Arg(n) | Expr::Global(n) => out.push_str(n),
        Expr::Slot(n) => out.push_str(&format!("v3[{n}]")),
        Expr::Reg { slot, version } => out.push_str(&format!("r{slot}_{version}")),
        Expr::This => out.push_str("this"),
        Expr::IterVar => out.push_str("_it"),
        Expr::CatchVar => out.push_str("_e"),
        Expr::InvokeArgs => out.push_str("arguments"),
        Expr::Unknown(k) => out.push_str(&format!("__unknown_{k}")),
        Expr::Force(a) => {
            fmt_expr(a, out);
            out.push_str("()");
        }
        Expr::Thunk(a) => {
            out.push_str("function() { return ");
            fmt_expr(a, out);
            out.push_str("; }");
        }
        Expr::Seq(list) => {
            out.push('(');
            fmt_list(list, out);
            out.push(')');
        }
        Expr::PartialApp { opcode, args, .. } => {
            out.push_str(&format!("op{opcode}"));
            if !args.is_empty() {
                out.push('(');
                fmt_list(args, out);
                out.push(')');
            }
        }
        Expr::Index(o, p) => {
            fmt_expr(o, out);
            out.push('[');
            fmt_expr(p, out);
            out.push(']');
        }
        Expr::Apply(name, args) => {
            out.push_str(name);
            out.push('(');
            fmt_list(args, out);
            out.push(')');
        }
        Expr::Call(c, args) => {
            fmt_expr(c, out);
            out.push('(');
            fmt_list(args, out);
            out.push(')');
        }
        Expr::New(c, args) => {
            out.push_str("new ");
            fmt_expr(c, out);
            out.push('(');
            fmt_list(args, out);
            out.push(')');
        }
        Expr::BinOp(op, l, r) => {
            out.push('(');
            fmt_expr(l, out);
            out.push_str(&format!(" {} ", bin_sym(op)));
            fmt_expr(r, out);
            out.push(')');
        }
        Expr::UnaryOp(op, a) => {
            out.push_str(un_sym(op));
            fmt_expr(a, out);
        }
        Expr::Cond { test, then, alt } => {
            fmt_expr(test, out);
            out.push_str(" ? ");
            fmt_expr(then, out);
            out.push_str(" : ");
            fmt_expr(alt, out);
        }
        Expr::Assign(t, v) => {
            fmt_expr(t, out);
            out.push_str(" = ");
            fmt_expr(v, out);
        }
        Expr::Let { .. } => out.push_str("undefined /* let */"),
        Expr::Lambda { .. } => out.push_str("function() {}"),
        Expr::TryCatch {
            try_body,
            catch_body,
        } => {
            out.push_str("(function() { try { return ");
            fmt_expr(try_body, out);
            out.push_str("; } catch (_e) { return ");
            fmt_expr(catch_body, out);
            out.push_str("; } })()");
        }
        Expr::ForIn { iter, body } => {
            out.push_str("(function() { for (var _it in ");
            fmt_expr(iter, out);
            out.push_str(") { ");
            fmt_expr(body, out);
            out.push_str("; } })()");
        }
        Expr::While { test, body } => {
            out.push_str("(function() { while (");
            fmt_expr(test, out);
            out.push_str(") { ");
            fmt_expr(body, out);
            out.push_str("; } })()");
        }
    }
}

fn fmt_value(v: &Value, out: &mut String) {
    match v {
        Value::Undefined => out.push_str("undefined"),
        Value::Null => out.push_str("null"),
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Number(n) if n.is_infinite() => {
            out.push_str(if *n > 0.0 { "Infinity" } else { "-Infinity" })
        }
        Value::Number(n) => out.push_str(&format!("{n}")),
        Value::String(s) => out.push_str(&format!("{s:?}")),
        Value::Window => out.push_str("window"),
        Value::Fn(s) => out.push_str(s),
    }
}

fn bin_sym(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Rem => "%",
        BinOp::Exp => "**",
        BinOp::Shl => "<<",
        BinOp::Shr => ">>",
        BinOp::ShrU => ">>>",
        BinOp::BitOr => "|",
        BinOp::BitXor => "^",
        BinOp::BitAnd => "&",
        BinOp::Eq => "==",
        BinOp::Neq => "!=",
        BinOp::StrictEq => "===",
        BinOp::StrictNeq => "!==",
        BinOp::Lt => "<",
        BinOp::Le => "<=",
        BinOp::Gt => ">",
        BinOp::Ge => ">=",
        BinOp::In => "in",
        BinOp::InstanceOf => "instanceof",
        BinOp::LogOr => "||",
        BinOp::LogAnd => "&&",
        BinOp::Coalesce => "??",
    }
}

fn un_sym(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Plus => "+",
        UnaryOp::Not => "!",
        UnaryOp::BitNot => "~",
        UnaryOp::TypeOf => "typeof ",
        UnaryOp::Void => "void ",
        UnaryOp::Delete => "delete ",
    }
}
