use waffle::{
    gc_frame,
    memory::{
        gcwrapper::{GCWrapper, Gc},
        Finalize, Object, Trace, Visitor,
    },
};

#[derive(Clone, Copy)]
pub enum Sexp {
    Nil,
    Float(f32),
    Int(isize),
    Cons(Gc<Cons>),
}
pub struct Cons(Sexp, Sexp);
unsafe impl Trace for Sexp {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        match self {
            Self::Cons(c) => c.trace(vis),
            _ => (),
        }
    }
}
unsafe impl Finalize for Sexp {}
impl Object for Sexp {}

unsafe impl Trace for Cons {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.0.trace(vis);
        self.1.trace(vis);
    }
}
unsafe impl Finalize for Cons {}
impl Object for Cons {}
fn main() {
    let mut gc = GCWrapper::new();

    let mut root = Sexp::Nil;

    gc_frame!(gc.roots() => root);

    let mut i = 0usize;
    while i < 1000000000 {
        *root.as_mut() = Sexp::Cons(gc.fixed(Cons(Sexp::Int(i as _), *root.as_ref())));
        if i % 10 * 1024 == 0 {
            *root.as_mut() = Sexp::Nil;
        }
        i += 1;
    }
}
