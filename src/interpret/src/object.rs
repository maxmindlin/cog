#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Object {
    Int(i64),
    Null,
    Error,
}
