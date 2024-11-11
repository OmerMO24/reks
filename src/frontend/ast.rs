#[derive(Debug, Default)]
pub enum IntWidth {
    #[default]
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Default)]
pub enum FloatWidth {
    #[default]
    F32,
    F64,
}

#[derive(Debug)]
pub enum Constness {
    Const,
    Mut,
}

#[derive(Debug)]
pub struct Identifier<'a> {
    pub name: &'a str,
}

#[derive(Debug)]
pub struct Integer {
    pub value: i128,
    pub width: IntWidth,
}

#[derive(Debug)]
pub struct Float {
    pub value: f64,
    pub width: FloatWidth,
}
