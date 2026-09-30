#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Triplet {
    pub dest: u16,
    pub operator: u16,
    pub operand: u16,
}

pub fn parse(bytecode: &[u16]) -> Vec<Triplet> {
    bytecode
        .chunks_exact(3)
        .map(|c| Triplet {
            dest: c[0],
            operator: c[1],
            operand: c[2],
        })
        .collect()
}
