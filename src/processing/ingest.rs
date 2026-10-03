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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn groups_words_into_triplets_in_order() {
        let triplets = parse(&[1, 2, 3, 4, 5, 6]);
        assert_eq!(
            triplets,
            vec![
                Triplet { dest: 1, operator: 2, operand: 3 },
                Triplet { dest: 4, operator: 5, operand: 6 },
            ]
        );
    }

    #[test]
    fn drops_incomplete_trailing_words() {
        assert_eq!(parse(&[1, 2, 3, 4, 5]).len(), 1);
        assert_eq!(parse(&[1, 2]).len(), 0);
    }

    #[test]
    fn empty_input_gives_no_triplets() {
        assert!(parse(&[]).is_empty());
    }
}
