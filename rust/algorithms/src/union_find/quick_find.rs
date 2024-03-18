pub struct QuickFindUF {
    data: Vec<usize>,
}

impl QuickFindUF {
    pub fn with_size(size: usize) -> Self {
        Self {
            data: Vec::from_iter(0..size),
        }
    }

    pub fn connected(&self, idxl: usize, idxr: usize) -> bool {
        return self.data[idxl] == self.data[idxr];
    }

    pub fn union(&mut self, idxl: usize, idxr: usize) {
        let val = self.data[idxl];
        let target_val = self.data[idxr];
        for idx in 0..self.data.len() {
            if self.data[idx] == val {
                self.data[idx] = target_val;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        let mut qf = QuickFindUF::with_size(10);
        qf.union(1, 2);
        qf.union(3, 4);
        qf.union(3, 5);
        assert!(!qf.connected(1, 5));
        assert!(qf.connected(4, 5));
    }
}
