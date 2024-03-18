pub struct QuickUnionUF {
    data: Vec<usize>,
}

impl QuickUnionUF {
    pub fn with_size(size: usize) -> Self {
        Self {
            data: Vec::from_iter(0..size),
        }
    }

    fn find_root(&self, mut idx: usize) -> usize {
        while self.data[idx] != idx {
            idx = self.data[idx];
        }
        return idx;
    }

    pub fn connected(&self, idxl: usize, idxr: usize) -> bool {
        return self.find_root(idxl) == self.find_root(idxr);
    }

    pub fn union(&mut self, idxl: usize, idxr: usize) {
        let rootl = self.find_root(idxl);
        let rootr = self.find_root(idxr);
        self.data[rootl] = rootr;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        let mut qf = QuickUnionUF::with_size(10);
        qf.union(1, 2);
        qf.union(3, 4);
        qf.union(3, 5);
        assert!(!qf.connected(1, 5));
        assert!(qf.connected(4, 5));
    }
}
