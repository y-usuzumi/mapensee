pub struct QuickUnionWeightedPC {
    data: Vec<usize>,
    sizes: Vec<usize>,
}

impl QuickUnionWeightedPC {
    pub fn with_size(size: usize) -> Self {
        Self {
            data: Vec::from_iter(0..size),
            sizes: vec![1; size],
        }
    }

    fn find_root(&mut self, mut idx: usize) -> usize {
        while idx != self.data[idx] {
            // This line of code makes every **other** node in path point to its grandparent
            // In practice this is just as good as flatten the whole path
            self.data[idx] = self.data[self.data[idx]];
            idx = self.data[idx];
        }

        return idx;
    }

    pub fn connected(&mut self, idxl: usize, idxr: usize) -> bool {
        return self.find_root(idxl) == self.find_root(idxr);
    }

    pub fn union(&mut self, idxl: usize, idxr: usize) {
        let rootl = self.find_root(idxl);
        let rootr = self.find_root(idxr);
        if self.sizes[rootl] > self.sizes[rootr] {
            self.data[rootr] = rootl;
            self.sizes[rootl] += self.sizes[rootr];
        } else {
            self.data[rootl] = rootr;
            self.sizes[rootr] += self.sizes[rootl];
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        let mut qf = QuickUnionWeightedPC::with_size(10);
        qf.union(1, 2);
        qf.union(3, 4);
        qf.union(3, 5);
        assert!(!qf.connected(1, 5));
        assert!(qf.connected(4, 5));
        assert_eq!(qf.sizes, vec![1, 1, 2, 1, 3, 1, 1, 1, 1, 1]);
    }
}
