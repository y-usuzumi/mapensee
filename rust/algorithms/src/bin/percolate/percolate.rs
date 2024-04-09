use algorithms::union_find::quick_union_weighted_with_path_compression::QuickUnionWeightedPC;

pub struct Percolate {
    size: usize,
    uf: QuickUnionWeightedPC,
    grid: Vec<bool>,
    open_sites: usize,
}

impl Percolate {
    pub fn new(size: usize) -> Self {
        Self {
            size,
            // The last two elements are the virtual top and the virtual bottom
            uf: QuickUnionWeightedPC::with_size(size * size + 2),
            grid: vec![false; size * size],
            open_sites: 0,
        }
    }

    pub fn is_open(&self, row: usize, col: usize) -> bool {
        return self.grid[self.row_col_to_idx(row, col)];
    }

    pub fn is_full(&mut self, row: usize, col: usize) -> bool {
        return self
            .uf
            .connected(self.get_virtual_top_idx(), self.row_col_to_idx(row, col));
    }

    pub fn number_of_open_sites(&self) -> usize {
        return self.open_sites;
    }

    pub fn percolates(&mut self) -> bool {
        return self
            .uf
            .connected(self.get_virtual_top_idx(), self.get_virtual_bottom_idx());
    }

    pub fn open(&mut self, row: usize, col: usize) {
        if self.is_open(row, col) {
            return;
        }

        let idx = self.row_col_to_idx(row, col);
        self.grid[idx] = true;
        self.open_sites += 1;
        // Connect to cell above
        if row == 0 {
            self.uf.union(self.get_virtual_top_idx(), idx);
        } else if self.is_open(row - 1, col) {
            let idx_above = self.row_col_to_idx(row - 1, col);
            self.uf.union(idx_above, idx);
        }
        // Connect to cell below
        if row == self.size - 1 {
            self.uf.union(idx, self.get_virtual_bottom_idx());
        } else if self.is_open(row + 1, col) {
            let idx_below = self.row_col_to_idx(row + 1, col);
            self.uf.union(idx, idx_below);
        }
        // Connect to cell on the left
        if col > 0 && self.is_open(row, col - 1) {
            let idx_left = self.row_col_to_idx(row, col - 1);
            self.uf.union(idx_left, idx);
        }
        // Connect to cell on the right
        if col < self.size - 1 {
            let idx_right = self.row_col_to_idx(row, col + 1);
            self.uf.union(idx, idx_right);
        }
    }

    fn get_virtual_top_idx(&self) -> usize {
        return self.size * self.size;
    }

    fn get_virtual_bottom_idx(&self) -> usize {
        return self.size * self.size + 1;
    }

    fn idx_to_row_col(&self, idx: usize) -> (usize, usize) {
        let row = idx / self.size;
        let col = idx % self.size;
        return (row, col);
    }

    fn row_col_to_idx(&self, row: usize, col: usize) -> usize {
        return row * self.size + col;
    }
}

#[cfg(test)]
mod tests {
    use super::Percolate;

    fn print_grid(p: &Percolate) {
        let grid: Vec<Vec<bool>> = p.grid.chunks(p.size).map(|s| s.into()).collect();
        println!("{:#?}", grid);
    }

    #[test]
    fn test_percolate() {
        let mut p = Percolate::new(5);
        // 0, 0, 0, 0, 1
        // 1, 1, 0, 0, 0
        // 0, 1, 0, 0, 0
        // 0, 1, 1, 0, 0
        // 0, 0, 1, 1, 1
        let opens = vec![
            (2, 1),
            (3, 2),
            (4, 3),
            (4, 4),
            (1, 0),
            (1, 1),
            (3, 1),
            (4, 2),
            (0, 4),
        ];
        for (row, col) in opens {
            p.open(row, col);
            assert!(!p.percolates());
        }
        p.open(0, 0);
        assert!(p.percolates())
    }
}
