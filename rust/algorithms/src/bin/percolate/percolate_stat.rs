use rand::Rng;

use crate::percolate::Percolate;

pub struct PercolateStat {
    size: usize,
    trials: usize,
    results: Vec<usize>,
}

impl PercolateStat {
    pub fn new(size: usize, trials: usize) -> Self {
        Self {
            size,
            trials,
            results: Vec::with_capacity(trials),
        }
    }

    pub fn run(&mut self) {
        let mut rng = rand::thread_rng();
        for _ in 0..self.trials {
            let mut p = Percolate::new(self.size);
            let mut count = 0;
            while !p.percolates() {
                loop {
                    let row = rng.gen_range(0..self.size);
                    let col = rng.gen_range(0..self.size);
                    if !p.is_open(row, col) {
                        p.open(row, col);
                        count += 1;
                        break;
                    }
                }
            }
            self.results.push(count);
        }
    }

    pub fn mean(&self) -> f64 {
        return self.results.iter().sum::<usize>() as f64
            / self.results.len() as f64
            / self.size as f64
            / self.size as f64;
    }
}
