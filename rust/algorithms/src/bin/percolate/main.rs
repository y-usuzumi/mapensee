use clap::Parser;
mod percolate;
mod percolate_stat;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Grid size (n-by-n)
    #[arg(default_value_t = 20)]
    size: usize,

    /// Number of experiment iterations
    #[arg(default_value_t = 100)]
    iterations: usize,
}

fn main() {
    let args = Args::parse();
    let mut ps = percolate_stat::PercolateStat::new(200, 100);
    ps.run();
    println!("mean: {}", ps.mean());
}
