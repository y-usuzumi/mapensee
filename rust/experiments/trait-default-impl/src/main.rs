mod lib;

use lib::*;

fn main() {
    let koshi = Identity::new(String::from("こし"));
    koshi.greet();
}
