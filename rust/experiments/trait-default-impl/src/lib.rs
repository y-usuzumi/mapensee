pub trait Greet<T> {
    fn greeter(&self) -> String;
    fn greet(&self) {
        println!("Greetings, {}.", self.greeter())
    }
}

pub struct Identity {
    name: String,
}

impl Identity {
    pub fn new(name: String) -> Self {
        return Identity { name: name };
    }
}

impl Greet<String> for Identity {
    fn greeter(&self) -> String {
        self.name.clone()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
