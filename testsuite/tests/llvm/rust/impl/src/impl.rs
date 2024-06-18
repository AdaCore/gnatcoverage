use std::hint::black_box;

struct Foo {
    x: u32
}

impl Foo {
    fn new(x: u32) -> Self {
        Self {
            x
        }
    }

    fn print(&self) {
        black_box(self.x);
    }
}

struct Bar;

trait Hello {
    fn hello(&self) -> &'static str;

    fn say_hello(&self) {
        black_box(self.hello());
    }

    fn say_bye(&self) {
        black_box("Bye");
    }

    fn say_nothing(&self) {
        black_box("nothing");
    }
}

impl Hello for Foo {
    fn hello(&self) -> &'static str {
        "Hola"
    }
}

impl Hello for Bar {
    fn hello(&self) -> &'static str {
        "Hallo"
    }
}

fn main() {
    let foo = Foo::new(15);
    foo.print();

    foo.say_hello();
    Bar.say_hello();

    foo.say_bye();

}

