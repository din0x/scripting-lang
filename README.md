# scripting-lang
Simple scripting language made for learning purposes.

## Installation
Make sure you are at least using `cargo 1.80.0-nightly`

```sh
# Clone the repository
git clone https://github.com/din0x/scripting-lang.git

# Navigate to the project directory
cd scripting-lang

# Build project
cargo build --release 
```

## Features

#### Types 
```rs
let integer = 420;
let float = 69.69;
let string = "hello world";
let bool = 2 != 1;
let list = [1, "lists can hold any types", ["evan", "lists"]]
let function = fn(x){ x * x }
```

#### Variables
```rs
let foo = "hello " * 10;
foo = 300
```

#### Conditionals
```rs
if i > 100 {
  print("nice");
}
```

#### Loops
```rs
while i != 10 {
  i = i + 1;
}
```

#### Functions 

```rs
fn example_function(a, b) {
  a * b
}

let function_literal = fn(name) {
  print("hello " + name)
}
```

## Examples
```rs
let i = 0;

while i <= 100 {
    if i % 15 == 0 {
        print("fizzbuzz")
    } else if i % 3 == 0 {
        print("fizz");
    } else if i % 5 == 0 {
        print("buzz");
    } else {
        print(i);
    }

    i = i + 1;
}

```
