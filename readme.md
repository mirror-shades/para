## built with zig 0.15.2

- use 'zig build' to build from source
- use `zig build test` to run tests (it sets `PARA_BIN` for the test runner)

# Para Language Documentation

Para is a data representation language designed to produce clear, statically typed data structures. It allows you to write your config in a more programmatic style, then convert it to the serialization language of your choice. Current output formats are: JSON, YAML, TOML, ZON, RON.

NOTE: This project is still in early development and should not be relied on in production databases.

## Design Principles

- **Simplicity**: Minimal syntax with clear distinctions between variables and groups.
- **Explicitness**: All variables must be initialized, and types are strictly enforced.
- **Compile-Time Safety**: Errors (e.g., undefined references, type mismatches) are caught at compile time.
- **Mutability**: Variables must be explicitly declared as `var` (mutable) or `const` (immutable).
- **Scoped Naming**: Variables and groups have distinct namespaces within their scope.

## Features

### Expressions and Operators

Para supports mathematical expressions with standard arithmetic operators:

```para
const result = 10 + 5 * 2  // 20
const power = 2 ^ 3        // 8 (exponentiation)
const modulo = 17 % 5      // 2
const division = 15 / 3    // 5
```

### Peek Operator (`?`)

The peek operator allows you to inspect variable values during preprocessing. Place `?` after any variable or expression to print its current value and type:

```para
const x = 42
x?  // Prints: [line:col] x :int = 42

const result = x + 8
result?  // Prints: [line:col] result :int = 50
```

This is especially useful for debugging complex expressions or verifying preprocessing steps.

### Variable Assignment and Reassignment

Variables can be assigned and reassigned within their scope:

```para
var counter = 0
counter = counter + 1  // Reassignment allowed for 'var'
```

### Temporary Variables

Use the `temp` keyword to declare variables that are only used during preprocessing and dropped from the final output:

```para
temp const intermediate = 100
const final = intermediate * 2  // 'intermediate' won't appear in output
```

### Assertions

You can add assertions to help catch incorrect or corrupted data:

```para
const port = 8080
# assert port > 1024 and port < 65535  // Valid port range

const timeout = 30
# assert timeout > 0 and timeout < 300  // Reasonable timeout range

const percentage = 85
# assert percentage >= 0 and percentage <= 100  // Valid percentage
```

## Syntax Rules

- Variables are declared with `:` followed by a type and `=` followed by a value or expression.
- Groups are accessed using dotted paths (e.g. `person.age`, `person.job.salary`). Scopes with braces can also be used for clarity (e.g. `person { ... }`).
- Statements end with a newline or EOF; whitespace and empty lines are ignored.
- Identifiers must be unique within their scope (e.g., a variable and group cannot share a name).
- All variables must be initialized; uninitialized declarations are a compile-time error.
- References to variables or groups must be defined earlier in the file (top-to-bottom evaluation).

### Identifiers

- Valid identifiers use `a-z`, `0-9`, `_`, but must not start with a number (e.g., `my_var`, `user2`, `first_name`).
- Case-sensitive (e.g., `myVar` and `MyVar` are distinct).
- Reserved keywords (TBD, e.g., `int`, `float`, `string`, `bool`, `time`) cannot be used as identifiers.

### Comments

- Single-line: `// This is a comment`
- Multi-line: `/* This is a multi-line comment */`

## Simple Types

Para has five fixed scalar types. Variables must be explicitly declared as `var` (mutable) or `const` (immutable). Temporary variables can be declared with the `temp` prefix and are dropped during the compression stage. Homogeneous arrays are supported via `T[]` (and can be nested like `T[][]`).

### Integer

```para
// int is an i64
var x: int = 18
```

### Float

```para
// float is a f64
var y: float = 3.14
```

### String

```para
// string is a []u8
var z: string = "hello world"
```

### Boolean

```para
// bool is a boolean
var logics: bool = true
```

### Time

`time` is stored as an `i64` containing Unix epoch milliseconds (UTC).

Accepted formats:

- Integer literal: treated as epoch milliseconds
- String literal: either a numeric epoch-millis value (optional leading `+`/`-`), or a subset of ISO-8601 (examples: `"2024-03-14"`, `"2024-03-14T16:45:00Z"`, `"2024-03-14T16:45:00.123+02:00"`); if no timezone is provided, it’s treated as UTC

```para
var created: time = 1710434700000
var updated: time = "2024-03-14T16:45:00Z"
```

### Arrays

Arrays are homogeneous and can be nested. Empty arrays require an explicit type annotation.

```para
const xs: int[] = [1, 2, 3]
const xss: int[][] = [[1, 2], [3]]
const empty: string[] = []
```

### Variables and Constants

Variables must be explicitly declared as mutable or immutable:

```para
// Mutable variables (can be reassigned)
var x: int = 42
x = 50  // This works

// Immutable constants (cannot be reassigned)
const id: int = 567
id = 600  // This errors

// Temporary variables (dropped during preprocessing)
temp var intermediate: int = 100
temp const constant_temp: string = "temporary"
```

## Variable assignment

Variables can be assigned to the value of another variable. The value is always copied, not referenced.

```para
const x: int = 5
const y: int = x
```



## Complex Types

Para has one complex type called a "group", similar to a struct in other languages.

### Basic Group Usage

```para
person.age: int = 25
person.name: string = "Bob"

// nested groupings are allowed as well
bigNest.littleNest.member1: int = 5
```

### Group Scopes

Groups can be written using scope syntax for better readability:

```para
// this
bigNest {
    littleNest {
        member1: int = 10
    }
    member2: int = 2
    member3: int = 3
}

// reduces to
bigNest.littleNest.member1: int = 10
bigNest.member2: int = 2
bigNest.member3: int = 3
```

### Typed Group Scopes

Groups can enforce types for their members:

```para
// this
nest: int {
    const member1 = 10
    const member2 = 20
}

// reduces to
const nest.member1: int = 10
const nest.member2: int = 20
```

## Preprocessing Steps

Para files go through several preprocessing steps for flexibility and optimization:

### Step 1: Starting Config

Configs are flexible - as long as values are initialized before use, they can be referenced:

```para
const default_age: int = 25
temp const new_age: int = defaults.age + 1
var person.age: int = new_age
const person.name: string = "Robert"
temp const nickname: string = "Bob"
const person.nickname = nickname
```



### Step 2: Baked Values

First stage is resolving expressions and assignments to literal values:

```para
default_age: int = 25
new_age: int = 26
person.age: int = 26
person.name: string = "Robert"
nickname: string = "Bob"
person.nickname = "Bob"
```

### Step 3: Compressed Values

Next we clean up the file and unify formatting. Temps are dropped, globals are raised, and groups are unified:

```para
default_age: int = 25
person {
    age: int = 26
    name: string = "Robert"
    nickname = "Bob"
}
```

### Step 4: Transpile

The compressed format can be transpiled into other formats:

```json
{
  "default_age": 25,
  "person": {
    "age": 26,
    "name": "Robert",
    "nickname": "Bob"
  }
}
```

Or into Zig object notation (ZON):

```zig
.{
  .default_age = 25,
  .person = .{
    .age = 26,
    .name = "Robert",
    .nickname = "Bob",
  },
}
```

Or into TOML tables:

```toml
default_age = 25

[person]
age = 26
name = "Robert"
nickname = "Bob"
```

Or into RON:

```ron
(
  default_age: 25,
  person: (
    age: 26,
    name: "Robert",
    nickname: "Bob",
  ),
)
```