## built with zig 0.15

- use 'zig build' to build from source

# Para Language Documentation

Para is a minimalist data representation language designed for clear, typed data structures. It prioritizes simplicity, compile-time error detection, and explicitness, making it ideal for configuration files, data serialization, and simple data modeling. Para maps directly to Zig types for efficient integration.

## Design Principles

- **Simplicity**: Minimal syntax with clear distinctions between variables and groups.
- **Explicitness**: All variables must be initialized, and types are strictly enforced.
- **Compile-Time Safety**: Errors (e.g., undefined references, type mismatches) are caught at compile time.
- **Mutability**: Variables must be explicitly declared as `var` (mutable) or `const` (immutable).
- **Scoped Naming**: Variables and groups have distinct namespaces within their scope.

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

Para has five fixed types. Variables must be explicitly declared as `var` (mutable) or `const` (immutable). Temporary variables can be declared with the `temp` prefix and are dropped during the compression stage. Heterogeneous arrays will be added in the future.

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

```para
// time is a i64 but it allows the front end to handle ISO conversion
var created: time = 17453900000
var updated: time = "2024-03-14T16:45:00Z"
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
