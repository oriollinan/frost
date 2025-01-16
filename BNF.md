# Backus-Naur form (BNF)

```bnf
<program> ::= <comment_block>* <import_block>? <function_block>*

<comment_block> ::= "%%" <comment_content> "%%"
<comment_content> ::= (%any_character% | "\n")*

<import_block> ::= "import" <string_literal>

<function_block> ::= <function_signature> "=" (<identifier> (" " <identifier>)*)? <block>
<function_signature> ::= <identifier> ":" <type>* "->" <type>

<type_definition> ::= <identifier> "::" <type>
<type> ::= "int" | "double" | "float" | "char" | "byte" | "never" | "bool" | "*"<type> | "struct" "{" <struct_field>* "}" | <user_defined_type>

<struct_field> ::= <identifier> "->" <type>
<user_defined_type> ::= <identifier>

<statement> ::= <variable_declaration> | <loop> | <for_loop> | <if_statement> | <function_call> | <expression> | <return_statement>

<variable_declaration> ::= <identifier> ":" <type> "=" <expression>
<expression> ::= <literal>
               | <binary_expression>
               | <unary_expression>
               | <function_call>
               | <type_conversion>
               | <array_access>
               | <struct_access>
               | <assignment>

<literal> ::= <int_literal> | <float_literal> | <array_literal> | <byte_literal> | <bool_literal> | "null"
<int_literal> ::= [0-9]+
<float_literal> ::= [0-9]+ "." [0-9]+
<byte_literal> ::= "'%any_printable_character%'"
<bool_literal> ::= "true" | "false"
<array_literal> ::= <string_literal> | <literal_array_literal>
<string_literal> ::= '"' <string_characters> '"'
<string_characters> ::= (%any_printable_character% - '"')*
<literal_array_literal> ::= "[" <expression> ("," <expression>)* "]"

<binary_expression> ::= <expression> <binary_operator> <expression>
<binary_operator> ::= "*" | "/" | "mod"
                    | "+" | "-"
                    | "&" | "|" | "^" | "<<" | ">>"
                    | "==" | "is" | "!=" | "<=" | ">=" | "<" | ">"
                    | "&&" | "and" | "||" | "or"

<unary_expression> ::= <prefix_operator> <expression> | <expression> <postfix_operator>
<prefix_operator> ::= "!" | "not" | "~" | "++" | "--"
<postfix_operator> ::= "++" | "--" | ".*" | ".&"

<type_conversion> ::= "@<type>" "(" <expression> ")"

<array_access> ::= <identifier> ".#" "(" <expression> ")"
<struct_access> ::= <identifier> "." <identifier>

<assignment> ::= <identifier> "=" <expression>

<loop> ::= "loop" "(" <expression> ")" <block>
<for_loop> ::= "from" <expression> "to" <expression> "by" <expression> "|" <identifier> ":" <type> "|" <block>
<if_statement> ::= "if" <expression> <block> <else_clause>?
<else_clause> ::= "else" <block>
<block> ::= "{" <statement>* "}"

<function_call> ::= <identifier> "(" (<expression> ("," <expression>)*)? ")"
<return_statement> ::= "ret" <expression> | <expression>

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

```

## Details on the BNF

### **Program Structure**
A **Frost** program consists of the following components:

- `<comment_block>`: Comments enclosed in `%%` markers. These are ignored by the parser and provide documentation or notes.
- `<import_block>`: Statements to import external modules or dependencies.
- `<function_block>`: Function definitions that include the function signature, optional arguments, and a block of statements.

**Example**:
```ff
%%
This is a comment block.
It provides information about the code.
%%
import "std/io.ff"

main: never -> int = {
    % Main function implementation
}
```

### Include Statements
You can include external modules using `import` statements:

- `<import_block>`: Specifies the inclusion of an external module.
- `<string_literal>`: The name of the module, enclosed in quotes.

**Example**:

```ff
import "std/io.ff"
```

### Functions
Functions in **Frost** are defined as follows:

- `<function_block>`: A function definition that includes:
    - `<function_signature>`: Declares the function name, parameter types, and return type.
    - Optional arguments: A space-separated list of parameter names.
    - `<block>`: A code block containing the function's statements.
- `<function_signature>`: Specifies the name, parameter types, and return type of the function.
- `<identifier>`: The name of the function or its parameters.

**Example**:

```ff
sum: int int -> int = a b {
    ret a + b
}
```

### Data Types
**Frost** supports a variety of data types, including primitives, pointers, and composite types:

- `<type>`:
    - Primitive types: `int`, `float`, `double`, `char`, `byte`, `bool`, `never`.
    - Pointer types: Denoted as `*<type>`.
    - Struct types: Defined using `struct` with fields.
    - User-defined types: Custom types defined by the user.
- `<struct_field>`: Fields in a struct, defined as `<identifier> -> <type>`.

**Example**:

```ff
personType :: struct {
    name -> char
    age -> int
}
```

### Statements

A **Frost** program consists of individual statements that define the program's behavior. These include:

- `<variable_declaration>`: Declares and initializes variables.
- `<loop>`: Iterative constructs.
- `<if_statement>`: Conditional constructs.
- `<function_call>`: Calls to functions.
- `<expression>`: Mathematical, logical, or function-related expressions.
- `<return_statement>`: Specifies the value returned by a function.

**Example**:

```ff
SET_WIDTH: int = 100

if x < 10 {
    ret x * 2
} else {
    ret x + 2
}
```

### Expressions

Expressions in **Frost** represent calculations, operations, or data manipulation. They include:

- `<literal>`: Constants like numbers, strings, booleans, or null.
- `<binary_expression>`: Expressions involving binary operators (e.g., `a + b`).
- `<unary_expression>`: Expressions with prefix or postfix operators.
- `<function_call>`: Invocations of functions.
- `<type_conversion>`: Explicit type conversions.
- `<array_access>`: Accessing elements of arrays.
- `<struct_access>`: Accessing fields of structs.
- `<assignment>`: Assigning values to variables.

**Example**:

```ff
char_index = iter * CHARSET_LENGTH / MAX_ITER
value: int = arr.#(index)
```

### Literals

Literals represent fixed values in Frost, such as numbers, strings, and booleans:

- `<int_literal>`: Integer values, e.g., `42`.
- `<float_literal>`: Floating-point values, e.g., `3.14`.
- `<byte_literal>`: A single byte enclosed in single quotes, e.g., `'A'`.
- `<bool_literal>`: Boolean values, `true` or `false`.
- `<array_literal>`: Arrays of literals or strings.
    - `<string_literal>`: Strings enclosed in double quotes.
    - `<literal_array_literal>`: Arrays enclosed in square brackets.

**Example**:

```ff
42          % Integer
3.14        % Float
'A'         % Byte
true        % Boolean
"hello"     % String
[1, 2, 3]   % Array
```

### Control Flow

Control flow constructs allow conditional and iterative execution:

- `<if_statement>`: Conditional statements with optional `else` clauses.
- `<loop>`: Repeatedly executes a block of code while a condition holds true.
- `<for_loop>`: Structured iteration over a range with a specific step size. Following the format:
    - from `<start>` to `<end>` by `<step>`: Specifies the range and step size.
    - `|<var>: <type>|`: Declares the loop variable and its type.
    - `<block>`: Contains the statements executed in each iteration.

**Example**:

```ff
if x > 0 {
    ret x
} else {
    ret -x
}

loop (i < 10) {
    i = i + 1
}

from 0 to SET_HEIGHT by 1 |y: int| {
    from 0 to SET_WIDTH by 1 |x: int| {
        sine_value: double = amplitude * sin(frequency * @double(x) / @double(SET_WIDTH) * 2,0 * M_PI + phase)

        scaled_y: int = SET_HEIGHT / 2 - @int(sine_value * @double(SET_HEIGHT) / (2,0 * amplitude))

        if y is scaled_y {
            printf("\x1b[%d;%dH*" y x)
        }
    }
    printf("\n" 0)
}
```

### Comments
Comments in **Frost** are enclosed in `%%` markers. They are ignored by the parser and provide documentation for the code.

**Example**:

```ff
%%
% This function calculates the sum of two numbers.
%%

```