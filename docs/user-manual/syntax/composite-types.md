# Composite Types in Frost

## Structs

Structs in Frost provide a way to create custom data types by combining
different fields into a single unit. They serve as the foundation for building
complex data structures.

### Basic Structure Definition

```frost
point :: struct {
    x -> double
    y -> double
}

color :: struct {
    r -> byte
    g -> byte
    b -> byte
    a -> byte
}
```

### Creating and Using Structs

```frost
% Creating instances
origin: point = point {
    x -> 0,0
    y -> 0,0
}

% Accessing fields
x_coord: double = origin.x
```

### Nested Structures

```frost
% Nested structs
rectangle :: struct {
    top_left -> point
    bottom_right -> point
    color -> color
}

% Creating nested structs
rect: rectangle = rectangle {
    top_left -> point {
        x -> 0,0
        y -> 0,0
    }
    bottom_right -> point {
        x -> 10,0
        y -> 10,0
    }
    color -> color {
        r -> 255
        g -> 0
        b -> 0
        a -> 255
    }
}

% Accessing nested fields
red: byte = rect.color.r

% Modifying nested fields
rect.color.r = 128
```

Remember that structs in Frost are designed to be efficient and safe, with
zero-cost abstractions where possible. The compiler optimizes struct layouts and
ensures memory safety while maintaining high performance.
