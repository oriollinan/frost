### Standard Library

Frost's Standard Library is primarily a wrapper around C header libraries,
providing access to essential functions through foreign function declarations.
This approach allows Frost to leverage existing, well-optimized C libraries
while maintaining its own syntax and type system. The Standard Library includes
bindings to:

- **string.ff**: String manipulation (wraps string.h)
- **math.ff**: Mathematical functions (wraps math.h)
- **io.ff**: Input/output operations (wraps stdio.h)
- **stdlib.ff**: General utilities (wraps stdlib.h)
- **sdl2.ff**: SDL2 bindings for multimedia programming
- **opengl.ff**: OpenGL bindings for graphics programming

Here's an example of how these foreign functions are declared in Frost:

```frost
import "https://frost-lang.deno.dev/std/string.ff"
```

To use these functions:

```frost
main: never -> int = {
    message: *byte = "Hello, Frost!"
    length: int = strlen(message)
    printf("Length of message: %d\n" length)

    0
}
```

### Extending the Standard Library

While the current Standard Library primarily consists of foreign function
declarations, Frost is designed to allow for the implementation of native
functions in the future. This would enable the creation of Frost-specific
abstractions and optimizations.

For example, a future version of the Standard Library might include a native
implementation of a string manipulation function:

```frost
reverse_string: *byte -> *byte = input {
    length: int = strlen(input)
    result: *byte = malloc(length + 1)
    defer free(result)

    from 0 to length by 1 [i: int] {
        result.#i = input.#(length - i - 1)
    }
    result.#length = '\0'

    result
}
```

This approach would combine the efficiency of C libraries with the
expressiveness and safety features of Frost.

Remember that when using Standard Library functions, you need to import the
appropriate module at the beginning of your Frost file. The compiler will handle
linking the necessary C libraries when compiling to the final executable. If it
cannot be found on the default path, you will need to specify the path to the
library manually.
