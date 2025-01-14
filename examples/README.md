# examples

This directory contains examples of how to use the `frost` programming language.
In order to run the examples, you must have the `frost` compiler installed on
your system. You can find instructions on how to install the `frost` compiler in
the [README.md](../README.md) file in the root of this repository.

Each file in this directory is a standalone example that demonstrates a specific
feature of the `frost` programming language.

## List of Examples

- [Array](array.ff): This example demonstrates how to access array elements.
- [3D Spinning Donut](donut.ff): This example demonstrates how to generate a
  spinning 3D donut using ASCII characters.
- [Hello](hello.ff): This example demonstrates how to print "Hello, World!".
- [Julia Set](julia.ff): This example demonstrates how to generate an animated
  Julia set.
- [Mandelbrot Set](mandelbrot.ff): This example demonstrates how to generate an
  snapshot of the Mandelbrot set.
- [Putstring](putstring.ff): This example implements the `putstring` function
  that prints a string to the terminal.
- [Sierpinski Triangle](sierpinski.ff): This example demonstrates how to
  generate a Sierpinski triangle.
- [Sine Wave](sine.ff): This example demonstrates how to generate an animated
  sine wave.

## Running the Examples

To run an example, you can use the `frost` compiler to compile the example file
to LLVM IR, and the use `lli` to run the LLVM IR. For example, to run the
Mandelbrot set example, you can run the following commands:

```sh
$ ./glados -i mandelbrot.ff -o mandelbrot.ll
```

> [!TIP]
> Use LLVM's [`opt`](https://llvm.org/docs/CommandGuide/opt.html) tool to
> optimize the generated LLVM IR before running it with `lli`.
>
> ```
> opt -passes=mem2reg -S mandelbrot.ll -o mandelbrot.opt.ll
> ```

```sh
$ lli mandelbrot.ll
```

When you run the above commands, the Mandelbrot set will be generated and
displayed in the terminal.

```
                                      @@@@@@                                   
                                      @@@@@@@                                  
                                        @@. .                                  
                           .@@  @@@@@@@@@@@@@@@@@@@   @                        
                           .@@@@@@@@@@@@@@@@@@@@@@@@@@@@                       
                        @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                       
                       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                    
       @    @         :@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                     
      .@@@@@@@@@@@    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                     
    .@@@@@@@@@@@@@@@ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.                     
.@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                       
.@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                       
    .@@@@@@@@@@@@@@@ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.                     
      .@@@@@@@@@@@    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                     
       @    @         :@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                     
                       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                    
                        @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@                       
                           .@@@@@@@@@@@@@@@@@@@@@@@@@@@@                       
                           .@@  @@@@@@@@@@@@@@@@@@@   @                        
                                        @@. .                                  
                                      @@@@@@@                                  
                                      @@@@@@
```
