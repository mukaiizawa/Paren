# NAME
brainfuck - brainfuck interpreter.

# SYNOPSIS

    brainfuck FILE

# DESCRIPTION
Reads the program from a `FILE` and execute it as brainfuck code.

# EXAMPLES

    $ cat hello.bf
    >+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++.
    $ paren brainfuck hello.bf
    Hello, World!

# SEE ALSO
- `brainfuck(1)`
- `whitespace(1)`
