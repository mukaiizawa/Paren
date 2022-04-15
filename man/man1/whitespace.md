# NAME
whitespace - whitespace interpreter.

# SYNOPSIS

    whitespace [OPTION] ... FILE

# DESCRIPTION
Reads the whitespace program from a `FILE` and interpret.

# OPTIONS

    -d
        Start in debug mode.
        In debug mode, in addition to outputting the intermediate format at the time of code generation, it also outputs the internal stack for each step at the time of execution.
    -L CHAR
        Use the CHAR instead of LF.
    -s
        Display the list of supported IMPs and exit the program.
    -S CHAR
        Use the CHAR instead of space.
    -T CHAR
        Use the CHAR instead of tab.

# NOTES
The L, S, T options support only one character.

# EXAMPLES

    $ cat hello.ws
    SSSTSSTSSS
    T
    SSSSSTTSSTST
    T
    SSSSSTTSTTSS
    T
    SSSSSTTSTTSS
    T
    SSSSSTTSTTTT
    T
    SSSSSTSSSSS
    T
    SSSSSTSTSTTT
    T
    SSSSSTTSTTTT
    T
    SSSSSTTTSSTS
    T
    SSSSSTTSTTSS
    T
    SSSSSTTSSTSS
    T
    SSSSSSSSTSTS
    T
    SS
    
    
    
    $ paren whitespace.p -SS -TT hello.ws
    Hello World

# SEE ALSO
- `basic(1)`
- `brainfuck(1)`
