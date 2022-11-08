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
        Display the list of supported Instruction Modification Parameters(IMP) and exit the program.
    -S CHAR
        Use the CHAR instead of space.
    -T CHAR
        Use the CHAR instead of tab.

# NOTES
The L, S, T options support only one character.

# EXAMPLES

    $ cat hello.ws
    ssstsstsssl
    tl
    sssssttsststl
    tl
    sssssttsttssl
    tl
    sssssttsttssl
    tl
    sssssttsttttl
    tl
    ssssstsssssl
    tl
    ssssstststttl
    tl
    sssssttsttttl
    tl
    ssssstttsstsl
    tl
    sssssttsttssl
    tl
    sssssttsstssl
    tl
    sssssssststsl
    tl
    ssl
    l
    l
    
    $ paren whitespace.p -Ss -Tt -Ll hello.ws
    Hello World

# SEE ALSO
- `basic(1)`
- `brainfuck(1)`
