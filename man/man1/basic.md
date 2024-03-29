# NAME
basic - basic interpreter.

# SYNOPSIS

    basic [OPTION] FILE

# DESCRIPTION
Reads the program from a `FILE` and runs it with classic Microsoft BASIC compatibility.

### Statements
- DATA
- DEF
- DIM
- END
- FOR
- GOSUB
- GOTO
- IF
- INPUT
- LET
- NEXT
- ON
- PRINT
- READ
- REM
- RESTORE
- RETURN
- STOP

### Functions
- ABS
- ASC
- ATN
- CHR$
- COS
- EXP
- INT
- LEFT$
- LEN
- LOG
- MID$
- NOT
- RIGHT$
- RND
- SGN
- SIN
- SPC
- SQR
- STR$
- TAN
- TAB
- VAL

### Operators
- (unary) -
- ^
- *, /
- +, -
- =, <, >, <>, <=, >=
- AND
- OR

# OPTIONS

    -i
        Display the conversion result to the intermediate format and exit.

# EXAMPLES

    $ cat hello.bas
    10 PRINT "Hello, World!"
    20 END
    $ paren basic hello.bas
    Hello, World!

# SEE ALSO
- `brainfuck(1)`
- `whitespace(1)`
