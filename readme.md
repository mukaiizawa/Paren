Programming Language Paren

# What is Paren

    Programs that write programs? When would you ever want to do that? Not very often, if you think in Cobol.
    All the time, if you think in Lisp.
    -- Paul Graham, Beating the Averages

Paren is a the programmable programming language.

- dynamic type checking
- lexical scope
- unlimited extent
- multi-paradigm
- concise syntax

# Compiling
## Unix

    $ git clone https://github.com/mukaiizawa/Paren
    $ cd Paren/src & make os=unix
    $ ../paren
    ) "hello world"
    "hello world"

## Windows

    $ git clone https://github.com/mukaiizawa/Paren
    $ cd Paren/src & make os=windows
    $ ../paren
    ) "hello world"
    "hello world"

# Structure

    + coreutils -- Paren implementation of like the GNU coreutils programs
    + game -- games and toy-like programs
    + misc -- miscellaneous programs
    + module -- importable Paren module files
    + src -- implementation of Paren itself
    + tool -- implementation of tool by Paren
    glossary.md  -- glossary
    lang.md  -- Paren language specification
    license.txt -- Paren license file
    tutorial.md -- tutorial of Paren

# Special Thanks

- Ken'ichi Tokuoka
- Kohei Kai
