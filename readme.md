Programming Language Paren

# What is Paren

    Programs that write programs?
    When would you ever want to do that? Not very often, if you think in Cobol.
    All the time, if you think in Lisp.
    -- Paul Graham, Beating the Averages

Paren is a the programmable programming language.

- dynamic type checking
- lexical scope
- unlimited extent
- multi-paradigm
- concise syntax

# Compiling

    $ git clone https://github.com/mukaiizawa/Paren
    $ cd Paren/src & make os=unix # or os=windows
    $ ../paren
    ) "hello world"
    "hello world"

# Structure

    + coreutils/ -- Paren implementation of like the GNU coreutils programs
    + game/ -- games and toy-like programs
    + man/ -- manuals
    + misc/ -- miscellaneous programs
    + module/ -- importable Paren module files
    + src/ -- implementation of Paren itself
    + tool/ -- implementation of tool by Paren
    glossary.md  -- glossary
    lang.md  -- Paren language specification
    license.txt -- Paren license file
    tutorial.md -- tutorial of Paren

# Documentation
Paren comes with a UNIX-like manual page system.

First, you need to generate an index file with the following command.

    $ paren mandb

After creating the index file, you can refer to the manual with the man command as follows.

    $ paren man [PAGE]

You may want to check the following manuals as a starting point.

- intro(7)
- whatis(1)
- man-pages(7)
- lang(7)

# Special Thanks

- Ken'ichi Tokuoka
- Kohei Kai
