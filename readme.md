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
- multi-paradigm(expression oriented, functional, object oriented, ...)
- concise syntax

# Compiling

    $ git clone https://github.com/mukaiizawa/Paren
    $ cd Paren/src & make os=unix # or os=windows
    $ ../paren
    ) "hello world"
    "hello world"

# Structure
The main project structures are shown below.

    + algorithm/ -- implementation of various algorithms by Paren.
    + games/ -- games and toy-like programs.
    + interpreters/ -- implementation of language processing system.
    + man/ -- manual files for man(1).
    + misc/ -- miscellaneous programs.
    + modules/ -- importable Paren module files.
    + src/ -- implementation of Paren itself.
    + tools/ -- implementation of tools by Paren.
     |+ coreutils/ -- Paren implementation of like the GNU coreutils programs.
    license.txt -- Paren license file.

Since `tools` and` tools/coreutils` are included in `$runtime-path(3)`, the files in these folders can be executed as script files anywhere.

# Documentation
Paren comes with a UNIX-like manual page system. You can refer to the manual with the man command as follows.

    $ paren man [PAGE]

The manual has a `paren-tutorial(7)` page, which may be a good starting point.

You can connect it to a pipe with your favorite pager, or you can format it into an html file as follows.

    $ paren man paren-tutorial | paren md2html > tutorial.html

Paren is a tinny language, so it should be fully usable after the tutorial.

In addition, the following pages may be helpful.

- whatis(1)
- man-pages(7)
- lang(7)

# Special Thanks

- Ken'ichi Tokuoka
- Kohei Kai
