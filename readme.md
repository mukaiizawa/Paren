Programming Language Paren

# What is Paren

    Programs that write programs?
    When would you ever want to do that?
    Not very often, if you think in Cobol.
    All the time, if you think in Lisp.
    -- Paul Graham, Beating the Averages

Paren is a dialect of the Lisp programming languages:

- dynamic type checking
- lexical scope
- unlimited extent
- multi-paradigm(expression oriented, functional, object oriented, ...)
- concise syntax

# Compiling
For windows, MSYS2+MinGW-w64 environment or Docker described below is recommended.

    $ git clone https://github.com/mukaiizawa/Paren && cd ./Paren/src
    $ make os=unix # or os=windows
    $ ../paren
    ) "hello world"
    "hello world"

# Docker
Only the first time, you will create an image with the following command.

    $ git clone https://github.com/mukaiizawa/Paren && cd ./Paren
    $ docker image build --no-cache --tag paren .

Run a Docker container based on this image.

    $ docker container run -it --rm --name paren paren
    root@29e9c4143be0:/~# paren
    ) "hello-world"
    "hello-world"

# Structure
The main project structures are shown below.

    + man/ -- manual files for man(1).
    + misc/ -- miscellaneous programs.
        + algorithm/ -- implementation of various algorithms by Paren.
        + games/ -- games and toy-like programs.
    + modules/ -- importable Paren module files.
    + scripts/ -- some useful script files, including POSIX-like ones.
    + src/ -- implementation of Paren itself.
    license.txt -- Paren license file.

Since `scripts` are included in `$runtime-path(3)`, the files in these folders can be executed as script files anywhere.

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
- Miki Yonemura
- Nobuaki Akasaka
