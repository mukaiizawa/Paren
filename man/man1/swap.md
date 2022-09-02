# NAME
swap - swap files.

# SYNOPSIS

    swap [OPTION] [SRC DST]...

# DESCRIPTION
Exchange multiple specified file names. Properly process even if they are circular.

Backup recommended before execution.

# OPTIONS

    -f FILE
        Read the exchange target from the CSV FILE.
    -n
        Dry-run.

# NOTES
If there are many arguments, the `-f` option is valid.

# EXAMPLES
Replace files `a` and `b`.

    $ paren swap a b

Replace file `x` with `y`, `y` with `z`, and `z` with `x`.

    $ paren swap x y y z
