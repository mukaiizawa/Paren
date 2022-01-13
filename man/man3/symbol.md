# NAME
symbol - make a symbol.

# SYNOPSIS

    (symbol [NAME [START [END]]])

# DESCRIPTION
The function `symbol` make a symbol.

# RETURN VALUE
Returns a symbol consisting of the `START-th` through `(END - 1)-th` bytes-like object `NAME`.

If `END` is omitted, the corresponding symbol from the `START` to the end is returned.

If `START` is omitted, a symbol whose name is `NAME` is returned.

If `NAME` is omitted, a numbered symbol starting with `$G-` is returned.

# NOTES
Symbols with no arguments are used to prevent unintended symbol binding in macro expansion.

# EXAMPLES

    ) (symbol)
    $G-3015
    ) (symbol)
    $G-3016
    ) (symbol)
    $G-3017

    ) (symbol "foo")
    foo
    ) (symbol "foo" 2)
    o
    ) (symbol "foo" 1 3)
    oo

# SEE ALSO
- keyword(3)
- with-gensyms(3)
- bytes-like(7)
- symbol(7)
