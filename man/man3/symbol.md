# NAME
symbol - make a symbol.

# SYNOPSIS

    (symbol [NAME])

# DESCRIPTION
The function `symbol` make a symbol.

# RETURN VALUE
Returns the a symbol whose name is `NAME`.

If `NAME` is omitted, a numbered symbol starting with `$G-` is returned.

# NOTES
Symbols with no arguments are used to prevent unintended symbol binding in macro expansion.

# EXAMPLES

    ) (symbol "foo")
    foo
    ) (== (symbol "bar") 'bar)
    true

    ) (symbol)
    $G-3015
    ) (symbol)
    $G-3016
    ) (symbol)
    $G-3017

# SEE ALSO
- keyword(3)
- with-gensyms(3)
- symbol(7)
