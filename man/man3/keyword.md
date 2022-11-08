# NAME
keyword - make a keyword.

# SYNOPSIS

    (keyword NAME [START [END]])

# DESCRIPTION
The function `keyword` make a keyword.

# RETURN VALUE
Returns a keyword consisting of the `START-th` through `(END - 1)-th` bytes-like object `NAME`.

If `END` is omitted, the corresponding keyword from the `START` to the end is returned.

If `START` is omitted, a keyword whose name is `NAME` is returned.

# EXAMPLES

    ) (keyword "foo")
    :foo
    ) (keyword "foo" 2)
    :o
    ) (keyword "foo" 1 3)
    :oo

# SEE ALSO
- `symbol(3)`
- `symbol(7)`
- `with-gensyms(3)`
