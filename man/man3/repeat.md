# NAME
repeat - repeat a certain element.

# SYNOPSIS

    (repeat X N)

# DESCRIPTION
The function `repeat` returns a list consisting of `N` elements `X`.

# RETURN VALUE
Returns a list consisting of `N` elements `X`.

# EXAMPLES

    ) (repeat 1 -1)
    nil
    ) (repeat 1 0)
    nil
    ) (repeat 1 3)
    (1 1 1)
    ) (join (repeat "a" 3))
    "aaa"

# SEE ALSO
- `join(3)`
