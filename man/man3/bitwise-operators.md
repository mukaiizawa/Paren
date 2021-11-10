# NAME
~, &, |, ^, <<, >> - bitwise operators.

# SYNOPSIS

    (~ X)
    (& X Y)
    (| X Y)
    (^ X Y)
    (<< X N)
    (>> X N)

# DESCRIPTION
These functions are basic bitwise operators.

# RETURN VALUE
The function `~` returns bitwise NOT of `X`.

The function `&` returns bitwise AND of `X` and `Y`.

The function `|` returns bitwise OR of `X` and `Y`.

The function `^` returns bitwise XOR of `X` and `Y`.

The function `<<` returns the value which shift the `X` of `N` bits to the left, logicaly.

The function `>>` returns the value which shift the `X` of `N` bits to the right, logicaly.

These functions only accept positive integers. Error if `X` is not positive integer.

# EXAMPLES

    ) (bin (~ 2x1010))
    "2x111111111111111111111111111111111111111111111111111111111110101"
    ) (bin (~ (~ 2x1010)))
    "2x1010"

    ) (bin (& 2x1010 2x0110))
    "2x10"

    ) (bin (| 2x1010 2x0110))
    "2x1110"

    ) (bin (^ 2x1010 2x0110))
    "2x1100"

    ) (bin (<< 2x1010 2))
    "2x101000"

    ) (bin (>> 2x1010 2))
    "2x10"

# SEE ALSO
- +(3)
- -(3)
