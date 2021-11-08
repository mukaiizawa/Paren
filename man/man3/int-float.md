# NAME
int, int32, float - convert to numeric type.

# SYNOPSIS

    (int X)
    (int32 X)
    (float X)

# DESCRIPTION
These functions convert a string or number to the specified numeric type.

# RETURN VALUE
The function `int` returns an integer constructed from a number or string `X`.

The function `int32` returns a 32 bit integer constructed from a number or string `X`.

The function `float` returns a float constructed from a number or string `X`.

If `X` is `nil`, these functions returns `0`.

# NOTES

    (int32 x) <=> (& 0xffffffff (int x))

# EXAMPLES

    ) (int nil)
    0
    ) (int32 nil)
    0
    ) (float nil)
    0

    ) (int 1)
    1
    ) (int32 1)
    1
    ) (float 1)
    1

    ) (int "3.14")
    3
    ) (int32 "3.14")
    3
    ) (float "3.14")
    3.14

    ) (int 0xfffffffff)
    68719476735
    ) (int32 0xfffffffff)
    4294967295
    ) (float 0xfffffffff)
    68719476735

# SEE ALSO
- int?(3)
