# NAME
byte? - predicate function for byte.

# SYNOPSIS

    (byte? X)

# DESCRIPTION
The function `byte?` is a predicate that determines if the argument can be considered a byte.

A byte is an integer between `0` and `255`.

# RETURN VALUE
Returns whether `X` is a byte.

# NOTES

    (byte? x)
    <=> (&& (int? x) (<= 0 x 255))

# EXAMPLES

    ) (byte? nil)
    nil
    ) (byte? -1)
    nil
    ) (byte? 0)
    true
    ) (byte? 0xff)
    true
    ) (byte? 0x100)
    nil

# SEE ALSO
- `int?(3)`
- `number?(3)`
