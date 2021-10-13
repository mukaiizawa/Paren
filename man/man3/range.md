# NAME
.. - generating sequences.

# SYNOPSIS

    (.. STOP)
    (.. START STOP [STEP])

# DESCRIPTION
The function `..` generate sequences which is determined by the formula:

    { x | start + step * i, i >= 0, x < stop }

# RETURN VALUE
Returns a list of numbers that increase step by `STEP` from `START` for which `START < STOP` holds.

If `START` is omitted, it is assumed that `0` is specified.

If `STEP` is omitted, it is assumed that `1` is specified.

# ERRORS
Error if `STEP` is not positive integer.

# NOTES

    (.. n) <=> (.. 0 n 1)
    (.. a b) <=> (.. a b 1)

# EXAMPLES

    ) (.. 10)
    (0 1 2 3 4 5 6 7 8 9)

    ) (.. 2 5)
    (2 3 4)

    ) (.. 0 10 2)
    (0 2 4 6 8)

# SEE ALSO
- seq(1)
