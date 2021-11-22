# NAME
symcmp - compares two objects in address order.

# SYNOPSIS

    (symcmp X Y)

# DESCRIPTION
The function `symcmp` compares two objects in address order.

# RETURN VALUE
Returns a negative integer, zero, or a positive integer as `X` is less than, equal to, or greater than `Y`.

# EXAMPLES

    ) (symcmp (symbol "foo") 'foo)
    0

    ) (symcmp 'foo 'bar)
    -21848

    ) (symcmp 'bar 'foo)
    21848

# SEE ALSO
- memcmp(3)
