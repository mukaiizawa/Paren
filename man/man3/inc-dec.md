# NAME
++, -- - increment/decrement operators.

# SYNOPSIS

    (++ X)
    (-- X)

# DESCRIPTION
These functions return a numeric value with the argument incremented or decremented.

# RETURN VALUE
The function `++` returns `X` plus `1`.

The function `--` returns `X` minus `1`.

# NOTES

    (++ x) <=> (+ x 1)
    (-- x) <=> (- x 1)

# EXAMPLES

    ) (++ 0)
    1
    ) (-- 0)
    -1

# SEE ALSO
- `+(3)`
- `-(3)`
