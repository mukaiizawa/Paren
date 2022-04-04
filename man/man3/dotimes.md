# NAME
dotimes - iterates over a series of integers.

# SYNOPSIS

    (dotimes (VAR VAL)
        EXPR ...)

# DESCRIPTION
The macro `dotimes` creates a context that iterates over a series of integers.

First evaluate the `VAL`, which should produce a integer.

After that, `VAR` is bound in order for integers from `0` to `VAL - 1`, and `EXPR`s is evaluated.

# RETURN VALUE
Returns the `nil`.

# NOTES
Supports `break(3)` and  `continue(3)`.

# EXAMPLES

    ) (<- sum 0)
    0
    ) (dotimes (i 10) (<- sum (+ sum i)))
    nil
    ) sum
    45

# SEE ALSO
- `break(3)`
- `continue(3)`
- `doarray(3)`
- `dolist(3)`
- `dostring(3)`
