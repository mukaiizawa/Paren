# NAME
built-in? - built-in operator judgment function.

# SYNOPSIS

    (built-in? X)

# DESCRIPTION
The function `built-in?` determine if the argument is built-in operator.

# RETURN VALUE
Returns whether the `X` is a built-in operater.

# EXAMPLES

    ) (built-in? f)
    true
    ) (built-in? +)
    true
    ) (built-in? (f (x) (+ 1 x)))
    nil
    ) (built-in? begin0)
    nil
    ) (built-in? built-in-function)
    nil

# SEE ALSO
- `built-in-name(3)`
- `special-operator(7)`
