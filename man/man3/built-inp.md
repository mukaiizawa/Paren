# NAME
built-in? - built-in operator judgment function.

# SYNOPSIS

    (built-in? X)

# DESCRIPTION
The word built-in operator means special operater or built-in function.

# RETURN VALUE
Returns whether the `X` is a built-in operater.

# EXAMPLES

    ) (built-in? f)
    true
    ) (built-in? +)
    true
    ) (built-in? ++)
    nil
    ) (built-in? begin0)
    nil
    ) (built-in? built-in-function)
    nil

# SEE ALSO
- built-in-function(7)
- special-operator(7)
