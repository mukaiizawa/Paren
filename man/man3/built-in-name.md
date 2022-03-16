# NAME
built-in-name - get the name of the built-in operator.

# SYNOPSIS

    (built-in-name OPERATOR)

# DESCRIPTION
The function `built-in-name` get the name of the built-in operator.

# RETURN VALUE
Returns the name of the built-in operator `OPERATOR`.

# ERRORS
Error if `OPERATOR` is not a built-in operator. This means that it is an error if the following expression does not return true

    (built-in? OPERATOR)

# EXAMPLES

    ) (built--name f)
    f
    ) (built-in? +)
    +

# SEE ALSO
- built-in?(3)
