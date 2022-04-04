# NAME
built-in-name - get the name of the built-in operator.

# SYNOPSIS

    (built-in-name OPERATOR)

# DESCRIPTION
The function `built-in-name` get the name of the built-in operator.

# RETURN VALUE
Returns the name of the built-in operator `OPERATOR`.

If the argument is not a built-in operator, return `nil`.

# EXAMPLES

    ) (built-in-name f)
    f
    ) (built-in-name -)
    nil

# SEE ALSO
- `built-in?(3)`
