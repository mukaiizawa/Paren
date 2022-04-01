# NAME
partial - partial function application.

# SYNOPSIS

    (partial FN PARTIAL-ARG ...)

# DESCRIPTION
The function `partial` creates a function with some of the arguments to the function fixed.

# RETURN VALUE
Returns the function with some of the arguments fixed.

# EXAMPLES

    ) (apply (partial + 1) (list 2 3 4))
    10

# SEE ALSO
- complement(3)
- compose(3)
