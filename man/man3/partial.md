# NAME
partial - partial function application.

# SYNOPSIS

    (partial FN PARTIAL-ARG ...)

# DESCRIPTION
The function `partial` creates a function with some of the arguments to the function fixed.

# RETURN VALUE
Returns the function with some of the arguments fixed.

# EXAMPLES

    ) (<- plus1 (partial + 1))
    (f (:rest args) (apply fn (concat partial-args args)))
    ) (plus1 2)
    3

# SEE ALSO
- complement(3)
- compose(3)
