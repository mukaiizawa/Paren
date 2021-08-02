# NAME
begin - evaluates expressions.

# SYNOPSIS

    (begin EXPR...)

# DESCRIPTION
`begin` evaluates EXPR, in the order in which they are given.

The values of each form but the last are discarded.

# RETURN VALUE
Returns the last evaluation result.

# EXAMPLES

    ) (begin 1 2 3)
    3

# SEE ALSO
- special-operator(7)
