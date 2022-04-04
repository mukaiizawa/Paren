# NAME
bound? - determine if the symbol is bound.

# SYNOPSIS

    (bound? X)

# DESCRIPTION
The function `bound?` is a predicate that determines if the symbol is bound.

# RETURN VALUE
Returns whether `X` is bound.

# EXAMPLES

    ) (bound? nil)
    true
    ) (bound? 'bound?)
    true
    ) (bound? (symbol))
    nil

# SEE ALSO
- `symbol(3)`
- `symbol(7)`
