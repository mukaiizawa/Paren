# NAME
dynamic - dynamic symbol resolution.

# SYNOPSIS

    (dynamic SYMBOL)

# DESCRIPTION
Dynamically resolve the SYMBOL.

# RETURN VALUE
Returns the result of dynamically resolving the SYMBOL.

# ERRORS
Error if, SYMBOL is not bound.

# EXAMPLES

    ) (<- foo :global)
    :global
    ) (function static-resolution () (list foo))
    static-resolution
    ) (function dynamic-resolution () (list (dynamic foo)))
    dynamic-resolution
    ) (static-resolution)
    (:global)
    ) (dynamic-resolution)
    (:global)
    ) (let (foo :lexical) (static-resolution))
    (:global)
    ) (let (foo :lexical) (dynamic-resolution))
    (:lexical)

# SEE ALSO
- special-operator(7)