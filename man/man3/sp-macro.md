# NAME
macro - create a macro.

# SYNOPSIS

    (macro NAME (PARAMS)
        EXPR ...)
    
    PARAMS = [ { PARAMS | REQUIRED-PARAM } ...]
             [:opt OPTIONAL-PARAM ...]
             [:key KEYWORD-PARAM ... | :rest REST-PARAM]

# DESCRIPTION
The special operator `macro` create a macro.

In addition to the fact that formal arguments can be specified in the same way as `f(3)`, a nested structure can be defined.

Read `paren-tutorial(7)` for details.

# SEE ALSO
- paren-tutorial(7)
- special-operator(7)
