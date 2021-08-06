# NAME
<- - valiable assignment statement.

# SYNOPSIS

    (<- BOUND-EXPR BINDING-EXPR [BOUND-EXPR BINDING-EXPR] ...)

# DESCRIPTION
Bind the `BOUND-EXPR` with the result of evaluating the `BINDING-EXPR` in order from the left to right.

The symbol is bound to the already bound environment closest to the current environment.

If it is not bound to the global environment, bind to the global environment.

If `BOUND-EXPR` is a tree rather than symbol, binds the symbols specified in tree to the corresponding values in the tree structure resulting from the evaluation of expression.

# RETURN VALUE
Returns the last evaluation result.

# ERRORS
Error if, `BOUND-EXPR` is not a symbol or a symbol tree expression.

Or When the return value of `BINDING-EXPR` does not match `BOUND-EXPR`.

# EXAMPLES

    ) (<- a 1)
    1
    ) a
    1
    ) (<- (a b c) (list 1 2 3))
    3
    ) (list a b c)
    (1 2 3)

# SEE ALSO
- let(3)
- environment(7)
- special-operator(7)
