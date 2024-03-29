# NAME
<- - valiable assignment statement.

# SYNOPSIS

    (<- BOUND-EXPR BINDING-EXPR [BOUND-EXPR BINDING-EXPR] ...)

# DESCRIPTION
The special operator `<-` bind the `BOUND-EXPR` with the result of evaluating the `BINDING-EXPR` in order from the left to right.

The symbol is bound to the already bound environment closest to the current environment.

If it is not bound to the global environment, bind to the global environment.

If `BOUND-EXPR` is a tree rather than symbol, binds the symbols specified in tree to the corresponding values in the tree structure resulting from the evaluation of expression.

# RETURN VALUE
Returns the last evaluation result.

# ERRORS
## ArgumentError
If, `BOUND-EXPR` is not a symbol or a symbol tree expression or the return value of `BINDING-EXPR` does not match `BOUND-EXPR`.

# EXAMPLES

    ) (<- a 1)
    1
    ) a
    1

    ) (<- (a b c) (list 1 2 3))
    (1 2 3)
    ) a
    1
    ) b
    2
    ) c
    3

# SEE ALSO
- `let(3)`
- `special-operator(7)`
