# NAME
let - create new envirionment.

# SYNOPSIS

    (let ([BOUND-EXPR BINDING-EXPR] ...)
        EXPR ...)

# DESCRIPTION
The special operator `let` create new variable bindings and execute a series of `EXPRs` that use these bindings.

From left to right, bind the evaluation result of `BINDING-EXPR` to `BOUND-EXPR`.

After all the bindings are done, `EXPR` is evaluated in order from the left.

# RETURN VALUE
Returns the evaluation result of the last `EXPR`.

# NOTES
Like `let*` in Common Lisp, symbols bound to a newly created environment affect the evaluation of subsequent bindings.

# EXAMPLES

    ) (<- foo :foo bar :bar)
    :bar
    ) (let (foo :foo2) foo)
    :foo2
    ) (let (foo :foo2 bar (list foo bar)) (list foo bar))
    (:foo2 (:foo2 :bar))
    ) (let ((foo bar) (list foo bar)) (list foo bar))
    (:foo :bar)

# SEE ALSO
- `<-(3)`
- `special-operator(7)`
