# NAME
while, for - conditional iteration of evaluation.

# SYNOPSIS

    (while TEST-EXPR
        EXPR ...)
    
    (for INIT-EXPR TEST-EXPR UPDATE-EXPR
        EXPR ...)

# DESCRIPTION
The macros `while` and `for` create a context for conditional iteration.

The macro `while` first evaluates `TEST-EXPR`.

If `TEST-EXPR` evaluates to `non-nil`, evaluates `EXPRs` in order, and then `TEST-EXPR` is evaluated again.

Repeat the above steps as long as `TEST-EXPR` evaluates to `non-nil`.

The macro `for` is the same as macro `while` except that it first builds an environment with `INIT-EXPR`, evaluates `EXPRs` under that environment, and executes binding with `UPDATE-EXPR` at the end of each loop.

# RETURN VALUE
These macros are always returns nil.

# NOTES
Since these macros are expanded into a special operator `loop(3)`, `break(3)` and `continue(3)` can be used.

    (while test expr ...)
    <=> (loop
            (if (nil? test) (break)
                (begin expr ...)))

    (for binding test update expr ...)
    <=> (let binding
            (while test
                expr ...
                (<- update)))

# EXAMPLES

    ) (<- i 0)
    0
    ) (while (< i 10) (<- i (++ i)))
    nil
    ) i
    10

    ) (<- l nil)
    nil
    ) (for (i 0 j 0) (< i 5) (i (++ i) j (% (++ j) 2))
        (push! (list i j) l))
    nil
    ) (reverse! l)
    ((0 0) (1 1) (2 0) (3 1) (4 0))

# SEE ALSO
- `break(3)`
- `continue(3)`
- `loop(3)`
