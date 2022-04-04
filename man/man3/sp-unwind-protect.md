# NAME
unwind-protect - guarantee of evaluation.

# SYNOPSIS

    (unwind-protect EXPR CLEANUP-EXPR)

# DESCRIPTION
The special operator `unwind-protect` evaluates `EXPR` and guarantees that `CLEANUP-FORMS` are executed before `unwind-protect` exits, whether it terminates normally or not.

# RETURN VALUE
Returns the evaluation result of the last `EXPR`.

# NOTES
In general, it is rare to call `unwind-protect` directly, and it is used in a macro like `with-open(3)`.

# EXAMPLES

    ) (unwind-protect (<- (i j k) (.. 3))
                      (<- (i j k) (reverse (.. 3))))
    (0 1 2)
    ) (list i j k)
    (2 1 0)

    ) (<- (i j k) (.. 3))
    (0 1 2)
    ) (while true
        (unwind-protect (begin (<- i :foo) (break) (<- j :bar))
                        (<- k :buzz)))
    nil
    ) (list i j k)
    (:foo 1 :buzz)

# SEE ALSO
- `special-operator(7)`
