# NAME
car!, cdr! - replace the car or cdr of cons.

# SYNOPSIS

    (car! CONS X)
    (cdr! CONS Y)

# DESCRIPTION
The function `car!` replace the car of the `CONS` with `X`.

The function `cdr!` replace the cdr of the `CONS` with `Y`.

# RETURN VALUE
The function `car!` returns `X`.

The function `cdr!` returns `Y`.

# ERRORS
Error if `CONS` or `Y` is not a cons.

# NOTES
See `cons(7)` for `Y` must be cons.

# EXAMPLES

    ) (<- l (.. 3))
    (0 1 2)
    ) (car! l :foo)
    :foo
    ) l
    (:foo 1 2)
    ) (cdr! l nil)
    nil
    ) l
    (:foo)

# SEE ALSO
- car(3)
- cdr(3)
- cons(7)
