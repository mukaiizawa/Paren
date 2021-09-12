# NAME
cons - create a cons.

# SYNOPSIS

    (cons X Y)

# DESCRIPTION
The function cons creates a cons.

# RETURN VALUE
Returns a cons, the car of which is `X` and the cdr of which is `Y`.

# ERRORS
Error if `Y` is not cons.

# EXAMPLES

    ) (cons 1 nil)
    (1)
    ) (cons 1 (cons 2 nil))
    (1 2)

# SEE ALSO
- car(3)
- cdr(3)
- list(3)
- cons(7)
