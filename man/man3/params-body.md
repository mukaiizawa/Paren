# NAME
params, body - accessor of the procedure.

# SYNOPSIS

    (params PROC)
    
    (body PROC)

# DESCRIPTION
These functions are for getting the definition of the function.

# RETURN VALUE
The function `params` returns a parameters of function or macro `PROC`.

The function `body` returns a body of function or macro `PROC`.

# EXAMPLES

    ) (params (f (x y z) (list x y z)))
    (x y z)
    ) (params (macro foo (((x) y) z) (list x y z)))
    (((x) y) z)

    ) (body (f (x y z) (list x y z)))
    ((list x y z))
    ) (body (macro foo (((x) y) z) (list x y z)))
    ((list x y z))

# SEE ALSO
- function(7)
- macro(7)
