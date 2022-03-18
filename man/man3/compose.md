# NAME
compose - create composite function.

# SYNOPSIS

    (compose FN ...)

# DESCRIPTION
The function `compose` creates a compose function.

The functions are composed from right to left.

# RETURN VALUE
Returns the composite function.

# EXAMPLES

    ) (begin (<- third (compose car cdr cdr)) (third (.. 10)))
    2
    ) (caddr (.. 10))
    2

# SEE ALSO
- complement(3)
- map(3)
- reject(3)
- select(3)
- select1(3)
- partial(3)
