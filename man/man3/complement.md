# NAME
complement - creating a composite function to invert boolean values.

# SYNOPSIS

    (complement FN)

# DESCRIPTION
The function `complement` creating a composite function to invert boolean values.

# RETURN VALUE
Returns a composite function to invert boolean values.

# NOTES

    (complement fn) <=> (compose ! fn)

# EXAMPLES

    ) (select zero? '(0 0 1 0 0 1 1 2 0))
    (0 0 0 0 0)
    ) (select (complement zero?) '(0 0 1 0 0 1 1 2 0))
    (1 1 1 2)

# SEE ALSO
- compose(3)
- map(3)
- reject(3)
- select(3)
- select1(3)
- partial(3)
