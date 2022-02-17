# NAME
max, min - get the largest and smallest objects.

# SYNOPSIS

    (max X1 X2 ...)
    (min X1 X2 ...)

# DESCRIPTION
These functions retrieve the largest or smallest object from multiple comparable objects.

# RETURN VALUE
The function `max` returns maximum value from argument.

The function `min` returns minimum value from argument.

# EXAMPLES

    ) (max 0 1 2)
    2
    
    ) (min 0 1 2)
    0

    ) (max "a" "b" "c")
    "c"
    
    ) (min "a" "b" "c")
    "a"

    ) (max 'a 'b 'c)
    c
    
    ) (min 'a 'b 'c)
    a

    ) (max :a :b :c)
    :c
    
    ) (min :a :b :c)
    :a

# SEE ALSO
- comparable(7)
