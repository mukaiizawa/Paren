# NAME
foreach - apply the function to each element of the list.

# SYNOPSIS

    (foreach FN LIST)

# DESCRIPTION
The function `foreach` executes a provided function once for each list element.

# RETURN VALUE
Returns `nil`.

# NOTES
In general, `foreach` is faster than `map`, because it doesn't create cons.

# EXAMPLES

    ) (foreach write (.. 10))
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    nil

# SEE ALSO
- dolist(3)
- map(3)
- higher-order-function(7)
