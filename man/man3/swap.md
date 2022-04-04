# NAME
swap! - swapping two elements.

# SYNOPSIS

    (swap! LIST I J)
    (swap! ARRAY I J)
    (swap! BYTES I J)

# DESCRIPTION
The function `swap!` destructively swaps the `I-th` and `J-th` elements of the first argument.

# RETURN VALUE
Returns the first argument.

# NOTES
The first argument is modified.

# EXAMPLES

    ) (swap! #[ 0 1 2 ] 0 1)
    #[ 1 0 2 ]

    ) (swap! (.. 3) 0 1)
    (1 0 2)

# SEE ALSO
- `sort!(3)`
