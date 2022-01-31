# NAME
swap! - swapping two elements.

# SYNOPSIS

    (swap! SEQUENCE I J)

# DESCRIPTION
The function `swap!` destructively swaps the `I-th` and `J-th` elements of the `SEQUENCE`.

# RETURN VALUE
Returns `SEQUENCE`.

# ERRORS
Error if `SEQUENCE` is not a mutable sequence.

# NOTES
The `SEQUENCE` is modified.

Since the string data type is an immutable sequence, the `swap!` function cannot be used. If you really want to swap the characters, you can convert it to a mutable sequence instead, as shown below, and then reverse the conversion.

    ) (<- s "foo")
    "foo"
    
    ) (string (swap! (bytes "foo") 0 1))
    "ofo"
    
    ) (join (swap! (split "foo") 0 1))
    "ofo"

# EXAMPLES

    ) (swap! #[ 0 1 2 ] 0 1)
    #[ 1 0 2 ]

    ) (swap! (bytes "foo") 0 1)
    #[ 0x6f 0x66 0x6f ]

    ) (swap! (.. 3) 0 1)
    (1 0 2)

# SEE ALSO
- sort!(3)
- sequence(7)
