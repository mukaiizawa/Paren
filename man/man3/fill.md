# NAME
fill! - fill the sequence with an element.

# SYNOPSIS

    (fill! ARRAY X [START [END]])
    (fill! BYTES X [START [END]])
    (fill! LIST X [START [END]])

# DESCRIPTION
The function `fill!` changes all elements in a sequence to a static value `X`.

`START` and `END` arguments follow `slice(3)` behavior.

# RETURN VALUE
Returns the first arguments.

# NOTES
It can also be called for list, but is deprecated because of its slow execution speed and because it is not recommended as a programming style.

# EXAMPLES

    ) (fill! (array 3) 1)
    #[ 1 1 1 ]
    ) (fill! (array 3) 1 2 3)
    #[ nil nil 1 ]
    ) (fill! (bytes 3) 1)
    #< 0x01 0x01 0x01 >
    ) (fill! (list 1 2 3) 1)
    (1 1 1)

# SEE ALSO
- `slice(3)`
