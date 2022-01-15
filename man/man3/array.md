# NAME
array - make an array.

# SYNOPSIS

    (array SIZE)
    (array SEQUENCE)

# DESCRIPTION
The function `array` make a array.

# RETURN VALUE
If the `SIZE` is a positive integer, returns an array initialized with `nil` of size `SIZE`.

If the `SEQUENCE` is a sequence, returns the corresponding array.

# EXAMPLES

    ) (array 3)
    #[ nil nil nil ]

    ) (array (bytes 3))
    #[ 0 0 0 ]
    
    ) (array (.. 3))
    #[ 0 1 2 ]
    
    ) (array "foo")
    #[ "f" "o" "o" ]

# SEE ALSO
- bytes(3)
- list(3)
- string(3)
- array(7)
- sequence(7)
