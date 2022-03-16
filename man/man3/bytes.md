# NAME
bytes - make a bytes.

# SYNOPSIS

    (bytes SIZE)
    (bytes BYTES [START [END]])
    (bytes KEYWORD [START [END]])
    (bytes STRING [START [END]])
    (bytes SYMBOL [START [END]])

# DESCRIPTION
The function `bytes` make a bytes.

# RETURN VALUE
If the first argument is an integer, returns a bytes of size `SIZE`. In this case, the contents are guaranteed to be all 0.

If the argument is a bytes, keyword, string, or symbol, returns a new bytes consisting of the `START-th` through `(END - 1)-th` elements of this argument as if it were a byte sequence.

If `END` is omitted, it is assumed that the length of the argument object is specified.

If `START` is omitted, a bytes corresponding to argument is returned.

# EXAMPLES

    ) (bytes 3)
    #[ 0x00 0x00 0x00 ]

    ) (bytes 'foo)
    #[ 0x66 0x6f 0x6f ]
    
    ) (bytes 'foo 1)
    #[ 0x6f 0x6f ]
    
    ) (bytes 'foo 1 2)
    #[ 0x6f ]

# SEE ALSO
- keyword(3)
- string(3)
- symbol(3)
- bytes(7)
