# NAME
bytes - make a bytes.

# SYNOPSIS

    (bytes SIZE)
    (bytes BYTES-LIKE [START [END]])

# DESCRIPTION
The function `bytes` make a bytes.

# RETURN VALUE
If the first argument is an integer, returns a bytes of size `SIZE`. In this case, the contents are guaranteed to be all 0.

If the first argument is a bytes-like object, a bytes consisting of the `START-th` through `(END - 1)-th` `BYTES-LIKE` is returned.

If `END` is omitted, the corresponding bytes from the `START` to the end `BYTES-LIKE` is returned.

If `START` is omitted, a bytes corresponding to `BYTES-LIKE` is returned.

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
- bytes-like(7)
