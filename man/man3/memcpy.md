# NAME
memcpy - copy memory area.

# SYNOPSIS

    (memcpy SRC SRC-START DST DST-START SIZE)

# DESCRIPTION
The function `memcpy` copy the `SIZE` bytes from the `SRC-START-th` of the bytes-like object `SRC` to the `DST-START-th` of the bytes `DST`.

The destination must be bytes, not bytes-like objects.

# RETURN VALUE
Returns the `DST`.

# EXAMPLES

    ) (<- src #< 0x01 0x02 0x03 0x04 0x05 0x06 > dst (bytes 6))
    #< 0x00 0x00 0x00 0x00 0x00 0x00 >
    ) (memcpy src 0 dst 0 3)
    #< 0x01 0x02 0x03 0x00 0x00 0x00 >
    ) (memcpy src 3 dst 3 3)
    #< 0x01 0x02 0x03 0x04 0x05 0x06 >
    ) dst
    #< 0x01 0x02 0x03 0x04 0x05 0x06 >

# ERRORS
ArgumentError if `SRC` is not a bytes-like or `DST` is not a bytes.

IndexError if the reference range of the `SRC` or `DST` is incorrect.

# NOTES
Unlike the C language, it works correctly even if the areas to be copied overlap.

# SEE ALSO
- `memcmp(3)`
- `slice(3)`
- `bytes(7)`
