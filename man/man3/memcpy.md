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

    ) (<- foo :foo bar :bar)
    :bar
    ) (string (memcpy (bytes foo) 0 (bytes bar) 0 3))
    "foo"
    ) (string (memcpy (bytes foo) 1 (bytes bar) 1 2))
    "boo"
    ) (string (memcpy (bytes foo) 1 (bytes bar) 2 1))
    "bao"

# ERRORS
ArgumentError if `SRC` is not a bytes-like or `DST` is not a bytes.

IndexError if the reference range of the `SRC` or `DST` is incorrect.

# NOTES
Unlike the C language, it works correctly even if the areas to be copied overlap.

# SEE ALSO
- memcmp(3)
- memmem(3)
- slice(3)
- bytes(7)
