# NAME
memlen - get the byte length of a bytes-like object.

# SYNOPSIS

    (memlen BYTES)

# DESCRIPTION
The function `memlen` returns the byte length of a bytes-like object.

# RETURN VALUE
Returns the byte length of `BYTES`.

# EXAMPLES

    ) (memlen "foo")
    3
    ) (memlen :foo)
    3
    ) (memlen 'foo)
    3
    ) (memlen "あ")
    3

# SEE ALSO
- len(3)
- bytes-like(7)
