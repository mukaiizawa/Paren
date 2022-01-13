# NAME
memcat - concatenate bytes-like objects.

# SYNOPSIS

    (memcat BYTES [... ARGS])

# DESCRIPTION
The function `memcat` concatenate the two or more bytes-like objects.

# RETURN VALUE
Returns the result of concatenating `X` with each `ARGS`.

# EXAMPLES

    ) (memcat nil :foo)
    nilfoo

    ) (memcat "foo" :bar)
    "foobar"

    ) (memcat :foo "bar" 'buzz)
    :foobarbuzz

# SEE ALSO
- concat(3)
- memcmp(3)
- memcpy(3)
- memlen(3)
- memmem(3)
- strcmp(3)
- bytes(7)
- bytes-like(7)
