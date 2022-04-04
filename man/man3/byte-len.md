# NAME
byte-len - get the byte length.

# SYNOPSIS

    (byte-len BYTES)
    (byte-len KEYWORD)
    (byte-len STRING)
    (byte-len SYMBOL)

# DESCRIPTION
The function `byte-len` returns the byte length.

# RETURN VALUE
Returns the byte length.

# EXAMPLES

    ) (byte-len "foo")
    3
    ) (byte-len :foo)
    3
    ) (byte-len 'foo)
    3
    ) (byte-len "あ")
    3

# SEE ALSO
- `len(3)`
