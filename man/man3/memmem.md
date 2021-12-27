# NAME
memmem - locate a bytes.

# SYNOPSIS

    (memmem BYTES SUBBYTES [START [END]])

# DESCRIPTION
The function `memmem` returns the position where subbytes appears from bytes.

# RETURN VALUE
Returns the position where the `SUBBYTES` appears first in the `BYTES`.

If `START` or `END` are specified, the search is performed on subbytes of `BYTES` according to the slice notation. However, the return value index will be the distance from the beginning of the original bytes.

If `START` or `END` is omitted, the entire `BYTES` will be searched. This implies that `START` is `0` and `END` is `(len bytes)`.

# EXAMPLES

    ) (memmem "012" "1")
    1

    ) (memmem "012" "1" 1)
    1
    ) (memmem "012" "1" 2)
    nil

    ) (memmem "012" "12" 0 1)
    nil
    ) (memmem "012" "12" 0 2)
    nil
    ) (memmem "012" "12" 1 3)
    1

# NOTES

    (memmem x y s e)
    <=> (memmem (slice x s e) y)

# SEE ALSO
- memcat(3)
- memcmp(3)
- memcpy(3)
- memlen(3)
- slice(3)
