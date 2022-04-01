# NAME
memcmp - compare two bytes-like objects.

# SYNOPSIS

    (memcmp X Y)

# DESCRIPTION
The function `memcmp` compares the two bytes-like objects.

# RETURN VALUE

If `X` is equals to `Y`, returns `0`.

If `X` is lexicographically less than `Y`, returns `-1`.

If `X` is lexicographically greater than `Y`, returns `1`.

# EXAMPLES

    ) (memcmp "bar" "foo")
    -1
    ) (memcmp "foo" "bar")
    1
    ) (memcmp "foo" "foo")
    0
    ) (memcmp "fo" "foo")
    -1
    ) (memcmp "foo" "fo")
    1

# NOTES
Use `strcmp(3)` to compare strings in units of characters.

# SEE ALSO
- <(3)
- <=(3)
- >(3)
- >=(3)
- memcpy(3)
- memlen(3)
- memmem(3)
- bytes(7)
