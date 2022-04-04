# NAME
hash - returns hash code value.

# SYNOPSIS

    (hash X)

# DESCRIPTION
The function `hash` returns hash code value.

The return value is always `0`, except for the following types:

- number
- string
- symbol
- keyword

Therefore, it is not recommended to use any type other than the above for `dictionary(7)` keys.

# RETURN VALUE
Returns a hash code value of the `X`.

# NOTES

    (= X Y) => (= (hash X) (hash Y))

# EXAMPLES

    ) (hash (dict))
    0
    ) (hash (f () nil))
    0

# SEE ALSO
- `=(3)`
