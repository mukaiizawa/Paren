# NAME
join - concatenate strings using the separator string.

# SYNOPSIS

    (join LIST [SEPARATOR])

# DESCRIPTION
The function `join` create string by concatenating all of the elements in the list, separated by a specified separator.

If `SEPARATOR` is omitted, the `LIST` elements are just concatenated.

# RETURN VALUE
Returns a string by concatenating all of the elements in the `LIST`, separated by `SEPARATOR`.

If `LIST` is `nil`, returns empty string.

# EXAMPLES

    ) (join nil)
    ""

    ) (join '("foo"))
    "foo"

    ) (join '("foo" "bar" "buzz"))
    "foobarbuzz"

    ) (join '("foo" "bar" "buzz") "::")
    "foo::bar::buzz"

# SEE ALSO
- split(3)
