# NAME
join - concatenate all of the elements, separated by separator.

# SYNOPSIS

    (join LIST [SEPARATOR])

# DESCRIPTION
The function `join` create string by concatenating all of the elements in the list, separated by a specified separator.

If `SEPARATOR` is omitted, the `LIST` elements are just concatenated.

Non-string elements are converted to human readable strings and then combined. Therefore, the `nil` is considered an empty string.

# RETURN VALUE
Returns a string by concatenating all of the elements in the `LIST`, separated by `SEPARATOR`.

# EXAMPLES

    ) (join nil)
    ""

    ) (join '("foo"))
    "foo"

    ) (join '("foo" "bar" "buzz"))
    "foobarbuzz"

    ) (join '("foo" "bar" "buzz") "::")
    "foo::bar::buzz"

    ) (join '(1 nil "buzz") ",")
    "1,,buzz"

# SEE ALSO
- `split(3)`
- `print(3)`
