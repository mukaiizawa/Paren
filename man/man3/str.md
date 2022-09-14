# NAME
str - make a human-readable string.

# SYNOPSIS

    (str X ...)

# DESCRIPTION
The function `str` converts an arbitrary object `X` to a string intended to be human-readable.

If more than one argument is given, a concatenated string is returned.

# RETURN VALUE
Returns the string whitch intended to look good to people.

# NOTES
`nil` will be converted to an empty string.

# EXAMPLES

    ) (str)
    ""
    ) (str nil)
    ""
    ) (str 1 "2" :three)
    "12three"
    ) (str (.. 3))
    "(0 1 2)"

# SEE ALSO
- `string(3)`
- `print(3)`
