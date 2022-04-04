# NAME
lower, upper, title - convert letter case.

# SYNOPSIS

    (lower X)
    (upper X)
    (title X)

# DESCRIPTION
These functions convert the letter case of a string.

# RETURN VALUE
The function `lower` returns the string with all the cased characters converted to lowercase.

The function `upper` returns the string with all the cased characters converted to uppercase.

The function `title` returns the string witch the first letter in each word upper case.

# EXAMPLES

    ) (lower "FoO bAr")
    "foo bar"
    ) (upper "FoO bAr")
    "FOO BAR"
    ) (title "FoO bAr")
    "Foo Bar"

# SEE ALSO
- `lower?(3)`
- `title?(3)`
- `upper?(3)`
