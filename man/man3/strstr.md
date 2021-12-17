# NAME
strstr, strlstr - locate a substring.

# SYNOPSIS

    (strstr TEXT PATTERN [START])
    (strlstr TEXT PATTERN)

# DESCRIPTION
These functions finds the first occurrence of the substring `PATTERN` in the string `TEXT`.

# RETURN VALUE
The function `strstr` returns the position where the substring `PATTERN` appears first in the string `TEXT`. If `START` is specified, search for substring `PATTERN` from `START` th of the string `TEXT`.

The function `strlstr` returns the position where the substring `PATTERN` appears last in the string `TEXT`.

If the string `PATTERN` is not a substring of the string `TEXT`, these functions returns `nil`.

# EXAMPLES

    ) (strstr "foo bar buzz" "bar")
    4
    ) (strlstr "foo bar buzz" "bar")
    4

    ) (strstr "foo bar buzz" " ")
    3
    ) (strlstr "foo bar buzz" " ")
    7

    ) (strstr "foo bar buzz" "spam")
    nil
    ) (strlstr "foo bar buzz" "spam")
    nil

    ) (strstr "foo bar buzz" " b")
    3
    ) (strstr "foo bar buzz" " b" 5)
    7

# SEE ALSO
- re(3)
