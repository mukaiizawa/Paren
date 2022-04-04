# NAME
member - search list.

# SYNOPSIS

    (member X LIST)

# DESCRIPTION
The function `member` search `LIST` for item `X`.

# RETURN VALUE
Return the cons of the first occurrence of `X` in the list.

Otherwise `nil` is returned.

# EXAMPLES

    ) (member 3 (.. 10))
    (3 4 5 6 7 8 9)
    ) (member -1 (.. 10))
    nil

# SEE ALSO
- assoc(3)
- index(3)
