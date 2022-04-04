# NAME
tr - translate or delete characters.

# SYNOPSIS

    tr SET1 [SET2]

# DESCRIPTION
Translate, squeeze, and/or delete characters from standard input, writing to standard output.

`SET1` and `SET2` are specified as strings of characters.

Most represent themselves.

Interpreted sequences are:

    CHAR1-CHAR2
        All characters from CHAR1 to CHAR2 in ascending order.

# EXAMPLES
Delete numbers.

    paren tr 0-9

Convert uppercase to lowercase.

    paren tr A-Z a-z

# SEE ALSO
- `coreutils(7)`
