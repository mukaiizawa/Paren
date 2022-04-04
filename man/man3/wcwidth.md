# NAME
wcwidth - character display width.

# SYNOPSIS

    (wcwidth CHAR)

# DESCRIPTION
The function `wcwidth` returns the display width of a character.

# RETURN VALUE
Returns the number of columns needed to represent the character `CHAR`.

# EXAMPLES

    ) (wcwidth "\n")
    0
    ) (wcwidth "a")
    1
    ) (wcwidth "ｱ")
    1
    ) (wcwidth "あ")
    2

# BUGS
This function is only implemented for characters that are used in daily life in Japan.

# SEE ALSO
- byte-len(3)
- len(3)
