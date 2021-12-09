# NAME
chr, ord - converts Unicode code points to strings and vice versa.

# SYNOPSIS

    (chr CODE)
    (ord CHAR)

# DESCRIPTION
These functions converts Unicode code points to strings and vice versa.

# RETURN VALUE
The function `chr` returns a string representing a character whose Unicode code point is an integer `CODE`.

The function `oct` returns an integer representing the Unicode code point of the character.

# NOTES

    (ord (chr x)) <=> x
    (chr (ord x)) <=> x

# EXAMPLES

    ) (chr 97)
    "a"
    ) (ord "a")
    97

# SEE ALSO
- int(3)
- str(3)
