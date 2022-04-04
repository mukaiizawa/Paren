# NAME
string - string data type.

# DESCRIPTION
A string data type is used to represent and manipulate a sequence of characters.

The string data type is immutable, and every operation will generate a new string as needed.

To generate a string, you can use a string literal or a function for manipulating the string.

A string literal is a string of characters enclosed in `"`.

The string data type can use the functions listed in `SEE ALSO` section, and can also use the functions supported by `bytes-like` and `sequence`.

# EXAMPLES

    ) "hello world" ; literal
    "hello world"
    ) (slice "hello world" 6)
    "world"

# SEE ALSO
- byte-len(3)
- index(3)
- last-index(3)
- prefix?(3)
- string(3)
- suffix?(3)
- data-types(7)
