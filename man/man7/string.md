# NAME
string - string type.

# DESCRIPTION
A string type is used to represent and manipulate a sequence of characters.

The string type is immutable, and every operation will generate a new string as needed.

To generate a string, you can use a string literal or a function for manipulating the string.

A string literal is a string of characters enclosed in `"`.

In addition, a series of built-in functions for `sequence(7)` can be used for strings.

# EXAMPLES

    ) "hello world" ; literal
    "hello world"
    ) (slice  "hello world" 6)
    "world"

# SEE ALSO
- string(3)
- sequence(7)
