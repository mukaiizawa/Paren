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
- `alnum?(3)`
- `alpha?(3)`
- `ascii?(3)`
- `bin(3)`
- `byte-len(3)`
- `chr(3)`
- `digit?(3)`
- `format(3)`
- `hex(3)`
- `index(3)`
- `join(3)`
- `last-index(3)`
- `lower(3)`
- `lower?(3)`
- `ltrim(3)`
- `oct(3)`
- `ord(3)`
- `prefix?(3)`
- `print?(3)`
- `rtrim(3)`
- `space?(3)`
- `split(3)`
- `str(3)`
- `string!(3)`
- `string(3)`
- `string?(3)`
- `suffix?(3)`
- `title(3)`
- `title?(3)`
- `trim(3)`
- `upper(3)`
- `upper?(3)`
- `wcwidth(3)`
- `data-types(7)`
