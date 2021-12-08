# NAME
bin, oct, hex - string representation with radix conversion.

# SYNOPSIS

    (bin X)
    (oct X)
    (hex X)

# DESCRIPTION
These functions convert integers to their string representation by radix conversion.

# RETURN VALUE
The function `bin` returns a binary representation string of `X`.

The function `oct` returns a octal representation string of `X`.

The function `hex` returns a hexdecimal representation string of `X`. If `X` is a bytes, it returns a hexadecimal dump string of `X`.

# NOTES
Use `format(3)` for more sensitive output.

# EXAMPLES

    ) (bin 10)
    "2x1010"

    ) (oct 10)
    "8x12"

    ) (hex 10)
    "0xa"

    ) (hex (bytes "foo"))
    "666f6f"

# SEE ALSO
- format(3)
