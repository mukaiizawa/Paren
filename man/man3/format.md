# NAME
format - formatted output conversion.

# SYNOPSIS

    (format FORMATTER [ARGS ...])

# DESCRIPTION
The function `format` performs format processing equivalent to `sprintf(3)` in the C language.

The `FORMATTER` format is as follows.

    %[FLAG...][WIDTH][.[PRECISION]]CONV

## FLAG
`FLAG` is an character, which modify the meaning of the conversion specification.

    + -- always print a sign for numeric values
    - -- pad with spaces on the right rather than the left (left-justify the field)
    <space> -- leave a space for elided sign in numbers
    0 -- pad with leading zeros rather than spaces

## WIDTH
`WIDTH` is  an unsigned integer, which gives the minimum field width.

## PRECISION
`PRECISION` is  an unsigned integer, which modifies the field width.

    d, o, x -- gives the minimum number of digits.
    e, f, g -- gives the number of digits to output after the radix.
    v, s -- gives the maximum number of bytes to be printed from a string in the s conversion specifiers.

## CONV
`CONV` is the type of conversion.

    v -- converts any argument to readable format.
    b -- converts an unsigned integers to unsigned binary format.
    o -- converts an unsigned integers to unsigned octal format.
    d -- converts a number to integer format.
    x -- converts an unsigned integers to unsigned hexadecimal format.
    f -- converts float to decimal notation.
    c -- converts an unsigned integer to a character represented by the corresponding Unicode code point.
    s -- converts bytes-like object to string.
    % -- print a '%' character; no argument is converted.

# RETURN VALUE
Returns a string with the arguments `ARGS` formatted according to the `FORMATTER`.

# EXAMPLES

    ) (format "%g%%" 20.5)
    "20.5%"

    ) (format "%+d" 1)
    "+1"

    ) (format "% d" 1)
    " 1"

    ) (format "%-5d" 1)
    "1    "

    ) (format "%-5v" :foo)
    ":foo "
    ) (format "%-5s" "foo")
    "foo  "

    ) (format "%010d" 10)
    "0000000010"

    ) (format "%5d" 11)
    "   11"
    ) (format "%+5d" 11)
    "  +11"
    ) (format "%+-5d" 11)
    "+11  "
    ) (format "%+05d" 11)
    "+0011"

    ) (format "%10.5d" 1)
    "     00001"
    ) (format "%10.5x" 1)
    "     00001"
    ) (format "%+10.5x" 1)
    "    +00001"
    ) (format "%10.5f" 1)
    "   1.00000"
    ) (format "%10.5f" 10)
    "  10.00000"
    ) (format "%10.5e" 1)
    "1.00000e+00"
    ) (format "%10.5e" 10)
    "1.00000e+01"
    ) (format "%10.5g"  10)
    "        10"
    ) (format "%10.2s" "foo")
    "        fo"
    ) (format "%5.5s" "foo")
    "  foo"
    ) (format "%5.5s" "foobar")
    "fooba"

    ) (format "%c" 97)
    "a"

    ) (format "%d" -1)
    "-1"
    ) (format "%d" 3.1)
    "3"
    ) (format "%d" 2x1010)
    "10"

    ) (format "%f" 1.0)
    "1.000000"
    ) (format "%e" 1.0)
    "1.000000e+00"
    ) (format "%g" 1.0)
    "1"

    ) (format "%f" 10.1)
    "10.100000"
    ) (format "%e" 10.1)
    "1.010000e+01"
    ) (format "%g" 10.1)
    "10.1"

    ) (format "%g" (pow 10 6))
    "1e+06"
    ) (format "%g" (pow 10 -5))
    "1e-05"

# SEE ALSO
- `bin(3)`
- `hex(3)`
- `oct(3)`
- `str(3)`
