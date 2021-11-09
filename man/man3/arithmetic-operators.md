# NAME
+, -, *, /, //, % - arithmetic operators.

# SYNOPSIS

    (+ [NUMBER ...])
    (- MINUEND [SUBTRAHEND ...])
    (* [NUMBER ...])
    (/ NUMERATOR [DENOMINATOR ...])
    (// NUMERATOR DENOMINATOR)
    (% NUMERATOR DENOMINATOR)

# DESCRIPTION
These functions are basic arithmetic operators.

# RETURN VALUE
The function `+` returns the sum of `NUMBERs`. If `NUMBER` is omitted, `0` is returned.

The function `-` returns the value of `MINUEND` minus the sum of `SUBTRAHENDs`. If `SUBTRAHEND` is omitted, the negation of that `MINUEND` is returned.

The function `*` returns the product of `NUMBERs`. If `NUMBER` is omitted, `1` is returned.

The function `/` returns the quotient of `NUMERATOR` divided by all `DENOMINATOR`. If `DENOMINATOR` is omitted, the reciprocal of `NUMERATOR` is returned.

The function `//` is the same as function `/`, except that the return value is rounded to an integer type.

The function `%` returns remainder of dividing `NUMERATOR` by `DENOMINATOR`.

# EXAMPLES

    ) (+)
    0
    ) (+ 1)
    1
    ) (apply + (.. 10))
    45

    ) (- 1)
    -1
    ) (- 1 2 3)
    -4

    ) (*)
    1
    ) (* 1 2 3)
    6
    ) (* 3.14 3 3)
    28.26

    ) (/ 2 1)
    2
    ) (/ 12 2 3)
    2
    ) (/ 1 2)
    0.5
    ) (/ 2)
    0.5

    ) (// 2 1)
    2
    ) (// 5 2)
    2

    ) (% 2 1)
    0
    ) (% 5 2)
    1

# SEE ALSO
- ++(3)
- --(3)
