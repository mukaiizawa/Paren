# NAME
zero?, pos?, neg?, even?, odd? - predicate functions for number.

# SYNOPSIS

    (zero? X)
    (pos? X)
    (neg? X)
    (even? X)
    (odd? X)

# DESCRIPTION
These functions are predicate functions for number.

# RETURN VALUE
The function `zero?` returns whether the `X` is `0` or not.

The function `neg?` returns whether the `X` is negative number or not.

The function `pos?` returns whether the `X` is positive number or not.

The function `even?` returns whether the `X` is even number or not.

The function `odd?` returns whether the `X` is odd number or not.

# EXAMPLES

    ) (zero? 0)
    true
    ) (zero? 1)
    nil

    ) (pos? -1)
    nil
    ) (pos? 1)
    true

    ) (neg? -1)
    true
    ) (neg? 1)
    nil

    ) (even? 0)
    true
    ) (even? 1)
    nil
    ) (even? 2)
    true

    ) (odd? 0)
    nil
    ) (odd? 1)
    true
    ) (odd? 2)
    nil

# SEE ALSO
- `<(3)`
- `<=(3)`
- `>(3)`
- `>=(3)`
