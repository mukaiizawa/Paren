# NAME
int? - predicate function for integer.

# SYNOPSIS

    (int? X)

# DESCRIPTION
The function `int?` is a predicate that determines if the argument can be considered an integer.

# RETURN VALUE
Returns whether `X` is a integer.

# EXAMPLES

    ) (int? nil)
    nil
    ) (int? 0)
    true
    ) (int? 0xfffff)
    true
    ) (int? 3.14159)
    nil

# SEE ALSO
- `byte?(3)`
- `number?(3)`
