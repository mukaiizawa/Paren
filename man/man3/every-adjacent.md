# NAME
every-adjacent? - test each adjacent elements of list for satisfaction of a given predicate.

# SYNOPSIS

    (every-adjacent? FN LIST)

# DESCRIPTION
The function `each-adjacent?` test each adjacent elements of `LIST` for satisfaction of a given predicate `FN`.

# RETURN VALUE
Returns whether the function `FN` returns `non-nil` for all two adjacent elements of a `LIST`.

If `LIST` is `nil`, returns `nil`.

# NOTES

    (each-adjacent? fn '(x1 x2 ...))
    <=> (&& (fn x1 x2) (fn x2 x3) ...)

# EXAMPLES

    ) (every-adjacent? < nil)
    true
    ) (every-adjacent? < (.. 10))
    true
    ) (every-adjacent? > (.. 10))
    nil

# SEE ALSO
- every?(3)
