# NAME
last-cons - returns last cons.

# SYNOPSIS

    (last-cons LIST)

# DESCRIPTION
The function `last-cons` returns the last cons (not last element).

# RETURN VALUE
Returns the last cons of the `LIST`.

If `LIST` is `nil`, returns `nil`.

# ERRORS
## ArgumentError
Error if `LIST` is not a list.

# EXAMPLES

    ) (last-cons nil)
    nil
    ) (last-cons (.. 3))
    (2)

# SEE ALSO
- `last(3)`
- `slice(3)`
