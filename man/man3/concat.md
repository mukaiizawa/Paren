# NAME
concat - concatenate the objects.

# SYNOPSIS

    (concat)
    (concat ARRAY ...)
    (concat BYTES ...)
    (concat KEYWORD ...)
    (concat LIST ...)
    (concat STRING ...)
    (concat SYMBOL ...)

# DESCRIPTION
The function `concat` concatenate an array, bytes, list, or string.

# RETURN VALUE
Returns the object concatenated in the order in which the arguments are given.

If there is no argument, `nil` is returned.

# NOTES
The return value will be created anew and none of the arguments will be changed.

# ERRORS
Error if the argument data types are not all the same. However, the symbol `nil` also behaves as an empty list.

# EXAMPLES

    ) (concat)
    nil

    ) (concat (array 1) (array 2))
    #[ nil nil nil ]

    ) (concat :foo :bar)
    :foobar

    ) (concat nil)
    nil
    ) (concat nil nil)
    nil
    ) (concat '(1 2) '(3))
    (1 2 3)
    ) (concat '(1) '(2))
    (1 2)
    ) (concat nil '(1) '(2))
    (1 2)

    ) (concat "0" "1" "2")
    "012"

    ) (concat 'foo 'bar)
    foobar

# SEE ALSO
- `join(3)`
- `split(3)`
