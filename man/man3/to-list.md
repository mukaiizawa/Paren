# NAME
->list - make a list.

# SYNOPSIS

    (->list X)

# DESCRIPTION
The function `->list` returns a value so that the argument is a list.

# RETURN VALUE
Returns a list with `X` as the only element.

If `X` is a list, returns `X`.

# NOTES

    (->list x) <=> (if (list? x) x (list x))

# EXAMPLES

    ) (->list nil)
    nil
    ) (->list :foo)
    (:foo)
    ) (->list (.. 3))
    (0 1 2)

# SEE ALSO
- `list(3)`
