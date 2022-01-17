# NAME
atom->list - make a list.

# SYNOPSIS

    (atom->list X)

# DESCRIPTION
The function `atom->list` returns a value so that the argument is a list.

# RETURN VALUE
Returns a list with `X` as the only element.

If `X` is a list, returns `X`.

# NOTES

    (atom->list x) <=> (if (list? x) x (list x))

# EXAMPLES

    ) (atom->list nil)
    nil
    ) (atom->list :foo)
    (:foo)
    ) (atom->list (.. 3))
    (0 1 2)

# SEE ALSO
- list(3)
