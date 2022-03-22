# NAME
uniq - omit repeated elements.

# SYNOPSIS

    (uniq LIST)

# DESCRIPTION
The function `uniq` filters adjacent identical elements.

Identical elements that are not adjacent to each other are not subject to deletion.

# RETURN VALUE
Returns a list excluding adjacent identical elements.

# EXAMPLES

    ) (uniq (.. 10))
    (0 1 2 3 4 5 6 7 8 9)
    ) (uniq '(2 0 0 0 1 1 2 3 4 0 0 1 2))
    (2 0 1 2 3 4 0 1 2)
    ) (uniq '(0 0 0 1 1 2 3 4 0 0 1 2))
    (0 1 2 3 4 0 1 2)
    ) (uniq (sort! '(0 0 0 1 1 2 3 4 0 0 1 2)))
    (0 1 2 3 4)

# SEE ALSO
- sort!(3)
