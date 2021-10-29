# NAME
flatten - convert a tree to a list.

# SYNOPSIS

    (flatten TREE)

# DESCRIPTION
The function `flatten` convert a `TREE` to a flat list.

# RETURN VALUE
Returns a flat list.

# NOTES
Even if it is `nil`, if it can be regarded as a leaf, it becomes an element of the list.

# EXAMPLES

    ) (flatten nil)
    (nil)
    ) (flatten '(nil (nil) nil))
    (nil nil nil)
    ) (flatten '(nil ((nil) (nil)) nil))
    (nil nil nil nil)
    ) (flatten '((a b (c d)) (e) f ()))
    (a b c d e f nil)

# SEE ALSO
- list(3)
