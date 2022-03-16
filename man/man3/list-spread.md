# NAME
list... - expand the elements of a sequence into a list.

# SYNOPSIS

    (list... SEQUENCE)

# DESCRIPTION
The function `list...` expand the elements of a sequence into a list.

# RETURN VALUE
Return a list such that the elements of the sequence `SEQUENCE` are in the elements.

# EXAMPLES

    ) (list... nil)
    nil
    
    ) (list... "foo")
    ("f" "o" "o")
    
    ) (list... (.. 3))
    (0 1 2)
    
    ) (list... (array 3))
    (nil nil nil)
    
    ) (list... (bytes 3))
    (0 0 0)

# SEE ALSO
- list(3)
