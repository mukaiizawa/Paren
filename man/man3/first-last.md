# NAME
first, last - getting the first and last elements.

# SYNOPSIS

    (first SEQUENCE)
    (last SEQUENCE)

# DESCRIPTION
These functions retrieve the first and last elements of a sequence.

# RETURN VALUE
The function `first` returns the first element of `SEQUENCE`.

The function `last` returns the last element of `SEQUENCE`.

These functions return `nil` if `SEQUENCE` is `nil`.

# NOTES

    (first seq)
    <=> ([] seq 0)

    (last seq)
    <=> ([] seq (-- (len seq)))

# EXAMPLES

    ) (first nil)
    nil
    
    ) (last nil)
    nil

    ) (first (.. 3))
    0
    
    ) (last (.. 3))
    2

    ) (first (array (.. 3)))
    0
    ) (last (array (.. 3)))
    2

    ) (first (bytes "foo"))
    102
    ) (last (bytes "foo"))
    111

    ) (first "foo")
    "f"
    ) (last "foo")
    "o"

# SEE ALSO
- [](3)
- butlast(3)
- last-cons(3)
