# NAME
span - search list.

# SYNOPSIS

    (span FN LIST)

# DESCRIPTION
The function `span` splits the element into the longest first part and the remaining part satisfying the predicate `FN`.

# RETURN VALUE
Returns a list of "before the first element that does not satisfy pred" and "after the first element that does not satisfy pred".

# NOTES

  (span fn lis) <=> (apply (juxt take-while drop-while) (list fn lis))

# EXAMPLES

    ) (span (partial > 3) (.. 10))
    ((0 1 2) (3 4 5 6 7 8 9))
    ) (span (partial != 5) (.. 10))
    ((0 1 2 3 4) (5 6 7 8 9))

# SEE ALSO
- juxt(3)
- drop-while(3)
- take-while(3)
