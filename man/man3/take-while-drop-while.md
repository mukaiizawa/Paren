# NAME
take-while, drop-while - first/all but the first elements of list.

# SYNOPSIS

    (take-while FN LIST)
    (drop-while FN LIST)

# DESCRIPTION
These functions return/discard elements from the beginning.

# RETURN VALUE
The function `take-while` returns the longest initial prefix of `LIST` whose elements all satisfy the predicate `FN`.

The function `drop-while` drops the longest initial prefix of `LIST` whose elements all satisfy the predicate `FN`, and returns the rest of the list.

# EXAMPLES

    ) (take-while (partial < 3) (.. 10))
    (0 1 2)

    ) (drop-while (partial < 3) (.. 10))
    (3 4 5 6 7 8 9)

# SEE ALSO
- drop(3)
- slice(3)
- split-with(3)
- take(3)
