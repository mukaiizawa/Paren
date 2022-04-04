# NAME
union, intersection, difference, symmetric-difference, product - set operations.

# SYNOPSIS

    (union X Y)
    (intersection X Y)
    (difference X Y)
    (symmetric-difference X Y)
    (product X Y)

# DESCRIPTION
These functions are the basic set operators.

The term `set` here refers to a list without duplicates.

# RETURN VALUE
The function `union` returns the union of sets `X` and `Y`.

The function `intersection` returns the intersection of sets `X` and `Y`.

The function `difference` returns the difference of sets `X` and `Y`.

The function `symmetric-difference` returns the symmetric difference of sets `X` and `Y`.

The function `product` returns the Cartesian product of sets `X` and `Y`.

# NOTES
The order of the elements in the list of return values is undefined.

Also, the behavior of duplicate elements in each set is undefined.

# EXAMPLES

    ) (<- X (.. 3) Y (map ++ X))
    (1 2 3)
    ) (union X Y)
    (3 0 1 2)
    ) (intersection X Y)
    (1 2)
    ) (symmetric-difference X Y)
    (3 0)
    ) (difference X Y)
    (0)
    ) (product X Y)
    ((0 1) (0 2) (0 3) (1 1) (1 2) (1 3) (2 1) (2 2) (2 3))

# SEE ALSO
- `in?(3)`
