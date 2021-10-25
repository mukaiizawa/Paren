# NAME
map - map the list.

# SYNOPSIS

    (map FN LIST ...)

# DESCRIPTION
The function `map` returns a list of the results of applying the function `FN` to each element of `LIST`.

The following multiple lists `l1, l2, ... ` are given,

    l1 = (l11 l12 ...)
    l2 = (l21 l22 ...)
    ...

The following list is returned.

    (map fn l1 l2 ...)
    => (y1 y2 ...)
    y1 = (fn l11 l21 ...)
    y2 = (fn l12 l22 ...)
    ...

However, when the length of li is Li,

    lij ∊ li is ignored. (L1 < j ≤ Li)
    lij ∉ li is treated as nil. (Li < j ≤ L1)

Hence, the length of the result list is  becomes `L1`.

# RETURN VALUE
Returns list of results of the mapping of list `LIST` by function `FN`.

# NOTES
If you don't use a return value, you should use `foreach` or `dolist` instead.

# EXAMPLES

    ) (map (f (x) (* x x)) (.. 10))
    (0 1 4 9 16 25 36 49 64 81)

    ) (map (f (x y) (+ x y)) (.. 10) (reverse! (.. 10)))
    (9 9 9 9 9 9 9 9 9 9)

    ) (map (f (x y z) (list x y z)) (.. 3) (.. 2) (.. 1))
    ((0 0 0) (1 1 nil) (2 nil nil))

    ) (map (f (x y z) (list x y z)) (.. 1) (.. 2) (.. 3))
    ((0 0 0))

# SEE ALSO
- dolist(3)
- foreach(3)
