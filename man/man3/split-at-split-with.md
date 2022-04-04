# NAME
split-at, split-with - splitting a list.

# SYNOPSIS

    (split-at LIST N)
    (split-with FN LIST)

# DESCRIPTION
These functions split the list into two parts by position/predicate.

# RETURN VALUE
The function `split-at` splits the `LIST` into the first `N` elements and the others.

The function `split-with` splits the `LIST` in the part where the predicate `FN` continues to be satisfied and the others.

# NOTES

  (split-at lis i) <=> (apply (juxt take drop) (list lis i))
  (split-with fn lis) <=> (apply (juxt take-while drop-while) (list fn lis))

# EXAMPLES

    ) (split-at (.. 3) 0)
    (nil (0 1 2))
    ) (split-at (.. 3) 1)
    ((0) (1 2))
    ) (split-at (.. 3) 3)
    ((0 1 2) nil)

    ) (split-with (partial > 3) (.. 10))
    ((0 1 2) (3 4 5 6 7 8 9))
    ) (split-with (partial != 5) (.. 10))
    ((0 1 2 3 4) (5 6 7 8 9))

# SEE ALSO
- `chunk(3)`
- `drop(3)`
- `drop-while(3)`
- `group-by(3)`
- `juxt(3)`
- `split(3)`
- `take(3)`
- `take-while(3)`
