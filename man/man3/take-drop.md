# NAME
take, drop, take-while, drop-while - first/all but the first elements of list.

# SYNOPSIS

    (take LIST N)
    (drop LIST N)
    (take-while FN LIST)
    (drop-while FN LIST)

# DESCRIPTION
These functions return/discard elements from the beginning.

# RETURN VALUE
The function `take` returns the first `N` elements of `LIST`. If there are not enough elements, return `N` elements from the beginning.

The function `drop` returns all but the first `N` elements of `LIST`. If there are not enough elements, `nil` is returned.

The function `take-while` returns the longest initial prefix of `LIST` whose elements all satisfy the predicate `FN`.

The function `drop-while` drops the longest initial prefix of `LIST` whose elements all satisfy the predicate `FN`, and returns the rest of the list.

# NOTES

    (take lis n) <=> (slice lis 0 n)
    (drop lis n) <=> (slice lis n)

If the list is long enough for `i`.

    (concat (take lis i) (drop lis i)) <=> lis

# EXAMPLES

    ) (take (.. 10) 3)
    (0 1 2)
    ) (take (.. 10) 100)
    (0 1 2 3 4 5 6 7 8 9)

    ) (drop (.. 10) 3)
    (3 4 5 6 7 8 9)
    ) (drop (.. 10) 100)
    nil

    ) (take-while (partial < 3) (.. 10))
    (0 1 2)

    ) (drop-while (partial < 3) (.. 10))
    (3 4 5 6 7 8 9)

# SEE ALSO
- butlast(3)
- last(3)
- partial(3)
- slice(3)
