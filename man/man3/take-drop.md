# NAME
take, drop - first/all but the first elements of list.

# SYNOPSIS

    (take LIST N)
    (drop LIST N)

# DESCRIPTION
These functions return/discard elements from the beginning.

# RETURN VALUE
The function `take` returns the first `N` elements of `LIST`. If there are not enough elements, return the `LIST`.

The function `drop` returns all but the first `N` elements of `LIST`. If there are not enough elements, `nil` is returned.

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

# SEE ALSO
- `[](3)`
- `butlast(3)`
- `drop-last(3)`
- `drop-while(3)`
- `last(3)`
- `slice(3)`
- `split-at(3)`
- `take-last(3)`
- `take-while(3)`
