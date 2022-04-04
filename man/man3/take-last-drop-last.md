# NAME
take-last, drop-last - the last elements or all but the last elements.

# SYNOPSIS

    (take-last LIST N)
    (drop-last LIST N)

# DESCRIPTION
These functions return/discard the last elements or all but the last elements.

# RETURN VALUE
The function `take-last` returns the last `N` elements of `LIST`. If there are not enough elements, return the `LIST`.

The function `drop-last` returns all but the last `N` elements of `LIST`. If there are not enough elements, `nil` is returned.

# NOTES

    (take-last lis n) <=> (slice lis 0 n)
    (drop-last lis n) <=> (slice lis n)

If the list is long enough for `i`.

    (concat (take-last lis i) (drop-last lis i)) <=> lis

# EXAMPLES

    ) (take-last (.. 10) 3)
    (7 8 9)
    ) (take-last (.. 10) 100)
    (0 1 2 3 4 5 6 7 8 9)

    ) (drop-last (.. 10) 3)
    (0 1 2 3 4 5 6)
    ) (drop-last (.. 10) 100)
    nil

# SEE ALSO
- `[](3)`
- `butlast(3)`
- `drop(3)`
- `drop-while(3)`
- `last(3)`
- `slice(3)`
- `split-at(3)`
- `take(3)`
- `take-while(3)`
