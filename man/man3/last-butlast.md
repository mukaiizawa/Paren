# NAME
last, butlast - the last element or a list excluding the last.

# SYNOPSIS

    (last LIST)
    (butlast LIST)

# DESCRIPTION
These functions perform processing related to the end of the list.

# RETURN VALUE
The function `last` returns the last element in `LIST`.

The function `butlast` returns a list of all but the last element in `LIST`.

These functions return `nil` if `LIST` is an empty list.

# NOTES

    (last lis) <=> (take-last lis 1)
    (butlast lis) <=> (drop-last lis 1)

# EXAMPLES

    ) (last nil)
    nil
    ) (last (.. 3))
    2

    ) (butlast nil)
    nil
    ) (butlast (.. 3))
    (0 1)

# SEE ALSO
- take(3)
- drop(3)
- drop-last(3)
- last-cons(3)
- take-last(3)
