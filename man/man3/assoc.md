# NAME
assoc - get the value from the association list.

# SYNOPSIS

    (assoc KEY ALIST)

# DESCRIPTION
The function `assoc` regards the list as an associative list and returns the value corresponding to the key.

An associative list is a list in which keys and values are arranged alternately.

    ((key1 val11 ...) (key2 val21 ...) ... )

# RETURN VALUE
Returns the value corresponding to the `KEY` in the association list `ALIST`.

If there are multiple applicable keys, the value close to the beginning is returned.

If there is no corresponding value, returns `nil`.

# EXAMPLES

    ) (assoc :foo '((:foo 0) (:bar 1)))
    (:foo 0)
    ) (assoc :bar '((:foo 0) (:bar 1)))
    (:bar 1)
    ) (assoc :buzz '((:foo 0) (:bar 1)))
    nil

# SEE ALSO
- `member(3)`
- `list(7)`
