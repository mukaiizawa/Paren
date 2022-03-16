# NAME
assoc - get the value from the association list.

# SYNOPSIS

    (assoc ALIST KEY)

# DESCRIPTION
The function `assoc` regards the list as an associative list and returns the value corresponding to the key.

An associative list is a list in which keys and values are arranged alternately.

    (key1 val1 key2 val2 ... )

# RETURN VALUE
Returns the value corresponding to the `KEY` in the association list `ALIST`.

If there are multiple applicable keys, the value close to the beginning is returned.

If there is no corresponding value, returns `nil`.

# EXAMPLES

    ) (<- alist '(:foo 0 :bar 1 :foo 2))
    (:foo 0 :bar 1 :foo 2)
    ) (assoc alist :foo)
    0
    ) (assoc alist :bar)
    1
    ) (assoc alist :buzz)
    nil

# SEE ALSO
- list(7)
