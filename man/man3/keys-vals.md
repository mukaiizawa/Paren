# NAME
keys, vals - list the keys/values contained in a dictionary.

# SYNOPSIS

    (keys DICTIONARY)
    (vals DICTIONARY)

# DESCRIPTION
These functions return a list of dictionary keys and values.

# RETURN VALUE
The function `keys` returns a list of keys contained in this `DICTIONARY`.

The function `vals` returns a list of values contained in this `DICTIONARY`.

# NOTES
The order of the return value is undefined.

# EXAMPLES

    ) (keys #{ :foo 0 :bar 1 :buzz 2 })
    (:buzz :bar :foo)

    ) (vals #{ :foo 0 :bar 1 :buzz 2 })
    (2 1 0)

# SEE ALSO
- dict(3)
- dictionary(7)
