# NAME
keys - list the keys contained in a dictionary.

# SYNOPSIS

    (keys DICTIONARY)

# DESCRIPTION
The function `keys` returns a list of keys in the dictionary.

# RETURN VALUE
Returns a list of keys contained in this dictionary.

# NOTES
The order of the return value is undefined.

# EXAMPLES

    ) (keys #{ :foo 0 :bar 1 :buzz 2 })
    (:buzz :bar :foo)

# SEE ALSO
- dict(3)
- dictionary(7)
