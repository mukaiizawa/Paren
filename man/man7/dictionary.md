# NAME
dictionary - dictionary data type.

# DESCRIPTION
A dictionary is a data type composed of a collection of `key-value` pairs, such that each possible key appears at most once in the collection.

The dictionary data type can use the functions listed in `SEE ALSO` section, and can also use the functions supported by `collection`.

Any object can be specified for the key and value, but if a mutable object is specified for the key, the search time will be `O(n)`. This is because the hash values of mutable objects are internally treated as fixed constants.

Unlike general dictionaries, pairs cannot be deleted. This limitation is because the main purpose of dictionary is to build the Paren Object System.

# EXAMPLES

    ) (<- d (dict))
    #{ }
    ) ([] d :foo 0)
    0
    ) ([] d :bar 1)
    1
    ) ([] d :foo)
    0
    ) (keys d)
    (:bar :foo)

# SEE ALSO
- `dict(3)`
- `dict?(3)`
- `in?(3)`
- `keys(3)`
- `len(3)`
- `vals(3)`
- `with(3)`
- `data-types(7)`
