# NAME
index - locate a element.

# SYNOPSIS

    (index COLLECTION VAL)

# DESCRIPTION
The function `index` returns the key of the `COLLECTION` corresponding to `VAL`.

# RETURN VALUE
Returns the key of the `COLLECTION` corresponding to `VAL`.

If `VAL` is not associated, returns `nil`.

# EXAMPLES

    ) (<- d #{ :foo 1 :bar 2 })
    #{ :bar 2 :foo 1 }
    ) (index d 1)
    :foo
    ) (index d 2)
    :bar
    ) (index d 3)
    nil

    ) (<- l (.. 3))
    (0 1 2)
    ) (index l 1)
    1
    ) (index l 2)
    2
    ) (index l 3)
    nil

    ) (<- a (array (.. 3)))
    #[ 0 1 2 ]
    ) (index a 1)
    1
    ) (index a 2)
    2
    ) (index a 3)
    nil

    ) (<- s "abc") ; string.
    "abc"
    ) (index s "a")
    0
    ) (index s "b")
    1
    ) (index s "d")
    nil

# SEE ALSO
- position(3)
- collection(7)
