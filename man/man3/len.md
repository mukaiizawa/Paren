# NAME
len - get the length of the collection.

# SYNOPSIS

    (len LIST)
    (len BYTES)
    (len STRING)
    (len ARRAY)
    (len DICTIONARY)

# DESCRIPTION
The function `len` get the length of the collection.

# RETURN VALUE
Returns the length of `COLLECTION`.

If the `COLLECTION` is `nil`, `0` is returned.

# EXAMPLES

    ) (len nil)
    0

    ) (len (.. 3))
    3

    ) (len #[ 0 1 2 ])
    3

    ) (len "foo")
    3

    ) (len #< 0x00 0x01 0x02 >)
    3

    ) (len #{ :foo 0 :bar 1 })
    2

# SEE ALSO
- `array(7)`
- `bytes(7)`
- `dictionary(7)`
- `list(7)`
- `string(7)`
