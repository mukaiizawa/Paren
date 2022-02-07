# NAME
[] - get and set elements of the collection.

# SYNOPSIS

    ([] COLLECTION KEY)
    ([] COLLECTION KEY VAL)

# DESCRIPTION
The function `[]` gets or sets an element of a collection.

# RETURN VALUE
Returns the value corresponding to `KEY` of the `COLLECTION`. If `COLLECTION` is a list and the index key is out of range, returns `nil`.

If `VAL` is specified, update the value corresponding to `KEY`. Returns `VAL`.

# ERRORS
IndexError if `COLLECTION` is a bytes or array or string data type and the index `KEY` is out of range.

ArgumentError if `COLLECTION` is a string data type and `VAL` is specified.

# EXAMPLES

    ) ([] nil 5)
    nil

    ) (<- l (.. 3))
    (0 1 2)
    
    ) ([] l 5)
    nil
    
    ) ([] l 1)
    1
    
    ) ([] l 1 10)
    10
    
    ) l
    (0 10 2)

    ) (<- a (array (.. 3)))
    #[ 0 1 2 ]
    
    ) ([] a 1)
    1
    
    ) ([] a 1 10)
    10
    
    ) a
    #[ 0 10 2 ]

    ) (<- b (bytes 3))
    #[ 0x00 0x00 0x00 ]
    
    ) ([] b 0)
    0
    
    ) ([] b 1 0xff)
    255
    
    ) b
    #[ 0x00 0xff 0x00 ]

    ) ([] "foo" 1)
    "o"

    ) (<- d #{ :foo 0 :bar 1 })
    #{ :bar 1 :foo 0 }
    
    ) ([] d :buzz)
    nil
    
    ) ([] d :foo)
    0
    
    ) ([] d :bar 10)
    10
    
    ) ([] d :buzz 2)
    2
    
    ) d
    #{ :buzz 2 :bar 10 :foo 0 }

# SEE ALSO
- collection(7)
