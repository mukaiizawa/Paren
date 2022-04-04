# NAME
[] - general purpose accessor.

# SYNOPSIS

    ([] LIST INDEX)
    ([] LIST INDEX VAL)
    ([] ARRAY INDEX)
    ([] ARRAY INDEX VAL)
    ([] BYTES INDEX)
    ([] BYTES INDEX VAL)
    ([] STRING INDEX)
    ([] DICTIONARY KEY)
    ([] DICTIONARY KEY VAL)

# DESCRIPTION
The function `[]` functions as a built-in per-type accessor.

# RETURN VALUE
If `VAL` is not specified, the value corresponding to `KEY` or `INDEX` is returned.

If `VAL` is specified, update the value corresponding to `KEY` or `INDEX`. Returns `VAL`.

# ERRORS
IndexError if `INDEX` is out of range. For a list, return `nil`.

ArgumentError if `X` is a string data type and `VAL` is specified.

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
- `array(7)`
- `bytes(7)`
- `dictionary(7)`
- `list(7)`
- `string(7)`
