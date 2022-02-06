# NAME
butlast - copy the sequence without the last part.

# SYNOPSIS

    (butlast SEQUENCE N)

# DESCRIPTION
The function `butlast` make a copy of sequence without the last part.

# RETURN VALUE
Returns a copy of the `SEQUENCE` from which the last `N` elements have been omitted.

# NOTES

    (butlast seq n)
    <=> (slice seq 0 n)

# EXAMPLES

    ) (butlast nil)
    nil
    
    ) (butlast nil 1)
    nil

    ) (butlast (.. 3))
    (0 1)
    
    ) (butlast (.. 3) 0)
    (0 1 2)
    
    ) (butlast (.. 3) 1)
    (0 1)
    
    ) (butlast (.. 3) 2)
    (0)

    ) (butlast (array (.. 3)))
    #[ 0 1 ]
    
    ) (butlast (array (.. 3)) 1)
    #[ 0 1 ]
    
    ) (butlast (array (.. 3)) 2)
    #[ 0 ]

    ) (butlast "foo")
    "fo"
    
    ) (butlast "foo" 1)
    "fo"
    
    ) (butlast "foo" 2)
    "f"

# SEE ALSO
- [](3)
- first(3)
- last(3)
- sequence(7)
