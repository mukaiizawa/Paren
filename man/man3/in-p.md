# NAME
in? - determine if the collection contains an element.

# SYNOPSIS

    (in? X COLLECTION)

# DESCRIPTION
The function `in?` determine if the collection contains an element.

# RETURN VALUE
Returns whether element `X` exists in the `COLLECTION`.

# EXAMPLES

    ) (in? 1 nil)
    nil
    
    ) (in? 1 (.. 3))
    true
    
    ) (in? 2 (.. 3))
    true

    ) (in? 1 #[ ])
    nil
    
    ) (in? 1 #[ 1 2 3 ])
    true
    
    ) (in? 1 #[ 1 2 3 ])
    true

    ) (in? "o" "foo")
    true
    
    ) (in? "oo" "foo")
    true
    
    ) (in? "bar" "foo")
    nil

    ) (<- b (bytes 3))
    #[ 0x00 0x00 0x00 ]
    
    ) (in? 0x00 b)
    true
    
    ) (in? 0x01 b)
    nil

    ) (<- d #{ :foo 0 :bar 1 })
    #{ :bar 1 :foo 0 }
    
    ) (in? :foo d)
    true
    
    ) (in? 1 d)
    nil
    
    ) (in? 2 d)
    nil

# SEE ALSO
- collection(7)
