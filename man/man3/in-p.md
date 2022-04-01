# NAME
in? - determine if a certain value is included.

# SYNOPSIS

    (in? X LIST)
    (in? X ARRAY)
    (in? X BYTES)
    (in? X STRING)
    (in? X DICTIONARY)

# DESCRIPTION
The function `in?` determine if a `X` is included in the second argument.

# RETURN VALUE
Returns whether element `X` exists or not.

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

    ) (in? :foo #{ :foo 0 :bar 1 })
    true
    
    ) (in? 1 #{ :foo 0 :bar 1 })
    nil
    
    ) (in? 2 #{ :foo 0 :bar 1 })
    nil

# SEE ALSO
- [](3)
- index(3)
