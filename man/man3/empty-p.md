# NAME
empty? - determine if there are any elements in the collection.

# SYNOPSIS

    (empty? COLLECTION)

# DESCRIPTION
The function `empty?` determine if there are any elements in the collection.

# RETURN VALUE
Returns whether the collection is zero-length.

# NOTES

    (empty? collection)
    <=> (= (len collection) 0)

# EXAMPLES

    ) (empty? nil)
    true
    
    ) (empty? (.. 3))
    nil

    ) (empty? #[ ])
    true
    
    ) (empty? #[ 0 1 2 ])
    nil

    ) (empty? "")
    true
    
    ) (empty? "foo")
    nil

    ) (empty? #{ })
    true
    
    ) (empty? #{ :foo 0 })
    nil

# SEE ALSO
- collection(7)
