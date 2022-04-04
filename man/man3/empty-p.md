# NAME
empty? - determine if it can be considered empty.

# SYNOPSIS

    (concat)
    (concat ARRAY ...)
    (concat BYTES ...)
    (concat LIST ...)
    (concat STRING ...)

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
- `len(3)`
