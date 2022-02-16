# NAME
<, >, <=, >= - compare objects.

# SYNOPSIS

    (< X1 X2 ...)
    (> X1 X2 ...)
    (<= X1 X2 ...)
    (>= X1 X2 ...)

# DESCRIPTION
These functions compare objects.

# RETURN VALUE
The function `<` returns whether the each of the specified args are in monotonically decreasing order.

The function `>` returns whether the each of the specified args are in monotonically increasing order.

The function `<=` returns whether the each of the specified args are in monotonically nondecreasing order.

The function `>=` returns whether the each of the specified args are in monotonically nonincreasing order.

# NOTES
See `memcmp(3)` for a comparison specification of bytes like data type objects.

# EXAMPLES

    ) (< 0 1 2)
    true
    
    ) (< 2 1 0)
    nil
    
    ) (< 0 0 1 2 2)
    nil
    
    ) (< 2 2 1 0 0)
    nil

    ) (> 0 1 2)
    nil
    
    ) (> 2 1 0)
    true
    
    ) (> 0 0 1 2 2)
    nil
    
    ) (> 2 2 1 0 0)
    nil

    ) (<= 0 1 2)
    true
    
    ) (<= 2 1 0)
    nil
    
    ) (<= 0 0 1 2 2)
    true
    
    ) (<= 2 2 1 0 0)
    nil

    ) (>= 0 1 2)
    nil
    
    ) (>= 2 1 0)
    true
    
    ) (>= 0 0 1 2 2)
    nil
    
    ) (>= 2 2 1 0 0)
    true

    ) (< "a" "b" "c")
    true
    
    ) (< 'a 'b 'c)
    true
    
    ) (< :a :b :c)
    true

# SEE ALSO
- memcmp(3)
- comparable(7)
