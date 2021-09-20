# NAME
dolist, dostring, doarray - iterates over the elements of a collection.

# SYNOPSIS

    (dolist (VAR VAL)
        EXPR ...)
    
    (dostring (VAR VAL)
        EXPR ...)
    
    (doarray (VAR VAL)
        EXPR ...)

# DESCRIPTION
These macros create a context for iterating through the entire collection.

First evaluate the `VAL`, which should produce a collection.

After that, each element of the collection is bound to `VAR` and `EXPR`s is evaluated in order.

# RETURN VALUE
Returns the `nil`.

# NOTES
Supports break(3) and  continue(3).

# EXAMPLES

    ) (<- sum 0)
    0
    ) (dolist (i (.. 10)) (<- sum (+ sum i)))
    nil
    ) sum
    45
    ) (<- l nil)
    nil
    ) (dostring (ch "foo") (push! ch l))
    nil
    ) (reverse! l)
    ("f" "o" "o")
    )
    (<- sum 0)
    0
    ) (doarray (i (array (.. 10))) (<- sum (+ sum i)))
    nil
    ) sum
    45
    )

# SEE ALSO
- break(3)
- continue(3)
- dotimes(3)
