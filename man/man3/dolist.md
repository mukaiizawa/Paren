# NAME
dolist - iterates over the elements of a list.

# SYNOPSIS

    (dolist (VAR LIST)
        EXPR ...)

# DESCRIPTION
The macro `dolist` create a context for iterating through the entire list.

First evaluate the `LIST`, which should produce a list.

After that, each element of the list is bound to `VAR` and `EXPR`s is evaluated in order.

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

# SEE ALSO
- break(3)
- continue(3)
- dotimes(3)
