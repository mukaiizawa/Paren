# NAME
list? - determines whether argument is a list.

# SYNOPSIS

    (list? X)

# DESCRIPTION
The function `list?` returns whether argument is a list.

# RETURN VALUE
Returns whether `X` is a list.

# NOTES

    (list? x) <=> (|| (nil? x) (cons? x))

# EXAMPLES

    ) (list? nil)
    true
    ) (list? 1)
    nil
    ) (list? (cons 1 nil))
    true

# SEE ALSO
- atom?(3)
- cons?(3)
