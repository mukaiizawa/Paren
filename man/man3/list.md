# NAME
list - create a list.

# SYNOPSIS

    (list [OBJECT] ...)

# DESCRIPTION
The function `list` creates a list.

# RETURN VALUE
Returns a list containing the supplied `OBJECT`s.

# EXAMPLES

    ) (list)
    nil
    ) (list :a :b :c)
    (:a :b :c)

# SEE ALSO
- car(3)
- cdr(3)
- list(3)
- cons(7)
