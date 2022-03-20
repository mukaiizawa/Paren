# NAME
member - search list.

# SYNOPSIS

    (member FN LIST)

# DESCRIPTION
The function `member` search `LIST` for item that satisfies the `FN`.

# RETURN VALUE
If some element satisfies the `FN`, the tail of list beginning with this element is returned.

Otherwise `nil` is returned.

# EXAMPLES

    ) (member (f (x) (> x 3)) (.. 10))
    (4 5 6 7 8 9)
    ) (member (partial = 5) (.. 10))
    (5 6 7 8 9)

# SEE ALSO
- assoc(3)
- drop-while(3)
- take-while(3)
