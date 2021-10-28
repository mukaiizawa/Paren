# NAME
find - find the element.

# SYNOPSIS

    (find FN LIST)

# DESCRIPTION
The function `find` find the element in the list `LIST` where the function `FN` first returns not `nil`.

# RETURN VALUE
Returns the return value of the function `FN` that did not return `nil` first.

If there is no such element, returns `nil`.

# NOTES

    (find fn list) <=> (car (reject nil? (map fn list)))

# EXAMPLES

    ) (<- l (.. 10))
    (0 1 2 3 4 5 6 7 8 9)

    ) (find (f (x) (if (> x 5) (* x x))) l)
    36

# SEE ALSO
- map(3)
- select(3)
- reject(3)
- higher-order-function(7)
