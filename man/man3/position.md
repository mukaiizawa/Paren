# NAME
position - position of an element.

# SYNOPSIS

    (position FN LIST)

# DESCRIPTION
The function `position` returns the position of an element for which function `FN` is applied and the result is `non-nil`.

`FN` is a function with an argument that takes element of a list `LIST`.

Positions are counted from zero.

# RETURN VALUE
Returns the position of an element for which function `FN` is applied and the result is `non-nil`.

If there is no such element, returns `nil`.

# EXAMPLES

    ) (<- l (.. 10))
    (0 1 2 3 4 5 6 7 8 9)
    ) (position int? l)
    0
    ) (position (f (x) (> x 0)) l)
    1
    ) (position nil? l)
    nil

# SEE ALSO
- index(3)
- find(3)
- higher-order-function(7)
