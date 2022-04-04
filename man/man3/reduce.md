# NAME
reduce - convolution of list by callback functions.

# SYNOPSIS

    (reduce FN LIST)

# DESCRIPTION
The function `reduce` apply function `FN` cumulatively to the elements of `LIST`, from left to right.

`FN` must be a function with two arguments.

# RETURN VALUE
Returns the value that apply function of two arguments cumulatively to the elements of the list args, from left to right.

# NOTES
If you want to set a specific value as the initial value, just cons it to `LIST`.

    (reduce fn (cons initial-value list))

# EXAMPLES

    ) (<- l (.. 10))
    (0 1 2 3 4 5 6 7 8 9)
    ) (reduce + l)
    45
    ) (reduce - (cons 100 l))
    55
    ) (reduce list l)
    (((((((((0 1) 2) 3) 4) 5) 6) 7) 8) 9)

# SEE ALSO
- `map(3)`
