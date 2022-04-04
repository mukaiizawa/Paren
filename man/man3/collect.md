# NAME
collect - list generation by function call.

# SYNOPSIS

    (collect FN)

# DESCRIPTION
The function `collect` takes one function` FN` and returns a list of returned values were called until that function `FN` returned `nil`.

`FN` is a function with no arguments and generally has some side effects. In other words, it is a function that eventually causes a state transition that returns `nil`.

The elements of the list are in the order in which `FN` is called.

`nil` is not included in the list elements.

# RETURN VALUE
Returns a list of returned values were called until that function `FN` returned `nil`.

# EXAMPLES

    ) (<- l (.. 3))
    (0 1 2)
    ) (collect (f () (pop! l)))
    (0 1 2)
    ) l
    nil

# SEE ALSO
- `list(3)`
- `map(3)`
