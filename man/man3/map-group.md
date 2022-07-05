# NAME
map-group - divide the elements of the list and map each of them.

# SYNOPSIS

    (map-group FN LIST N)

# DESCRIPTION
The function `map-gropu` is a composite function of the functions `map` and `group`.

# RETURN VALUE
Returns the list resulting from mapping the list into sublists.

# NOTES

    (map-group fn lis n)
    <=> (map (f (args) (apply fn args))
          (group lis n))

# EXAMPLES

    ) (map-group list (.. 10) 2)
    ((0 1) (2 3) (4 5) (6 7) (8 9))
    ) (map-group + (.. 10) 2)
    (1 5 9 13 17)

# SEE ALSO
- `group(3)`
- `group-by(3)`
- `map(3)`
