# NAME
juxt - juxtaposition of functions.

# SYNOPSIS

    (juxt FN ...)

# DESCRIPTION
The function `juxt` create a function that is the juxtaposition of functions `FNs`.

# RETURN VALUE
Returns list of results of the mapping of list `LIST` by function `FN`.

# EXAMPLES

    ) (begin
        (<- xlen (juxt len (compose len (partial select pos?)) (compose len (partial select zero?))))
        (xlen '(-1 0 0 1 2 3)))
    (6 3 2)

    ) (begin
        (<- divide (juxt take drop))
        (divide (.. 10) 3))
    ((0 1 2) (3 4 5 6 7 8 9))

    ) (map (juxt identity (f (x) (* x x)) (f (x) (* x x x))) (.. 3))
    ((0 0 0) (1 1 1) (2 4 8))

# SEE ALSO
- `map(3)`
