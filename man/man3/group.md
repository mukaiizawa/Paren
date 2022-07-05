# NAME
group - grouping list into sublists.

# SYNOPSIS

    (group LIST N)

# DESCRIPTION
The function `group` grouping list into sublists.

# RETURN VALUE
Returns a list in which the elements `LIST` are grouped into sublists of length `N`.

# ERRORS
Error if `N` is not positive integer.

# NOTES
The length of the last sublists is:

    (if (= (// (len LIST) N) 0) N
        (% (len LIST) N))

# EXAMPLES

    ) (group nil 2)
    nil
    ) (group (.. 10) 1)
    ((0) (1) (2) (3) (4) (5) (6) (7) (8) (9))
    ) (group (.. 10) 2)
    ((0 1) (2 3) (4 5) (6 7) (8 9))
    ) (group (.. 10) 3)
    ((0 1 2) (3 4 5) (6 7 8) (9))
    ) (group (.. 10) 4)
    ((0 1 2 3) (4 5 6 7) (8 9))

# SEE ALSO
- `group-by(3)`
- `map-group(3)`
- `slice(3)`
