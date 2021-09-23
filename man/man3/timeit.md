# NAME
timeit - measure evaluation time.

# SYNOPSIS

    (timeit EXPR ...)

# DESCRIPTION
The macro `timeit` creates a context for measuring the evaluation time of `EXPRs` and the number of virtual-machine cycles.

# RETURN VALUE
Returns evaluation result of the last `EXPRs`.

# EXAMPLES

    ) (timeit (.. 10))
    (:time 0 :cycle 631)
    (0 1 2 3 4 5 6 7 8 9)
    ) (timeit 1 2 3)
    (:time 0 :cycle 166)
    3

# SEE ALSO
- clock(3)
- cycle(3)
