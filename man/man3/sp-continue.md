# NAME
continue - terminates a current iteration and execute next iteration.

# SYNOPSIS

    (continue)

# DESCRIPTION
In contrast to the break statement, continue does not terminate the execution of the loop entirely.

# RETURN VALUE
Returns the `nil`.

# EXAMPLES

    ) (<- i 0)
    0
    ) (loop (if (< (<- i (++ i)) 10) (continue) (break)))
    nil
    ) i
    10

# SEE ALSO
- break(3)
- loop(3)
- while(3)
- for(3)
- special-operator(7)
