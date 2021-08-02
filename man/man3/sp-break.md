# NAME
break - terminate a loop context.

# SYNOPSIS

    (break)

# DESCRIPTION
`break` terminate a innermost loop context.

# RETURN VALUE
Returns the `nil`.

# EXAMPLES

    ) (<- i 0)
    0
    ) (loop (if (>= (<- i (++ i)) 10) (break)))
    nil
    ) i
    10

# SEE ALSO
- continue(3)
- loop(3)
- while(3)
- for(3)
- special-operator(7)
