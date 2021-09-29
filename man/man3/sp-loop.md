# NAME
loop, break, continue - iteration of evaluation.

# SYNOPSIS

    (loop [EXPR] ...)
    
    (break)
    
    (continue)

# DESCRIPTION
These special operators `loop`, `break` and `continue` supports the iterative evaluation.

The special operator `loop` is evaluate `EXPRs` in order from the left, and when the last `EXPR` is evaluated, evaluation is performed again in order from the beginning.

When `break` is evaluated, the nearest loop is terminated.

When `continue` is evaluated, terminates a current iteration and execute next iteration.

In contrast to the `break` expression, `continue` does not terminate the execution of the loop entirely.

# RETURN VALUE
The special operator `loop` returns the `nil`.

# EXAMPLES

    ) (<- i 0)
    0
    ) (loop (if (< (<- i (++ i)) 10) (continue) (break)))
    nil
    ) i
    10

# SEE ALSO
- while(3)
- for(3)
- special-operator(7)
