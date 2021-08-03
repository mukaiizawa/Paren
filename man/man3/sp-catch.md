# NAME
catch - exception handling.

# SYNOPSIS

    (catch (ERROR HANDLER [ERROR HANDLER] ...)
        EXPR ...)

# DESCRIPTION
The `catch` special operator evaluates EXPR in order from the left.

If an exception occurs while evaluating EXPR, control is first transferred to a HANDLER that matches the exception type regarded as ERROR.

HANDLER is a function that takes an exception as an argument.

# RETURN VALUE
Returns evaluation result of the last EXPR.

If control is transferred HANDLER, returns handler execution result.

# EXAMPLES

    ) (catch (ArgumentError (f (e) 1) StateError (f (e) 2))
        3)
    3
    ) (catch (ArgumentError (f (e) 1) StateError (f (e) 2))
        (raise StateError))
    2

# SEE ALSO
- raise(3)
- throw(3)
- special-operator(7)
