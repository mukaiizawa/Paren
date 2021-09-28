# NAME
throw, catch - throw exceptions and catch exceptions.

# SYNOPSIS

    (throw EXCEPTION)
    
    (catch (EXCEPTION-CLASS HANDLER [EXCEPTION-CLASS HANDLER] ...)
        EXPR ...)

# DESCRIPTION
The special-operator throw throws a `EXCEPTION`.

When `EXCEPTION` is thrown, execution of the current function stops and control is passed to the first `HANDLER` in the first matching catch expression.

The special-operator `catch` evaluates `EXPRs` in order from the left.

If an `EXCEPTION` is thrown by throw while evaluating `EXPRs`, control is first transferred to a `HANDLER` that matches the `EXCEPTION` type regarded as `EXCEPTION-CLASS`.

`HANDLER` is a function that takes a thrown `EXCEPTION` as an argument.

If no `HANDLER` is matched, the program will terminate.

# NOTES
In general, `throw` is rarely used directly, and `raise(3)` is used instead.

# EXAMPLES

    ) (catch (ArgumentError (f (e) 1) StateError (f (e) 2))
        3)
    3
    ) (catch (ArgumentError (f (e) 1) StateError (f (e) 2))
        (throw (.new StateError)))
    2

# SEE ALSO
- raise(3)
- special-operator(7)
