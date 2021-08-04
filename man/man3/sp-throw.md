# NAME
throw - throws a exception.

# SYNOPSIS

    (throw EXCEPTION)

# DESCRIPTION
The `throw` operator throws a exception.

Execution of the current function will stop, and control will be passed to the first handler in the first matching catch(3) expression.

If no error handler is matched, the program will terminate.

# RETURN VALUE
None.

# NOTES
Generally, the raise(3) function is used, so it is not used directly.

# EXAMPLES

    ) (catch (ArgumentError (f (e) 1) StateError (f (e) 2))
        3)
    3
    ) (catch (ArgumentError (f (e) 1) StateError (f (e) 2))
        (throw (.new StateError)))
    2

# SEE ALSO
- catch(3)
- raise(3)
- special-operator(7)
