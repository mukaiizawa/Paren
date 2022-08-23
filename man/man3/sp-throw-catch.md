# NAME
catch, throw - throw exceptions and catch exceptions.

# SYNOPSIS

    (catch EXPR HANDLER)
    
    (throw VALUE)

# DESCRIPTION
The special operator `catch` evaluates `EXPR` first. If an exception is thrown when it is evaluated, the process is transferred to `HANDLER`. `HANDLER` is a function that takes a thrown `VALUE` as an argument.

The special operator `throw` throws a `VALUE`.  When a `VALUE` is thrown, it is caught by the exception handler of the most recent catch expression and processing is transferred.

# NOTES
As shown in the example, OOP error handling can be implemented as needed.

# EXAMPLES

    ) (catch (/ 4 2) (f (e) "division by zero"))
    2
    ) (catch (/ 4 0) (f (e) "division by zero"))
    "division by zero"
    ) (catch
        (throw (.new ArgumentError))
        (f (e)
          (if (is-a? e StateError) :StateError
              (is-a? e ArgumentError) :ArgumentError
              (throw e))))
    :ArgumentError

# SEE ALSO
- `raise(3)`
- `unwind-protect(3)`
- `special-operator(7)`
