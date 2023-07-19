# NAME
raise - raising exceptions.

# SYNOPSIS

    (raise EXCEPTION-CLASS FORMAT [ITEM]...)

# DESCRIPTION
The function `raise` initialize and throw an instance of the exception class with a formatted message.

# EXAMPLES

    ) (raise ArithmeticError "division by %d" 0)
    ArithmeticError -- division by 0
      at: (raise #{ :fields nil :features nil :super Error :symbol ArithmeticError :class Class } "division by %d" 0)
      at: (repl)
      at: (boot nil)

# SEE ALSO
- `catch(3)`
- `format(3)`
- `throw(3)`
