# NAME
f - anonymous expression.

# SYNOPSIS

    (f ([REQUIRED-PARAM ...]
        [:opt OPTIONAL-PARAM ...]
        [:key KEYWORD-PARAM ... | :rest REST-PARAM])
        EXPR ...)

# DESCRIPTION
Create a new anonymous function.

When the function is called, the specified arguments are bound to the parameter and EXPR(s) is evaluated in order.

## Parameter
There are the following types of parameters.

- required parameter
- optional parameter
- keyword parameter
- rest parameter

When specifying multiple types of parameters, they must be specified in this order.

Also, keyword parameter and rest parameter cannot be specified at the same time.

### Required parameter
REQUIRED-PARAM is a required parameter that results in an error if not specified when calling the function.

### Optional parameter
OPTIONAL-PARAM is optional a parameter that need not be specified when calling the function.

### Keyword parameter
KEYWORD-PARAM is a keyword parameter that specified with names without regard to order when calling the function.

### Rest parameter
REST-parameters is a rest parameter implement variable length arguments.

# RETURN VALUE
Returns an anonymous function.

# EXAMPLES

    ) (apply (f (x) x) '(1))
    1

see function(3).

# SEE ALSO
- function(3)
- macro(3)
- special-operator(7)
