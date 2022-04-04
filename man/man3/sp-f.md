# NAME
f, function, function! - create a function.

# SYNOPSIS

    (f (PARAMS)
        EXPR ...)
    
    (function NAME (PARAMS)
        EXPR ...)
    
    (function! NAME (PARAMS)
        EXPR ...)
    
    PARAMS = [REQUIRED-PARAM ...]
             [:opt OPTIONAL-PARAM ...]
             [:key KEYWORD-PARAM ... | :rest REST-PARAM]

# DESCRIPTION
The special operator `f` and macros `function`, and `function!` each create a function.

## Parameter
There are the following types of parameters.

- required parameter
- optional parameter
- keyword parameter
- rest parameter

When specifying multiple types of parameters, they must be specified in this order.

Also, keyword parameter and rest parameter cannot be specified at the same time.

### Required parameter
`REQUIRED-PARAM` is a required parameter that results in an error if not specified when calling the function.

### Optional parameter
`OPTIONAL-PARAM` is optional a parameter that need not be specified when calling the function.

If you specify `KEYWORD-PARAMs` or `REST-PARAM` with `OPTIONAL-PARAMs`, you cannot specify a value for `KEYWORD-PARAMs` or `REST-PARAM` unless you specify all `OPTIONAL-PARAMS` at the time of calling.

### Keyword parameter
`KEYWORD-PARAM` is a keyword parameter that specified with names without regard to order when calling the function.

### Rest parameter
`REST-PARAM` is a rest parameter implement variable length arguments.

# RETURN VALUE
The special operator `f` returns a anonymous functions.

The macros `function!` create and returns named function whether `NAME` is already bound.

The macros `function` binds the specified `NAME` to the function. At this time, the `EXPRs` are expanded. Returns the `NAME`.

# ERRORS
The macro `function` error if, the symbol is already bound.

# NOTES

    (function! NAME PARAMS EXPR ...)
    <=> (<- NAME (f PARAMS EXPR ...))

# EXAMPLES

    ) (function foo (x) x)
    foo
    ) (foo 3)
    3

    ) (function! foo (:opt x) x)
    (f (:opt x) x)
    ) (foo)
    nil
    ) (foo 1)
    1

    ) (function params (reqired-param :opt optional-param :key keyword-param)
        (list reqired-param optional-param keyword-param))
    params
    ) (params 1)
    (1 nil nil)
    ) (params 1 2 :keyword-param 3)
    (1 2 3)

# SEE ALSO
- `special-operator(7)`
