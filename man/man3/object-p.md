# NAME
object? - determine if the argument is an object.

# SYNOPSIS

    (object X)

# DESCRIPTION
The function `object?` determines if the argument `X` is an object in the Paren object system.

# RETURN VALUE
Returns whether x is an object in the Paren object system.

# NOTES
The parenobject system is a simplified object system built using dictionary data type. The following equation illustrates this point.

    (object? x) <=> (&& (dict? x) ([] x :class))

# EXAMPLES

    ) (object? nil)
    nil
    ) (object? 3)
    nil
    ) (object? "str")
    nil
    ) (object? 'symbol)
    nil
    ) (object? (.new Object))
    Object
    ) (object? (.new Error))
    Error

# SEE ALSO
- `number?(3)`
- `cons?(3)`
- `symbol?(3)`
- `keyword?(3)`
- `string?(3)`
- `bytes?(3)`
- `array?(3)`
- `dict?(3)`
- `special-operator?(3)`
- `macro?(3)`
- `function?(3)`
- `object-system(7)`
