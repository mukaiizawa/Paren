# NAME
number?, cons?, symbol?, keyword?, string?, bytes?, array?, dict?, special-operator?, macro?, function? - predicate functions for primitive type determination.

# SYNOPSIS

    (number? X)
    (cons? X)
    (symbol? X)
    (keyword? X)
    (string? X)
    (bytes? X)
    (array? X)
    (dict? X)
    (special-operator? X)
    (macro? X)
    (function? X)

# DESCRIPTION
These functions determine if the argument is of the specified primitive type.

# RETURN VALUE
The function `number?` returns whether `X` is an number.

The function `cons?` returns whether `X` is a cons.

The function `symbol?` returns whether `X` is a symbol.

The function `keyword?` returns whether `X` is a keyword.

The function `string?` returns whether `X` is a string.

The function `bytes?` returns whether `X` is a bytes.

The function `array?` returns whether `X` is a array.

The function `dict?` returns whether `X` is a dictionary.

The function `special-operator?` returns whether `X` is a special-operator.

The function `macro?` returns whether `X` is a macro.

The function `function?` returns whether `X` is a function.

# NOTES
The function `bytes?` returns true only if `X` is exactly bytes, even if `X` is `bytes-like(7)`.

# EXAMPLES

    ) (number? 1)
    true
    ) (number? 3.14)
    true
    ) (number? 0x20)
    true
    ) (number? 'x)
    nil

    ) (cons? '(1))
    true
    ) (cons? nil)
    nil

    ) (symbol? 'foo)
    true
    ) (symbol? :foo)
    nil
    ) (symbol? (bytes 3))
    nil

    ) (keyword? :foo)
    true
    ) (keyword? 'foo)
    nil
    ) (keyword? (bytes 3))
    nil

    ) (string? "")
    true
    ) (string? "aaa")
    true
    ) (string? (bytes 1))
    nil

    ) (bytes? (bytes 3))
    true
    ) (bytes? 'foo)
    nil
    ) (bytes? :foo)
    nil
    ) (bytes? "foo")
    nil
    ) (bytes? (array 3))
    nil

    ) (array? (array 3))
    true
    ) (array? (bytes 3))
    nil

    ) (dict? (dict))
    true
    ) (dict? (array 1))
    nil

    ) (special-operator? <-)
    true
    ) (special-operator? special-operator?)
    nil

    ) (macro? begin0)
    true
    ) (macro? begin)
    nil

    ) (function? (f (x) x))
    true
    ) (function? begin0)
    nil

# SEE ALSO
- `atom?(3)`
- `byte?(3)`
- `int?(3)`
- `list?(3)`
- `nil?(3)`
