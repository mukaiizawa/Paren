# NAME
=, ==, !=, !== - indicates whether the object is equal.

# SYNOPSIS

    (= X Y)
    (== X Y)
    (!= X Y)
    (!== X Y)

# DESCRIPTION
These functions indicate if the objects are `same` or `equal`.

`same` means that they are allocated in the same memory.

`equal` means that the objects have the same type and the same value.

Therefore, `same` is `equal`, but `equal` is not necessarily `same`.

## equivalence
### number
If two numbers are mathematically the same, they are considered to be the same.

### bytes & string
If the two bytes (string) are the same in length and content, they are considered the same.

### cons
If the two conses car and cdr are the same, they are considered to be the same.

### arrray
If two arrays have the same size and the corresponding elements, they are considered the same.

### keyword & symbol
If two symbols (keywords) are the same, they are considered equal.

This is because there is only one symbol (keyword) with the same name in the system.

### dictionary
If the two dictionaries have the same number of entries and the respective keys and corresponding values are the same, then the two dictionaries are considered to be the same.

# RETURN VALUE
The function `=` returns whether `X` and `Y` are equal.

The function `==` returns whether `X` and `Y` are same.

The function `!=` and `!==` return the result of inverting the boolean values of `=` and `==`, respectively.

# NOTES

    (!= X Y) <=> (! (= X Y))
    (!== X Y) <=> (! (== X Y))

# EXAMPLES

    ) (= 1 1)
    true
    ) (== 1 1)

    true
    ) (= 1.0 1)
    true
    ) (== 1.0 1)
    nil

    ) (= '(1 2 3) '(1 2 3))
    true
    ) (== '(1 2 3) '(1 2 3))
    nil

    ) (= 'foo 'foo)
    true
    ) (== 'foo 'foo)
    true

# SEE ALSO
- !(3)
