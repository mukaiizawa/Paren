# NAME
if - conditional statements.

# SYNOPSIS

    (if TEST THEN
        [TEST THEN] ...
        [ELSE])

# DESCRIPTION
The special operator `if` evaluates `TEST` in order from the top and executes `THEN` of the clause that returned true.

If none of the `TESTs` return true, evaluate the `ELSE` clause.

# RETURN VALUE
Returns a evaluation result of `THEN` or `ELSE` evaluated.

If `ELSE` is undefined and any `TEST` is `nil`, returns `nil`.

# EXAMPLES

    ) (if true 1)
    1
    ) (if nil 1)
    nil
    ) (if true 1 2)
    1
    ) (if nil 1 2)
    2
    ) (if nil 1
          nil 2
          3)
    3

# SEE ALSO
- `when(3)`
- `special-operator(7)`
