# NAME
macro - macro data type.

# DESCRIPTION

    The definition of a macro is essentially a function that generates Lisp code - a program that writes programs.
    -- 

A macro data type is a sub-program that can be called by code external (or internal in the case of recursion) to the macro.

Like the program itself, a macro is composed of a sequence of expression called the macro body.

Arguments can be passed to a macro, and the macro will return a value.

In Paren, macro are first-class objects, so it can also be given to the argument of the macro.

# SEE ALSO
- `macro(3)`
- `data-types(7)`
- `function(7)`
- `special-operator(7)`
