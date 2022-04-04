# NAME
push!, pop! - stack manipulation.

# SYNOPSIS

    (push! VAL VAR)
    (pop! VAR)

# DESCRIPTION
These macros are for treating lists like a stack.

The macro `push!` treats the list of referenced by the symbol `VAR` as a stack and pushes the value `VAL` onto the stack.

The macro `pop!` treats the list of referenced by the symbol `VAR` as a stack and pops and returns the top of the stack.

Both macros destructively modify the list referenced by the symbol `VAR`.

# RETURN VALUE
The macro `push!` returns `VAL`.

The macro `pop!` returns first element of the list referenced by the symbol `VAR`.

# EXAMPLES

    ) (<- stack nil)
    nil
    ) (push! 1 stack)
    1
    ) (push! 2 stack)
    2
    ) (push! 3 stack)
    3
    ) stack
    (3 2 1)
    ) (pop! stack)
    3
    ) (pop! stack)
    2
    ) (pop! stack)
    1
    ) stack
    nil

# SEE ALSO
- `list(3)`
