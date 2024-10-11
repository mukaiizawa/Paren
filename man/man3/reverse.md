# NAME
reverse, reverse! - reverses a list in place.

# SYNOPSIS

    (reverse LIST)
    (reverse! LIST)

# DESCRIPTION
These functions reverses a list.

The function `reverse` always creates and returns new conses.

The function `reverse!` same as `reverse` except that it destructively modifies the argument `LIST`.

# RETURN VALUE
Returns a list with the elements of `LIST` reversed.

If `LIST` is `nil`, returns `nil`.

# ERRORS
## ArgumentError
If `LIST` is not a list.

# NOTES
The function `reverse!` generally faster than `reverse`.

The reverse function accepts only list. If you want to reverse a string, you need to make it a list once as shown below.

    (join (revere! (split s)))

# EXAMPLES

    ) (<- l1 (.. 3) l2 (slice l1))
    (0 1 2)
    ) (reverse nil)
    nil
    ) (reverse l1)
    (2 1 0)
    ) l1
    (0 1 2)
    ) (reverse! nil)
    nil
    ) (reverse! l2)
    (2 1 0)
    ) l2
    (0)

# SEE ALSO
- `slice(3)`
