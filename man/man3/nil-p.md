# NAME
nil? - determines whether argument is nil.

# SYNOPSIS

    (nil? X)

# DESCRIPTION
The function `nil?` returns whether argument is `nil`.

# RETURN VALUE
Returns whether `X` is `nil` or not.

# NOTES
Since `nil` is the only value that is treated as false, the following equation holds.

    (nil? x) <=> (! x)

# EXAMPLES

    ) (nil? nil)
    true
    ) (nil? 1)
    nil
    ) (nil? true)
    nil

# SEE ALSO
- `!(3)`
