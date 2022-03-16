# NAME
prefix?, suffix? - determines whether a string starts or ends with the characters of a specified string.

# SYNOPSIS

    (prefix? BYTES PREFIX)
    (suffix? BYTES SUFFIX)

# DESCRIPTION
These functions determines whether a `BYTES` starts or ends with the characters of a specified string.

# RETURN VALUE
The function `prefix?` returns whether `BYTES` starts with the specified `PREFIX` or not.

The function `suffix?` returns whether `BYTES` ends with the specified `SUFFIX` or not.

# NOTES
In the current implementation, these functions return the position where the partial bytes matches if the condition is satisfied. This behavior is due to the internal use of `memmem(3)`. They should not use except for boolean value as this may change in the future without notice.

# EXAMPLES

    ) (prefix? "foo" "")
    nil
    ) (suffix? "foo" "")
    nil

    ) (prefix? "foo" "fo")
    0
    ) (suffix? "foo" "oo")
    1

    ) (prefix? "foo" "foo")
    0
    ) (suffix? "foo" "foo")
    0

    ) (prefix? "foo" "fooo")
    nil
    ) (suffix? "foo" "fooo")
    nil

# SEE ALSO
- memmem(3)
