# NAME
prefix?, suffix? - determines whether a string starts or ends with the characters of a specified string.

# SYNOPSIS

    (prefix? STRING PREFIX)
    (suffix? STRING SUFFIX)

# DESCRIPTION
These functions determines whether a `STRING` starts or ends with the characters of a specified string.

# RETURN VALUE
The function `prefix?` returns whether `STRING` starts with the specified `PREFIX` or not.

The function `suffix?` returns whether `STRING` ends with the specified `SUFFIX` or not.

# EXAMPLES

    ) (prefix? "foo" "")
    true
    ) (prefix? "foo" "fo")
    true
    ) (prefix? "foo" "foo")
    true
    ) (prefix? "foo" "fooo")
    nil

    ) (suffix? "foo" "")
    true
    ) (suffix? "foo" "oo")
    true
    ) (suffix? "foo" "foo")
    true
    ) (suffix? "foo" "fooo")
    nil


# SEE ALSO
- `index(3)`
- `last-index(3)`
