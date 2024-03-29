# NAME
split - split a string.

# SYNOPSIS

    (split STRING [SEPARATOR])

# DESCRIPTION
The function `split` creates a list of words that are split using delimiters.

# RETURN VALUE
Returns a list of the words in the `STRING`, using `SEPARATOR` as the delimiter string.

If `STRING` is empty string or `nil`, returns `nil`.

If `SEPARATOR` is omitted, returns a list of characters.

# NOTES
If you want to split using a regular expression, refer to the `re(3)` module.

# EXAMPLES

    ) (split nil)
    nil
    
    ) (split "")
    nil

    ) (split "foo")
    ("f" "o" "o")
    
    ) (split "foo" ":")
    ("foo")
    
    ) (split "foo::bar::buzz" "::")
    ("foo" "bar" "buzz")
    
    ) (split "foo::::bar::buzz" "::")
    ("foo" "" "bar" "buzz")

    ) (split ",foo" ",")
    ("" "foo")
    
    ) (split "foo," ",")
    ("foo" "")
    
    ) (split ",foo," ",")
    ("" "foo" "")

# SEE ALSO
- `join(3)`
- `re(3)`
- `split-at(3)`
- `split-with(3)`
