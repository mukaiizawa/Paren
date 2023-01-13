# NAME
trim, ltrim, rtrim - make a substring with the leading and trailing characters removed.

# SYNOPSIS

    (trim STRING [FN])
    (ltrim STRING [FN])
    (rtrim STRING [FN])

# DESCRIPTION
The function `trim` removes the leading and trailing characters while the function `FN` returns `non-nil` value.

The function `ltrim` is the same as function `trim`, except that only remove the leading characters.

The function `rtrim` is the same as function `trim`, except that only remove the trailing characters.

# RETURN VALUE
Returns the string with characters removed from which the function `FN` returned `non-nil` value.

If function `FN` is ommited, `space?` is assumed to be specified.

# EXAMPLES

    ) (trim "   foo   ")
    "foo"
    
    ) (ltrim "   foo   ")
    "foo   "
    
    ) (rtrim "   foo   ")
    "   foo"

    ) (trim "012,foo,abc" alnum?)
    ",foo,"
    
    ) (ltrim "012,foo,abc" alnum?)
    ",foo,abc"
    
    ) (rtrim "012,foo,abc" alnum?)
    "012,foo,"

# NOTES

    (trim s fn)
    <=> (ltrim (rtrim s fn) fn)
    <=> (rtrim (ltrim s fn) fn)

# SEE ALSO
- `slice(3)`
- `space?(3)`
- `string(7)`
