# NAME
strip, lstrip, rstrip - make a substring with the leading and trailing characters removed.

# SYNOPSIS

    (strip STRING [FN])
    (lstrip STRING [FN])
    (rstrip STRING [FN])

# DESCRIPTION
The function `strip` removes the leading and trailing characters while the function `FN` returns `non-nil` value.

The function `lstrip` is the same as function `strip`, except that only remove the leading characters.

The function `rstrip` is the same as function `strip`, except that only remove the trailing characters.

# RETURN VALUE
Returns the string with characters removed from which the function `FN` returned `non-nil` value.

If function `FN` is ommited, `space?` is assumed to be specified.

# EXAMPLES

    ) (strip "   foo   ")
    "foo"
    
    ) (lstrip "   foo   ")
    "foo   "
    
    ) (rstrip "   foo   ")
    "   foo"

    ) (strip "012,foo,abc" alnum?)
    ",foo,"
    
    ) (lstrip "012,foo,abc" alnum?)
    ",foo,abc"
    
    ) (rstrip "012,foo,abc" alnum?)
    "012,foo,"

# NOTES

    (strip s fn)
    <=> (lstrip (rstrip s fn) fn)
    <=> (rstrip (lstrip s fn) fn)

# SEE ALSO
- `slice(3)`
- `space?(3)`
- `string(7)`
