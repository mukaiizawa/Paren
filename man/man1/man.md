# NAME
man - Paren reference manuals.

# SYNOPSIS

    man PAGE(SECTION)
    man [[SECTION] PAGE]

# DESCRIPTION
Display the specified `PAGE` manual.

If both `PAGE` and `SECTION` are omitted, display this manual.

If `SECTION` is omitted, the `PAGE` with the lowest section number is displayed.

If `SECTION` is provided, will direct man to look only in that `SECTION` of the manual.

If the corresponding `PAGE` does not exist, list the pages with a Levenshtein distance of 3 or less.

# NOTES
There is no `f` option, so use `whatis(1)` instead.

See `man-pages(7)` for concept of the section number.

# EXAMPLES
Display the manual page for the `man(1)`.

    paren man

Search for the first page that matches `man` in ascending order of section number (hence `man(1)`) and display it.

    paren man man

Display the manual page `man` for section number `1`.

    paren man 1 man
    paren man man(1)

# See Also
- whatis(1)
- man-pages(7)
