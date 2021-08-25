# NAME
man - Paren reference manuals.

# SYNOPSIS

    man PAGE(SECTION)
    man [[SECTION] PAGE]

# DESCRIPTION
Display the specified PAGE manual.

A SECTION, if provided, will direct man to look only in that SECTION of the manual.

See man-pages(7) for sections.

If the section number is omitted, the search is performed in a smaller order.

# NOTES
There is no `f` option, so use whatis(1) instead.

# EXAMPLES
Display the manual page for the man(1).

    paren man
    paren man man
    paren man 1 man
    paren man man(1)

Display the manual page for the man-pages(7).

    paren man man-pages
    paren man 7 man-pages
    paren man man-pages(7)

# See Also
- whatis(1)
- man-pages(7)
