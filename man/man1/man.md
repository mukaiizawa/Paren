# NAME
man - paren reference manuals.

# SYNOPSIS

    man [[SECTION] PAGE]

# DESCRIPTION
Display the specified PAGE manual.

A SECTION, if provided, will direct man to look only in that SECTION of the manual.

See man-pages(7) for sections.

If the section number is omitted, the search is performed in a smaller order.

# EXAMPLES
Display the manual page for the man(1).

    paren man
    paren man man
    paren man 1 man

Display the manual page for the man-pages(7).

    paren man man-pages
    paren man 7 man-pages

# See Also
- man-pages(7)
