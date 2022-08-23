# NAME
mangen - manual template generator.

# SYNOPSIS

    mangen PAGE(SECTION)
    mangen SECTION PAGE

# DESCRIPTION
Generate a template for the specified manual.

# EXAMPLES
Create a manual named `mangen` in section 1.

    $ paren mangen mangen(1)

It is equivalent to write the following.

    $ paren mangen 1 mangen

# ERRORS
If the manual to be created already exists, an error is generated.

# SEE ALSO
- `man(1)`
- `man-pages(7)`
