# NAME
whatis - display one-line manual page descriptions.

# SYNOPSIS

    whatis [NAME] [OPTION]

# DESCRIPTION
Each manual page has a short description available within it.

`whatis` searches the manual page names and displays the manual page descriptions of any `NAME` matched.

# OPTIONS

    -s SECTIONS
        Search only the given manual sections.
        SECTIONS is a comma-separated list of sections.

# EXAMPLES
Search for all manuals.

    $ paren whatis

Search the manual containing the string `ca`.

    $ paren whatis ca

Search for manuals containing the string `ca` from section 1 and 3.

    $ paren whatis -s1,3 ca

# SEE ALSO
- man(1)
- man-pages(7)
