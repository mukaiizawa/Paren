# NAME
mandep - manual dependency.

# SYNOPSIS

    mandep [OPTION]

# DESCRIPTION
Extract the references to other manuals listed in the `SEE ALSO` section of each manual that do not exist.

Therefore, the output is a list of manuals that must be created.

# OPTIONS

    -a
        Extract all references, regardless of whether they exist or not.

# NOTES
Manuals referenced outside the `SEE ALSO` section are not subject to extraction.

# See Also
- man(1)
- mandb(1)
