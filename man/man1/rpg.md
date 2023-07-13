# NAME
rpg - random password generator.

# SYNOPSIS

    rpg [OPTION]... [LENGTH]

# DESCRIPTION
Generate a random 8-character password consisting of alphanumeric characters and symbols.

# OPTIONS

    -a
        Only use lower-case letters.
    -A
        Only use upper-case letters.
    -l LENGTH
        Set the length of the output password to LENGTH characters.
    -n
        Only use numbers.
    -s
        Only use symbols.

# NOTES
Options do not undo others previously given.

Therefore, by specifying a plurality of options as in the `EXAMPLES`, only a specific item can be output.

Generated using a weak random number generator and should not be used in secure applications.

# EXAMPLES
Generate an 8-digit password consisting of alphanumeric characters.

    paren rpg -aAnl8

# SEE ALSO
- `shuf(1)`
