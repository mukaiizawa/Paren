# NAME
wc - print newline, word, and byte counts of standard input.

# SYNOPSIS

    wc [OPTION]...

# DESCRIPTION
Print newline, word, and byte counts.

# OPTIONS

    -b
        Only print the byte counts.
    -c
        Only print the character counts.
    -l
        Only print the line counts.
    -w
        Only print the word counts.

# NOTES
Options do not undo others previously given.

Therefore, by specifying a plurality of options as in the `EXAMPLES`, only a specific item can be output.

Regardless of the number or order of options, the output always follows the following order:

1. line count
1. word count
1. character count
1. byte count

# EXAMPLES
Print the number of lines, words, and characters.

    paren wc

Print only the number of lines

    paren wc -l

Print only the number of words and characters

    paren wc -wc

# SEE ALSO
- `coreutils(7)`
