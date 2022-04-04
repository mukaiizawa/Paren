# NAME
which - shows the full path of file.

# SYNOPSIS

    which [OPTION] NAME ...

# DESCRIPTION
Search for specified file `NAME` paths based on the user's `PATH`.

The search does not take into account whether the file is executable or not.

# OPTIONS

    -a
        Print all matching.
    -s
        Strict match.
        If not only the base-name but also the extension is the same, it is considered to be a match.

# EXAMPLES
Search for files whose base-name is `paren`.

    $ paren which paren

Search all files whose basename is `paren`.

    $ paren which -a paren

Search for files whose name exactly matches `paren`.

    $ paren which -s paren

# BUGS
If run without sufficient permissions, unreadable directories will be excluded from the search.

# SEE ALSO
- `getenv(3)`
