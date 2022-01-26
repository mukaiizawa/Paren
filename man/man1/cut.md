# NAME
cut - remove sections from each line of files.

# SYNOPSIS

    cut -b LIST
    cut -c LIST
    cut -f LIST [-d DELIM]

# DESCRIPTION
Print selected parts of lines from standard input to standard output.

# OPTIONS

    -b LIST
        Select only these bytes.
    -c LIST
        Select only these characters.
    -d DELIM
        Use DELIM instead of TAB for field delimiter.
    -f LIST
        Select only these fields.

Use one, and only one of `-b`, `-c` or `-f`.

Each `LIST` is made up of one range, or many ranges separated by commas. Selected input is written in the same order that it is read, and is written exactly once. Each range is one of:

    N
        N'th byte, character or field, counted from 1.
    N-
        from N'th byte, character or field, to end of line.
    N-M
        from N'th to M'th byte, character or field.
    -M
        from first to M'th byte, character or field.

# EXAMPLES
Display the third and subsequent characters of each line of the text file `foo.txt`.

    $ paren cut -c 3- < foo.txt

Display the fourth, third, first, and second columns of the CSV file `foo.csv`.

    $ paren cut -d, -f 4,3,-2 < foo.csv

# SEE ALSO
- coreutils(7)
