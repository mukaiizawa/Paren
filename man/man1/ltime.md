# NAME
ltime - list time.

# SYNOPSIS

    ltime [OPTION]

# DESCRIPTION
Enumerate the time every hour from `00:00` to `24:00`.

# OPTIONS

    -e HH:MM
        End to HH:MM.
    -d M
        Set the interval M minutes.
    -s HH:MM
        Start from HH:MM.

# EXAMPLES
Display every `15` minutes from `12:00` to `14:00`.

    $ paren ltime -s12:00 -e14:00 -d15
    12:00
    12:15
    12:30
    12:45
    13:00
    13:15
    13:30
    13:45
    14:00

# SEE ALSO
- `ldate(1)`
