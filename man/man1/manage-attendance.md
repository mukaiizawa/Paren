# NAME
manage-attendance - manage attendance and summarize.

# SYNOPSIS

    manage-attendance [OPTION] ... [FILE] ...

# DESCRIPTION
Read attendance information from standard input or `FILE` and output summary to standard output.

See `manage-attendance(5)` for attendance information format specifications.

# OPTIONS

    -h HOUR
        Specify HOUR for the target monthly working hours instead of 140.
    -s
        Suppress summary.
    -v
        Output working hours verbosely.

# EXAMPLES
Reads attendance information from file `attendance.p` and display a summary of time and attendance information.

    $ paren manage-attendance attendance.p

Reads attendance information from file `attendance.p` and display only the hours worked per day.

    $ paren manage-attendance -vs attendance.p

Reads attendance information from file `attendance.p` and displays a summary of 160 hours per month of target working hours.

    $ paren manage-attendance -l160 attendance.p

# SEE ALSO
- `cal(1)`
- `ldate(1)`
