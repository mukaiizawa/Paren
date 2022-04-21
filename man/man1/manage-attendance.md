# NAME
manage-attendance - manage attendance and summarize.

# SYNOPSIS

    manage-attendance [OPTION] ...

# DESCRIPTION
Read attendance information from standard input and output summary to standard output.

The default assumption is to work 140 hours per month.

See `manage-attendance(5)` for attendance information format specifications.

# OPTIONS

    -h HOUR
        Specify HOUR for the target monthly working hours.
    -v
        Output working hours verbosely.

# SEE ALSO
- `cal(1)`
- `ldate(1)`
