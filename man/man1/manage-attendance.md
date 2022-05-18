# NAME
manage-attendance - manage attendance and summarize.

# SYNOPSIS

    manage-attendance [OPTION] ... [FILE] ...

# DESCRIPTION
Read attendance information from standard input or `FILE` and output summary to standard output.

See `manage-attendance(5)` for attendance information format specifications.

# OPTIONS

    -a
        All subject to.
    -e
        Estimate the change in working hours if working at current levels.
    -E HOUR
        Estimate the change in working hours if working HOUR hours per day.
    -h HOUR
        Specify HOUR for the target monthly working hours instead of 140.
    -m MONTHS
        MONTHS are considered as the month to be counted.
        The default is this month.
    -s
        Suppress summary.
    -y YEARS
        YEARS are considered as the year to be counted.
        The default is this year.
    -v
        Output working hours verbosely.

# EXAMPLES
Reads attendance information from the file `attendance.p` and display a summary of time and attendance information.

    $ paren manage-attendance attendance.p

Reads attendance information from the file `attendance.p` and display only the hours worked per day.

    $ paren manage-attendance -vs attendance.p

Reads attendance information from the file `attendance.p` and displays a summary of 160 hours per month of target working hours.

    $ paren manage-attendance -l160 attendance.p

Read attendance information from the file `attendance.p` and display data from `2020` to `2022` and `2024`.

    $ paren manage-attendance -y2020-2022,2024 attendance.p

# SEE ALSO
- `cal(1)`
- `ldate(1)`
