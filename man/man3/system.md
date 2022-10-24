# NAME
system - execute a shell command.

# SYNOPSIS

    system COMMAND

# DESCRIPTION
Execute host system commands `COMMAND`.

# RETURN VALUE
Returns the termination status of the child shell used to execute command.

# EXAMPLES

    ) (system "cal")
        October 2022
    Su Mo Tu We Th Fr Sa
                       1
     2  3  4  5  6  7  8
     9 10 11 12 13 14 15
    16 17 18 19 20 21 22
    23 24 25 26 27 28 29
    30 31
    0

# SEE ALSO
- `pclose(3)`
- `popen(3)`
