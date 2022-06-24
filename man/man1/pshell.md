# NAME
pshell - paren shell.

# SYNOPSIS

    pshell

# DESCRIPTION
Invoke repl for simple execution of Paren script. The input string is interpreted as a command with prefix `paren` and executed.

# EXAMPLES

    $ paren pshell
    $ man cal
    cal(1)
    
    # NAME
    cal - display a calendar.
    
    # SYNOPSIS
    
        cal [[YEAR] MONTH]
    
    # DESCRIPTION
    Display the calendar for the specified `YEAR` and `MONTH`.
    
    If the `YEAR` and `MONTH` are omitted, it is considered that the current year a
    nd month are specified.
    
    # SEE ALSO
    - `date(1)`
    
    --
    man1/cal.md
    $ cal
    2022-06
    Su Mo Tu We Th Fr Sa
              1  2  3  4
     5  6  7  8  9 10 11
    12 13 14 15 16 17 18
    19 20 21 22 23 24 25
    26 27 28 29 30

# SEE ALSO
- `man(1)`
- `whatis(1)`
