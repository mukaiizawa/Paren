# NAME
xargs - build and execute command lines from standard input.

# SYNOPSIS

    xargs [OPTION] COMMAND [PARTIAL-ARGUMENTS]...

# DESCRIPTION
Build and execute command lines from standard input.

# OPTIONS

    -c
        Execute a command with the entire standard input as an argument.
    -s
        Only display what kind of processing will be done and exit.

# BUGS
The `c` option may be truncated to the shell if the input is too long.

In addition, since the arguments are passed to the shell as is, appropriate escaping must be performed by the caller.

# EXAMPLES
Display only the first line of all files in the current directory.

    $ paren -f ls | paren xargs head -n1

Perform name resolution for valid addresses with the CIDR notation `192.168.30.0/24`.

    $ paren cidr -l 192.168.30.75/24 | paren xargs nslookup
