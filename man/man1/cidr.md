# NAME
cidr - CIDR calclator.

# SYNOPSIS

    cidr CIDR

# DESCRIPTION
Calculate network information in CIDR notation.

# OPTIONS

    -l
        Enumerate IP addresses in the range.

# EXAMPLES
Calculate network information in CIDR notation `192.168.30.0/24`.

    $ paren cidr 192.168.30.0/24

Resolve the name of a valid address with the specified cidr notation.

    $ paren cidr -l 192.168.30.0/24 | paren xargs nslookup
