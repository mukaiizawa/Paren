coreutils

# Overview
This directory holds gnu coreutils implemented by Paren.

This is not a practical tool, it is intended to show you how to use Paren based on the well-known features of coreutils.  -- the handling of dates may be known from the cal command and so on.

For that purpose, most commands are implemented with minimal options.

# Usage
These programs is included in Paren's runtime path, so it can be executed anywhere the path to paren is in place.

    $ paren seq.p 0 10 | paren head.p -n 5
    0
    1
    2
    3
    4
