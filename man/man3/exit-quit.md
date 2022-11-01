# NAME
exit, quit - cause process termination.

# SYNOPSIS

    (exit STATUS)
    (quit)

# DESCRIPTION
These functions terminate process and the least significant byte of `STATUS` is returned to the parent.

All cleanup functions registered with `load(3)` are called.

# RETURN VALUE
Undefined.

# NOTES

    (quit) <=> (exit 0)

# SEE ALSO
- `load(3)`
