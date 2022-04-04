# NAME
import - import module file.

# SYNOPSIS

    (import MODULE [DIR])

# DESCRIPTION
The function `import` load a `MODULE` file from a `DIR` directory.

If `DIR` is omitted, read from `$paren-home/modules`.

If the `MODULE` has already been loaded, nothing is done, so calling the `MODULE` with the same name multiple times will work properly.

# RETURN VALUE
Returns the `MODULE`.

# NOTES
The symbol `main(3)` is bound to `nil` so that the `main` function is not executed when started in script mode.

# EXAMPLES

    ) (import :core)
    :core

# SEE ALSO
- `load(3)`
- `main(3)`
- `module(7)`
