# NAME
load - file loading.

# SYNOPSIS

    (load FILE)

# DESCRIPTION
The function `load` loads the `FILE`.

If the read succeeds, the following side effects occur.

1. If the `startup` symbol is bound, it is assumed to be a function with no arguments and called.
1. If the `cleanup` symbol is bound, it is assumed to be a no argument function and called at the end of the program.

The macro `with-open(3)` is called to open the file, so the value you can specify for `FILE` conforms to it.

# RETURN VALUE
Returns the value to which the symbol `main` is bound in the file read.

If the symbol `main` is not bound in the read file, returns the `nil`.

# NOTES
Unlike `import(3)`, it loads as many times as it is called for the same `FILE`.

# EXAMPLES

    ) (load "./foo.p")
    nil

    ) (load (.resolve $paren-home "modules/re.p"))
    nil

# SEE ALSO
- `import(3)`
- `with-open(3)`
