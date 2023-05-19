# NAME
load - file loading.

# SYNOPSIS

    (load FILE)

# DESCRIPTION
The function `load` loads the `FILE`.

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
