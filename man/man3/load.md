# NAME
load - file loading.

# SYNOPSIS

    (load FILE)

# DESCRIPTION
The function `load` loads the `FILE`.

The macro `with-open(3)` is called to open the file, so the value you can specify for `FILE` conforms to it.

Unlike `import(3)`, it loads as many times as it is called for the same `FILE`.

If `FILE` is a keyword data type, as in the function `import`, it will be read with the extension `p`.

# RETURN VALUE
Returns the `true`.

# EXAMPLES

    ) (load (.resolve $paren-home "modules/re.p"))
    true

# SEE ALSO
- import(3)
- with-open(3)
