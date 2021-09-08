# NAME
$runtime-path - list of directories which will be searched for runtime.

# DESCRIPTION
The symbol `$runtime-path` holds a list of directories to search for scripts to run at runtime.

By default, the following directories are searched at runtime.

- $paren-home/tools
- $paren-home/tools/coreutils

You can add or remove runtime paths by changing the value of `$runtime-path` in file `.parenrc(5)`.

# SEE ALSO
- $paren-home(3)
- .parenrc(5)
