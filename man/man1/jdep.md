# NAME
jdep - java dependency.

# SYNOPSIS

    jdep [OPTION] ...

# DESCRIPTION
Analyze import statements and output dependencies for all java files under the current directory.

If the `o` option is specified, graphviz is used to output the dependency graph to a png file.

# OPTIONS

    -c CLASSES
        Except CLASSES.
    -o FILE
        Output to FILE using graphviz
    -p PACKAGES
        Except PACKAGES.

# SEE ALSO
- `cdep(1)`
