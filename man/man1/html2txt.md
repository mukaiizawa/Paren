# NAME
html2txt - convert from HTML to text.

# SYNOPSIS

    html2txt

# DESCRIPTION
Reads standard input as HTML and outputs nodes to standard output.

The output targets all but the script element in the body element.

# EXAMPLES

    $ curl https://foo.bar.buzz | paren html2txt

# SEE ALSO
- `html2txt(1)`
- `md2html(1)`
