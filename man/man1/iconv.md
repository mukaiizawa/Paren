# NAME
iconv - internationalization conversion.

# SYNOPSIS

    iconv [OPTIONS]... FROM TO

# DESCRIPTION
Convert standard input to another encoding.

# OPTIONS

    -l
        List the supported encodings and exit.

# ERRORS
If the conversion fails, an error occurs.

# EXAMPLES
Convert standard input to utf8 by assuming it is sjis.

    $ paren iconv sjis utf8
