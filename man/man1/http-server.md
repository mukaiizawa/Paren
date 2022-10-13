# NAME
http-server - simple HTTP server.

# SYNOPSIS

    http-server [OPTION]... [DIR]

# DESCRIPTION
Serve files relative to the `DIR` directory.

# OPTIONS

    -p PORT
        Listen on port number PORT. (default 8080)

# NOTES
Do not use in production as it is not implemented with security in mind.

# EXAMPLES
Serve current directory as context root using port number `9000`.

    $ paren http-server -p 9000

Serve `/home/www/` directory as context root using port number `8080`.

    $ paren http-server /home/www

# SEE ALSO
- `http(3)`
- `sock(3)`
