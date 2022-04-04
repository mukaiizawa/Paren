# NAME
$in, $out - I/O streams.

# DESCRIPTION
The symbols `$in` and `$out` hold the stream that the functions listed in the `SEE ALSO` section, implicitly reference when reading and writing.

Immediately after execution, they are connected to `$stdin(3)` and `$stdout(3)`.

The macro `with-memory-stream(3)` allows you to connect to a string I/O stream.

# SEE ALSO
- `read(3)`
- `read-byte(3)`
- `read-bytes(3)`
- `read-char(3)`
- `read-line(3)`
- `with-memory-stream(3)`
- `write(3)`
- `write-byte(3)`
- `write-bytes(3)`
- `write-line(3)`
