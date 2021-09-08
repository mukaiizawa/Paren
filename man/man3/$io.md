# NAME
$in, $out -- I/O streams.

# DESCRIPTION
The symbols $in and $out hold the stream that the functions listed below implicitly reference when reading and writing.

    $in
        read(3) -- read S-expression.
        read-byte(3) -- read 1 byte.
        read-bytes(3) -- reads the specified length or all the rest.
        read-char(3) -- read 1 character.
        read-line(3) -- read one line.
    
    $out
        write(3) -- write in a format that can be read by read(3).
        write-byte(3) -- write 1byte.
        write-bytes(3) -- write bytes-like object.
        write-line(3) -- write bytes-like object and new line.

Immediately after execution, they are connected to $stdin(3) and $stdout(3).

Macro with-memory-stream(3) allows you to connect to a string I/O stream.

# SEE ALSO
- read(3)
- read-byte(3)
- read-bytes(3)
- read-char(3)
- read-line(3)
- with-memory-stream(3)
- write(3)
- write-byte(3)
- write-bytes(3)
- write-line(3)
