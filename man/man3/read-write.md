# NAME
read, read-byte, read-bytes, read-char, read-line, write, write-byte, write-bytes, write-line, print, println - input and output functions.

# SYNOPSIS

    (read)
    (read-byte)
    (read-bytes [BYTES [FROM [SIZE]]])
    (read-char)
    (read-line)
    (write-byte BYTE)
    (write-bytes BYTES [FROM [SIZE]])
    (write-line [BYTES])
    (write EXPR [:start START] [:end END])
    (print EXPR ...)
    (println EXPR ...)

# DESCRIPTION
These functions read from the stream to which $in is bound and write to the stream to which $out is bound.

    $in
        read -- read S-expression.
        read-byte -- read 1 byte.
        read-bytes -- if BYTES is not specified, read until the stream reaches the end. Otherwise, read at most SIZE and write from the FROMth of the BYTES.
        read-char -- read 1 character.
        read-line -- read 1 line.
    
    $out
        write -- write S-expression in a format that can be read by function read.
        write-byte -- write a byte BYTE.
        write-bytes -- if BYTES is not specified write a bytes BYTES. Otherwise, write the length SIZE from the FROMth of BYTES.
        write-line -- write a bytes BYTES and new line(LF -- 0x0a).
        print -- print multiple S-expressions in a more human-readable format.
        println -- print multiple S-expressions in a more human-readable format and new line(LF -- 0x0a).

The initial values of `FROM`, `SIZE`, `START` and `END` are `0`, `(len BYTES)`, `""` and `"\n"` respectively.

# RETURN VALUE
The function read returns the result of S-expression read. If the stream has reached the end, `nil` is returned.

The function read-byte returns the result of a `1-byte` read. If the stream has reached the end, `-1` is returned.

The function read-bytes if `BYTES` is not specified, the bytes read until the end of the stream is reached is returned. Otherwise, returns the number of bytes successfully read.

The function read-char returns the result of reading one character. If the stream has reached the end, return `nil`. The stream is assumed to be encoded in UTF-8.

The function read-line returns the result of reading one line. If the stream is terminated, `nil` is returned. The return value does not include a newline character.

The function write returns `EXPR`.

The function write-byte returns `BYTE`.

The function write-bytes if `FROM` is not specified, returns `BYTES`. Otherwise, returns the number of bytes successfully written.

The function write-line returns `BYTES`.

The function print returns `nil`.

The function println returns `nil`.

# NOTES
If a single buffer can be used, as socket programming, `BYTES` can be specified for `read-bytes` and `write-bytes`.

# SEE ALSO
- $in(3)
- $out(3)
