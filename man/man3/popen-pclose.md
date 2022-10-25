# NAME
popen, pclose - pipe stream to or from a process.

# SYNOPSIS

    (popen COMMAND MODE)
    (pclose FP)

# DESCRIPTION
The function `popen` opens a process by creating a pipe, forking, and invoking the shell.

Since a pipe is by definition unidirectional, the type argument may specify only reading or writing, not both; the resulting stream is correspondingly read-only or write-only.

The argument `MODE` can specify bellow value.

     0 -- Open file for reading.
     1 -- Open file for writing.

# RETURN VALUE
The function `popen` returns a pointer to an open stream that can be used to read or write to the pipe.

The function `pclose` returns the `nil`.

# EXAMPLES

    ) (let (fp (popen "paren echo foo" 0) line (fgets fp))
        (pclose fp)
        line)
    "foo"

    ) (let (fp (popen "paren cat" 1))
        (fputc 0x66 fp)
        (fputc 0x6f fp)
        (fputc 0x6f fp)
        (fputc 0x0a fp)
        (pclose fp))
    foo
    nil

# SEE ALSO
- `fclose(3)`
- `fopen(3)`
- `system(3)`
- `with-process(3)`
