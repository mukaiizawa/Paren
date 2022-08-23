# NAME
fp - get a pointer to a standard I/O streams.

# SYNOPSIS

    (fp FD)

# DESCRIPTION
The function `fp` get a pointer to a standard I/O streams.

This function is usually not used directly, but instead `$stdin(3), $stdout(3), $stderr(3)` is used.

# RETURN VALUE
Returns the file pointer associated with the file descriptor `FD`.

The argument `FD` can specify bellow value.

     0 -- stdin
     1 -- stdout
     2 -- stderr

# SEE ALSO
- `$stderr(3)`
- `$stdin(3)`
- `$stdout(3)`
- `chdir(3)`
- `clock(3)`
- `cycle(3)`
- `fclose(3)`
- `fgetc(3)`
- `fgets(3)`
- `fopen(3)`
- `fputc(3)`
- `fread(3)`
- `fseek(3)`
- `ftell(3)`
- `fwrite(3)`
- `getcwd(3)`
- `mkdir(3)`
- `readdir(3)`
- `remove(3)`
- `rename(3)`
- `sleep(3)`
- `stat(3)`
- `time(3)`
- `utcoffset(3)`
- `utime(3)`
