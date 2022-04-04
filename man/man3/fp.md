# NAME
fp - get a pointer to a standard I/O streams.

# SYNOPSIS

    (fp FD)

# DESCRIPTION
The function `fp` get a pointer to a standard I/O streams.

# RETURN VALUE
Returns the file pointer associated with the file descriptor `FD`.

The argument `FD` can specify bellow value.

     0 -- stdin
     1 -- stdout
     2 -- stderr

# EXAMPLES

    ) (= (fp 0) (&fp $stdin))
    true
    
    ) (= (fp 1) (&fp $stdout))
    true
    
    ) (= (fp 2) (&fp $stderr))
    true

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
