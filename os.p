; OS module.

(global-symbol OS.name
  ; Host os.
  ; Determined by compile-time arguments.
  ; The values to be set are as follows.
  ; - :windows
  ; - :linux
  ; - :android
  ; - :mac
  )

(builtin-function OS.fp (fd)
  (assert (= (&fp $stdin) (OS.fp 0))))

(builtin-function OS.fopen (filename mode)
  ; Opens the file whose name is the string pointed to by filename and associates a stream with it.
  ; The argument mode can specify bellow value.
  ;      0 -- Open file for reading.
  ;      1 -- Open file for writing.
  ;      3 -- Open file for appending
  ;      4 -- Open file for reading and writing.
  )

(builtin-function OS.fgetc (fp)
  ; Reads the next character from stream and returns it.
  ; Return  -1 if EOF.
  )

(builtin-function OS.fputc (fp c)
  ; Write the byte specified by c to the output stream pointed to by fp. 
  )

(builtin-function OS.fgets (fp)
  ; Read a line from the steream pointed to by fp and return it.
  ; Do not include newline characters.
  ; Returns nil if EOF.
  )

(builtin-function OS.fread (buf from size fp)
  ; Reads size bytes of data from the stream pointed to by fp, storing them at the location given by byte-array buf offset from.
  ; Returns size;
  )

(builtin-function OS.fwrite (buf from size fp)
  ; Writes size bytes of data to the stream pointed to by fp, obtaining them at the location given by byte-array buf offset from.
  ; Returns size;
  )

(builtin-function OS.fseek (fp)
  ; Sets the file position indicator for the stream pointed to by fp
  ; Returns nil.
  )

(builtin-function OS.ftell (fp)
  ; Returns the current value of the file position indicator for the stream pointed to by fp.
  )

(builtin-function OS.fclose (fp)
  ; Flushes the stream pointed to by fp (writing any buffered output data) and closes the underlying file descriptor.
  ; Returns nil.
  )

(builtin-function OS.stat (filename)
  ; Returns the file status indicated filename.
  ; The return value is an array of length 3.
  ;     0 -- file type and mode.
  ;         1 none
  ;         2 file
  ;         4 dir
  ;         8 other
  ;         16 readable
  ;         32 writable
  ;     1 -- file size
  ;     2 -- modification timestamp
  )

(builtin-function OS.utime (filename unix-time)
  ; Change the access time and modification time of the file indicated filename to the specified unix-time in times.
  ; Returns nil.
  )

(builtin-function OS.getcwd ()
  ; Returns a string containing the absolute filename of the current working directory.
  )

(builtin-function OS.chdir (filename)
  ; Change the current working directory to the directory specified in filename
  ; Returns nil.
  )

(builtin-function OS.readdir (filename)
  ; Return the contents of the directory indicated by filename as a character string delimited by a newline character.
  )

(builtin-function OS.remove (filename)
  ; Attempts to remove a file whose name is pathname.
  ; Returns nil.
  )

(builtin-function OS.mkdir (filename)
  ; Attempts to create a directory whose name is pathname.
  ; It is an error if filename already exists.
  ; Returns nil.
  )

(builtin-function OS.rename (src dst)
  ; Rename the file and move between directories if necessary.
  ; Returns nil.
  )

(builtin-function OS.time ()
  ; Returns the number of seconds relative to the unix epoch (January 1, 1970, 00:00:00 UTC).
  (assert (OS.time)))

(builtin-function OS.clock ()
  ; Returns the approximate processor time[sec] used by the program.
  (assert (OS.clock)))

(builtin-function OS.sleep (sec)
  ; Sleep for a specified number of seconds.
  ; Returns nil.
  (assert (! (OS.sleep 0.001))))

(builtin-function OS.getenv (name)
  ; Looks up the environment variable named name in the environment list and returns value string.
  ; Returns nil if not found.
  )

(builtin-function OS.putenv (name value)
  ; Add environment variables or change values.
  ; If name does not exist in the environment, name-value is added to the environment.
  ; If name exists in the environment, the value of name is changed to value.
  ; Returns nil.
  )
