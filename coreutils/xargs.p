; build and execute command lines from standard input.

(function read-args ()
  (with-memory-stream ($out)
    (let (c nil)
      (while (/= (<- c (read-byte)) -1)
        (write-byte c)))))

(function xargs (args delim combine?)
  ; xargs [OPTION] COMMAND [INITIAL-ARGUMENTS]...
  ;     -d Separate by the specified delimiter
  ;     -s Consider space as delimiters
  ;     -c Execute a command with the entire standard input as an argument
  (if combine? (system (join (append args (read-args)) " "))
      ))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "d:sc") args))
    (xargs args delim (.get op "d") (.get op "c"))))
