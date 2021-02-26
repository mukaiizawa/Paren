; build and execute command lines from standard input.

(import :optparse)

(function xargs (cmd initial-args :key combine?)
  ; xargs [OPTION] COMMAND [INITIAL-ARGUMENTS]...
  ;     -c Execute a command with the entire standard input as an argument
  (let (run (f (cmd initial-args args)
              (system (join (cons cmd (append initial-args args)) " "))))
    (if combine? (run cmd initial-args (collect read-line))
        (foreach (f (:rest args) (run cmd initial-args args))
                 (collect read-line)))))

(function! main (args)
  (let ((op (cmd :rest initial-args)) (.parse (.init (.new OptionParser) "c") args))
    (xargs cmd initial-args :combine? (.get op "c"))))
