; output the last part of files

(import :optparse)

(function tail (:opt n)
  ; tail [OPTION]
  ; Print the last 10 lines of standard input to standard output.
  ;     -n print the last n lines.
  (let (i 0 buf (array n) line nil)
    (while (<- line (read-line))
      ([] buf (% i n) line)
      (<- i (++ i)))
    (if (< i n) (<- n i i 0))
    (dotimes (_ n)
      (write-line ([] buf (% i n)))
      (<- i (++ i)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args) n (.get op "n"))
    (tail (if n (int n) 10))))
