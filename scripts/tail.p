; tail.

(import :optparse)

(function tail (n)
  (let (i 0 buf (array n) line nil)
    (while (<- line (read-line))
      ([] buf (% i n) line)
      (<- i (++ i)))
    (if (< i n) (<- n i i 0))
    (dotimes (_ n)
      (write-line ([] buf (% i n)))
      (<- i (++ i)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args))
    (tail (.get-int op "n" 10))))
