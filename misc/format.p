; format function.

(function! main (args)
  ;; FLAG
  ;;; +
  (assert (= (format "%+d" 1) "+1"))
  ;;; ' '
  (assert (= (format "% d" 1) " 1"))
  ;;; -
  (assert (= (format "%-5d" 1) "1    "))
  (assert (= (format "%-5v" :foo) ":foo "))
  (assert (= (format "%-5s" "foo") "foo  "))
  ;;; 0
  (assert (= (format "%010d" 10) "0000000010"))
  (assert (= (format "%010d" 10) "0000000010"))
  ;; WIDTH 
  (assert (= (format "%5d" 11) "   11"))
  (assert (= (format "%+5d" 11) "  +11"))
  (assert (= (format "%+-5d" 11) "+11  "))
  (assert (= (format "%+05d" 11) "+0011"))
  ;; PRECISION
  (assert (= (format "%10.5d" 1) "     00001"))
  (assert (= (format "%+10.5x" 1) "    +00001"))
  (assert (= (format "%10.5d" 1) "     00001"))
  (assert (= (format "%+10.5x" 1) "    +00001"))
  (assert (= #p(format "%10.5f" 1) "   1.00000"))
  (assert (= #p(format "%10.5f" 10) "  10.00000"))
  (assert (= #p(format "%10.5f" 1) "   1.00000"))
  (assert (= (format "%10.5f" 10) "  10.00000"))
  (assert (= (format "%10.5e" 1) "1.00000e+00"))
  (assert (= (format "%10.5e" 10) "1.00000e+01"))
  (assert (= (format "%10.5g" 10) "        10"))
  (assert (= (format "%10.5e" 1) "1.00000e+00"))
  (assert (= (format "%10.5e" 10) "1.00000e+01"))
  (assert (= (format "%10.5g" 10) "        10"))
  (assert (= (format "%10.2s" "foo") "        fo"))
  (assert (= (format "%5.5s" "foo") "  foo"))
  (assert (= (format "%5.5s" "foobar") "fooba"))

  )
