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
  (assert (= (format "%10.5x" 1) "     00001"))
  (assert (= (format "%+10.5x" 1) "    +00001"))
  (assert (= (format "%10.5f" 1) "   1.00000"))
  (assert (= (format "%10.5f" 10) "  10.00000"))
  (assert (= (format "%10.5e" 1) "1.00000e+00"))
  (assert (= (format "%10.5e" 10) "1.00000e+01"))
  (assert (= (format "%10.5g"  10) "        10"))
  (assert (= (format "%10.2s" "foo") "        fo"))
  (assert (= (format "%5.5s" "foo") "  foo"))
  (assert (= (format "%5.5s" "foobar") "fooba"))
  ;; CONV
  ;;; c
  (assert (= (format "%c" 97) "a"))
  ;;; d
  (assert (= (format "%d" -1) "-1"))
  (assert (= (format "%d" 3.1) "3"))
  (assert (= (format "%d" 2x1010) "10"))
  ;;; f
  (assert (= (format "%f" 1.0) "1.000000"))
  (assert (= (format "%e" 1.0) "1.000000e+00"))
  (assert (= (format "%g" 1.0) "1"))
  (assert (= (format "%f" 10.1) "10.100000"))
  (assert (= (format "%e" 10.1) "1.010000e+01"))
  (assert (= (format "%g" 10.1) "10.1"))
  (assert (= (format "%g" (pow 10 6)) "1e+06"))
  (assert (= (format "%g" (pow 10 -5)) "1e-05")))
