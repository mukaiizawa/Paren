; Morse code.

(import :optparse)

; International Morse code.
; 1. The length of a dot is one unit.
; 2. A dash is three units.
; 3. The space between parts of the same letter is one unit.
; 4. The space between letters is three unit.
; 5. The space between words is seven unit.

(<- $word-space "       "
    $letter-space "   "
    $table
    '((a .-)
      (b -...)
      (c -.-.)
      (d -..)
      (e .)
      (f ..-.)
      (g --.)
      (h ....)
      (i ..)
      (j .---)
      (k -.-)
      (l .-..)
      (m --)
      (n -.)
      (o ---)
      (p .--.)
      (q --.-)
      (r .-.)
      (s ...)
      (t -)
      (u ..-)
      (v ...-)
      (w .--)
      (x -..-)
      (y -.--)
      (z --..)
      (1 .----)
      (2 ..---)
      (3 ...--)
      (4 ....-)
      (5 .....)
      (6 -....)
      (7 --...)
      (8 ---..)
      (9 ----.)
      (0 -----)))

(function encode-ch (ch)
  (find (f (x) (if (= (str (car x)) ch) (str (cadr x))))
        $table))

(function encode-word (word)
  (join (map encode-ch (split word))
        $letter-space))

(function encode (line)
  (join (map encode-word (except empty? (split line " ")))
        $word-space))

(function decode-ch (ch)
  (find (f (x) (if (= (str (cadr x)) ch) (str (car x))))
        $table))

(function decode-word (word)
  (join (map decode-ch (except empty? (split word " ")))))

(function decode (line)
  (join (map decode-word (split line $word-space))
        " "))

(function morse-code (decode?)
  (let (line nil)
    (while (<- line (read-line))
      (write-line (if (nil? decode?) (encode line)
                      (decode line))))))

(function! main (args)
  ; # NAME
  ; morse-code [OPTION]
  ;
  ; # DESCRIPTION
  ; Encode and decode standard input with Morse code.
  ;
  ; # OPTION
  ;     -d -- Decode standard input.
  (let ((op args) (.parse (.init (.new OptionParser) "d") args))
    (morse-code (.get op "d"))))
