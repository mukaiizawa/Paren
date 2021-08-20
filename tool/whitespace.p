; whitespace interpreter.

(<- $next nil)


(<- $IMP
    '((:SP
        (:SP :push)
        (:TAB
          (:SP :copy)
          (:LF :ndrop))
        (:LF
          (:SP :dup)
          (:TAB :swap)
          (:LF :drop)))
      (:TAB
        (:SP
          (:SP
            (:SP :add)
            (:TAB :sub)
            (:LF :mul))
          (:TAB
            (:SP :div)
            (:TAB :mod)))
        (:TAB
          (:SP :store)
          (:TAB :load))
        (:LF
          (:SP
            (:SP :putc)
            (:TAB :puti))
          (:TAB
            (:SP :getc)
            (:TAB :geti))))
      (:LF
        (:SP
          (:SP :mark)
          (:TAB :gosub)
          (:LF :goto))
        (:TAB
          (:SP :jump-if-zero)
          (:TAB :jump-if-neg)
          (:LF :return))
        (:LF
          (:LF :end)))))

(function inst-set (tree :opt acc)
  (if (keyword? tree) (write (cons tree (reverse acc)))
      (foreach (f (x) (inst-set x (cons (car tree) acc)))
               (cdr tree))))
; (foreach inst-set $IMP)

(function next-ch (rd)
  (let (x (if (.next rd) (.skip rd) nil))
    (if (nil? x) :EOF
        (= x " ") :SP
        (= x "\t") :TAB
        (= x "\n") :LF
        (next-ch rd))))

(function parse-inst (rd)
  (let (walk (f (x :opt acc)
               (if (keyword? (car x)) (car x)
                   (let (ch (next-ch rd) next-branch (find (f (y) (if (= (car y) ch) y)) x))
                     (if next-branch (walk (cdr next-branch) (cons (car next-branch) acc))
                         (== ch :EOF) (raise EOFError "unexpected EOF")
                         (raise SyntaxError (str "unknown instruction" (reverse (cons ch acc)))))))))
    (walk $IMP)))

(function parse (rd)
  (let (inst (parse-inst rd))
    (write inst)
    ))

(function load-code (file)
  (with-open ($in file :read)
    (let (rd (.new AheadReader))
      (return (collect (f () (parse rd)))))))

(function! main (args)
  (if (nil? args) (raise ArgumentError "require source file")
      (load-code (car args))))
