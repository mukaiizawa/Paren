; whitespace interpreter.

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

(<- $code nil
    $ip 0
    $stack nil
    $call-stack nil
    $heap (dict))

(function inst-set (tree :opt acc)
  (if (keyword? tree) (write (cons tree (reverse acc)))
      (foreach (f (x) (inst-set x (cons (car tree) acc)))
               (cdr tree))))
; (foreach inst-set $IMP)

(function peek-ch (rd)
  (let (x (.next rd))
    (if (nil? x) nil
        (= x " ") :SP
        (= x "\t") :TAB
        (= x "\n") :LF
        (begin
          (.skip rd)
          (peek-ch rd)))))

(function next-ch (rd)
  (begin0
    (peek-ch rd)
    (.skip rd)))

(function parse-int (rd)
  (let (sign nil val 0 ch (next-ch rd))
    (<- sign (if (== ch :SP) 1 (== ch :TAB) -1 (raise EOFError "missing sign")))
    (loop
      (if (nil? (<- ch (next-ch rd))) (raise EOFError "numbers must be terminated with LF")
          (== ch :SP) (<- val (* val 2))
          (== ch :TAB) (<- val (+ (* val 2) 1))
          (== ch :LF) (return (* sign val))
          (assert nil)))))

(function parse-inst (rd)
  (let (walk (f (x :opt acc)
               (if (keyword? (car x)) (car x)
                   (let (ch (next-ch rd) next-branch (find (f (y) (if (= (car y) ch) y)) x))
                     (if next-branch (walk (cdr next-branch) (cons (car next-branch) acc))
                         (nil? ch) (raise EOFError "unexpected EOF")
                         (raise SyntaxError (str "unknown instruction" (reverse (cons ch acc)))))))))
    (if (peek-ch rd) (walk $IMP))))

(function parse (rd)
  (if (nil? (peek-ch rd)) nil
      (let (inst (parse-inst rd))
        (if (in? inst '(:push :copy :ndrop :mark :gosub :goto :jump-if-zero :jump-if-neg))
            (list inst (parse-int rd))
            (list inst)))))

(function load-code (file)
  (with-open ($in file :read)
    (let (rd (.new AheadReader))
      (return (<- $code (array (collect (f () (parse rd)))))))))

(function label->ip (label)
  (dotimes (i (len $code))
    (let ((inst :opt arg) ([] $code i))
      (if (&& (== inst :mark) (= arg label)) (return i)))))

(function interpret (code)
  (loop
    (let ((inst :opt arg) ([] code $ip))
      (if (== inst :push) (push! arg $stack)
          (== inst :copy) (if (< arg (len $stack)) (push! ([] $stack arg) $stack)
                              (raise StateError "copy/index out of bounds"))
          (== inst :ndrop) (if (< arg (len $stack)) (<- $stack (cons (car $stack) (slice $stack arg)))
                               (raise StateError "ndrop/index out of bounds"))
          (== inst :dup) (if (nil? $stack) (raise StateError "copy/stack must not be empty")
                             (push! (car $stack) $stack))
          (== inst :swap) (let (x (pop! $stack) y (pop! $stack))
                            (if (&& x y)
                                (begin
                                  (push! x $stack)
                                  (push! y $stack))
                                (raise StateError "swap/stack must not be empty")))
          (== inst :drop) (if (nil? $stack) (raise StateError "drop/stack must not be empty")
                              (pop! $stack))
          (== inst :add) (let (x (pop! $stack) y (pop! $stack))
                           (if (&& x y) (push! (+ y x) $stack)
                               (raise StateError "add/two values must be on the stack")))
          (== inst :sub) (let (x (pop! $stack) y (pop! $stack))
                           (if (&& x y) (push! (- y x) $stack)
                               (raise StateError "sub/two values must be on the stack")))
          (== inst :mul) (let (x (pop! $stack) y (pop! $stack))
                           (if (&& x y) (push! (* y x) $stack)
                               (raise StateError "mul/two values must be on the stack")))
          (== inst :div) (let (x (pop! $stack) y (pop! $stack))
                           (if (&& x y) (push! (// y x) $stack)
                               (raise StateError "div/two values must be on the stack")))
          (== inst :mod) (let (x (pop! $stack) y (pop! $stack))
                           (if (&& x y) (push! (% y x) $stack)
                               (raise StateError "mod/two values must be on the stack")))
          (== inst :store) (let (x (pop! $stack) y (pop! $stack))
                             (if (&& x y) ([] $heap y x)
                                 (raise StateError "store/two values must be on the stack")))
          (== inst :load) (let (x (pop! $stack))
                            (if (nil? x) (raise StateError "load/one values must be on the stack")
                                (nil? (<- x ([] $heap x))) (raise StateError "load/missing value")
                                (push! x $stack)))
          (== inst :putc) (let (x (pop! $stack))
                            (if (nil? x) (raise StateError "putc/one values must be on the stack")
                                (write-bytes (chr x))))
          (== inst :puti) (let (x (pop! $stack))
                            (if (nil? x) (raise StateError "puti/one values must be on the stack")
                                (write-bytes (str x))))
          (== inst :getc) (let (x (pop! $stack))
                            (if (nil? x) (raise StateError "getc/one values must be on the stack")
                                ([] $heap x (ord (read-char)))))
          (== inst :geti) (let (x (pop! $stack))
                            (if (nil? x) (raise StateError "geti/one values must be on the stack")
                                ([] $heap x (int (read-line)))))
          (== inst :mark) nil
          (== inst :gosub) (let (ip (label->ip arg))
                             (if (nil? ip) (raise StateError "goto/missing label")
                                 (begin
                                   (push! $ip $call-stack)
                                   (<- $ip ip)
                                   (continue))))
          (== inst :goto) (let (ip (label->ip arg))
                            (if (nil? ip) (raise StateError "goto/missing label")
                                (begin
                                  (<- $ip ip)
                                  (continue))))
          (== inst :jump-if-zero) (let (x (pop! $stack) ip (label->ip arg))
                                    (if (nil? ip) (raise StateError "goto/missing label")
                                        (nil? x) (raise StateError "jump-if-zero/one values must be on the stack")
                                        (= x 0)
                                        (begin
                                          (<- $ip ip)
                                          (continue))))
          (== inst :jump-if-neg) (let (x (pop! $stack) ip (label->ip arg))
                                   (if (nil? ip) (raise StateError "goto/missing label")
                                       (nil? x) (raise StateError "jump-if-zero/one values must be on the stack")
                                       (< x 0)
                                       (begin
                                         (<- $ip ip)
                                         (continue))))
          (== inst :return) (let (ip (pop! $call-stack))
                              (if (nil? ip) (raise StateError "return/call-stack must not be empty")
                                  (<- $ip ip)))
          (== inst :end) (quit)
          (assert nil))
      (<- $ip (++ $ip)))))

(function! main (args)
  (if (nil? args) (raise ArgumentError "require source file")
      (interpret (load-code (car args)))))
