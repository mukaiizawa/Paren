; whitespace interpreter.

(import :optparse)

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
          (:SP :jzero)
          (:TAB :jneg)
          (:LF :ret))
        (:LF
          (:LF :end)))))

(<- $code nil
    $ip 0
    $stack nil
    $call-stack nil
    $heap (dict))

(function walk-tree (fx)
  (let (rec (f (tree :opt acc)
              (if (keyword? tree) (fx (cons tree (reverse acc)))
                  (foreach (f (x) (rec x (cons (car tree) acc)))
                           (cdr tree)))))
    (foreach rec $IMP)))

(function peek-ch (rd)
  (let (x (.next rd))
    (if (nil? x) nil
        (= x $SP) :SP
        (= x $TAB) :TAB
        (= x $LF) :LF
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
  (let (step (f (x :opt acc)
               (if (keyword? (car x)) (car x)
                   (let (ch (next-ch rd) next-branch (select1 (f (y) (= (car y) ch)) x))
                     (if next-branch (step (cdr next-branch) (cons (car next-branch) acc))
                         (nil? ch) (raise EOFError "unexpected EOF")
                         (raise SyntaxError (str "unknown instruction " (reverse (cons ch acc)))))))))
    (if (peek-ch rd) (step $IMP))))

(function parse (rd)
  (if (nil? (peek-ch rd)) nil
      (let (inst (parse-inst rd))
        (if (in? inst '(:push :copy :ndrop :mark :gosub :goto :jzero :jneg))
            (list inst (parse-int rd))
            (list inst)))))

(function load-code (file)
  (debug-write :code-generation)
  (with-open ($in file :read)
    (let (rd (.new AheadReader))
      (<- $code (array (collect (f () (debug-write (parse rd)))))))))

(function ws-apply (inst req fx)
  (if (<= req (len $stack)) (fx)
      (raise StateError (str inst "/not enough elements on the stack"))))

(function ws-apply-binary (inst fx)
  (ws-apply inst 2 (f () (let (x (pop! $stack) y (pop! $stack)) (push! (fx y x) $stack)))))

(function ws-store (val addr)
  ([] $heap addr val))

(function ws-load (addr)
  (let (val ([] $heap addr))
    (if (nil? val) (raise StateError "load/missing value")
        val)))

(class WSJump (Error))

(function ws-jump (inst req label cond fx)
  (let (ip nil)
    (dotimes (i (len $code))
      (let ((inst :opt arg) ([] $code i))
        (if (&& (== inst :mark) (= label arg))
            (begin
              (<- ip i)
              (break)))))
    (if (nil? ip) (raise StateError (str inst "/missing label")))
    (ws-apply inst req (f () (fx ip)))
    (when cond
      (<- $ip ip)
      (raise WSJump))))

(function ws-return ()
  (let (ip (pop! $call-stack))
    (if (nil? ip) (raise StateError "return/call-stack must not be empty")
        (begin
          (<- $ip ip)
          (raise WSJump)))))

(function interpret (code)
  (debug-write :interpret)
  (loop
    (catch (WSJump (f (e) nil))
      (let ((inst :opt arg) ([] code $ip))
        (debug-write `(:ip ,$ip ,inst ,arg :stack ,$stack :call-stack ,$call-stack :heap ,$heap))
        (if (== inst :push) (ws-apply inst 0 (f () (push! arg $stack)))
            (== inst :copy) (ws-apply inst (++ arg) (f () (push! ([] $stack arg) $stack)))
            (== inst :ndrop) (ws-apply inst (++ arg) (f () (cdr! $stack (slice $stack (++ arg)))))
            (== inst :dup) (ws-apply inst 1 (f () (push! (car $stack) $stack)))
            (== inst :swap) (ws-apply inst 2 (f () (swap! $stack 0 1)))
            (== inst :drop) (ws-apply inst 1 (f () (pop! $stack)))
            (== inst :add) (ws-apply-binary inst +)
            (== inst :sub) (ws-apply-binary inst -)
            (== inst :mul) (ws-apply-binary inst *)
            (== inst :div) (ws-apply-binary inst //)
            (== inst :mod) (ws-apply-binary inst %)
            (== inst :store) (ws-apply inst 2 (f () (ws-store (pop! $stack) (pop! $stack))))
            (== inst :load) (ws-apply inst 1 (f () (push! (ws-load (pop! $stack)) $stack)))
            (== inst :putc) (ws-apply inst 1 (f () (write-bytes (chr (pop! $stack)))))
            (== inst :puti) (ws-apply inst 1 (f () (write-bytes (str (pop! $stack)))))
            (== inst :getc) (ws-apply inst 1 (f () (ws-store (ord (read-char)) (pop! $stack))))
            (== inst :geti) (ws-apply inst 1 (f () (ws-store (int (read-line)) (pop! $stack))))
            (== inst :mark) nil
            (== inst :gosub) (ws-jump inst 0 arg true (f (ip) (push! (++ $ip) $call-stack)))
            (== inst :goto) (ws-jump inst 0 arg true (f (ip) nil))
            (== inst :jzero) (ws-jump inst 1 arg (= (pop! $stack) 0) (f (ip) nil))
            (== inst :jneg) (ws-jump inst 1 arg (< (pop! $stack) 0) (f (ip) nil))
            (== inst :ret) (ws-return)
            (== inst :end) (quit)
            (assert nil))
        (<- $ip (++ $ip))))))

(function debug-write (expr)
  (if (nil? expr) nil
      (nil? $ws-debug?) expr
      (write expr)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "dL:sS:T:") args))
    (<- $ws-debug? (.get op "d")
        $SP (|| (.get op "S") " ")
        $TAB (|| (.get op "T") "\t")
        $LF (|| (.get op "L") "\n"))
    (if (.get op "s") (walk-tree write)
        (nil? args) (raise ArgumentError "require source file")
        (interpret (load-code (car args))))))
