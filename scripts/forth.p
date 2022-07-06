; forth interpreter.

(<- $forth-nil 0
    $forth-true 1
    $stack nil
    $dictionary (dict))

(class ForthError (Error))

; Lexer.

(class ForthLexer (AheadReader))

(method ForthLexer .skip-comment ()
  (while (!= (.next self) ")") (.skip self))
  (.skip self)
  self)

(method ForthLexer .skip-line-comment ()
  (.skip-line self)
  self)

(method ForthLexer .skip-word ()
  (.skip-space self)
  (while (! (space? (.next self))) (.get self))
  (.token self))

(method ForthLexer .lex ()
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (= next "(") (.lex (.skip-comment self))
        (= next "\\") (.lex (.skip-line-comment self))
        (|| (= next "-") (digit? next)) (.skip-number self)
        (.skip-word self))))

; Reader.

(class ForthReader ()
  lexer token)

(method ForthReader .init () 
  (<- self->lexer (.new ForthLexer))
  self)

(method ForthReader .scan ()
  (<- self->token (.lex self->lexer))
  self)

(method ForthReader .parse-definition ()
  (list :definition
        (collect (f ()
                   (let (x (.parse (.scan self)))
                     (if (!= x :semicolon) x))))))

(method ForthReader .parse-if ()
  (let (x nil then nil else nil)
    (while (! (in? (<- x (.parse (.scan self))) '(:then :else)))
      (push! x then))
    (when (= x :else)
      (while (!= (<- x (.parse (.scan self))) :then)
        (push! x else)))
    (list :if (reverse! then) (reverse! else))))

(method ForthReader .parse ()
  (let (token (&token self))
    (if (nil? token) (raise SystemExit)
        (= token ":") (.parse-definition self)
        (= token "if") (.parse-if self)
        (= token "else") :else
        (= token "then") :then
        (= token ";") :semicolon
        (number? token) token
        (symbol token))))

(function forth-read () 
  (.parse (.scan (.new ForthReader))))

; Evaluate.

(function forth-number? (x)
  (number? x))

(function forth-built-in? (x)
  (&& (symbol? x) (function? ([] $dictionary x))))

(function forth-function? (x)
  (&& (symbol? x) (list? ([] $dictionary x))))

(function forth-definition? (x)
  (&& (list? x) (== (car x) :definition)))

(function forth-if? (x)
  (&& (list? x) (== (car x) :if)))

(function forth-bool (x)
  (if (nil? x) $forth-nil $forth-true))

(function forth-apply-built-in (x)
  (apply ([] $dictionary x) '()))

(function forth-apply (x)
  (foreach forth-eval ([] $dictionary x)))

(function forth-define (x)
  (let ((key (name :rest body)) x)
    (assert (== key :definition))
    (if (! (symbol? name)) (raise ForthError "illegal function name") 
        ([] $dictionary name body))))

(function forth-if (x)
  (let ((key then :opt else) x)
    (assert (== key :if))
    (foreach forth-eval (if (= (forth-pop) $forth-nil) else then))))

(function forth-top ()
  (car $stack))

(function forth-pop ()
  (if (nil? $stack) (raise ForthError "stack under flow")
      (pop! $stack)))

(function forth-push (x)
  (push! x $stack))

; Built-in-functions.

(macro forth-built-in (name :rest body)
  `([] $dictionary ',name (f () ,@body)))

(macro forth-binary-built-in (name :opt fn)
  (with-gensyms (x1 x2)
    `(forth-built-in ,name
                     (let (,x1 (forth-pop) ,x2 (forth-pop))
                       (forth-push (apply (eval ,(|| fn name)) (list ,x2 ,x1)))))))

(macro forth-binary-comparator (name :opt fn)
  `(forth-binary-built-in ,name
      ,(eval `(f (:rest args) (forth-bool (apply ,(|| fn name) args))))))

(function ok () (write-line "ok"))
(function write1 (x) (write x :end " "))

(forth-built-in .s (foreach write1 (reverse $stack)) (ok))
(forth-built-in . (write1 (forth-pop)) (ok))
(forth-built-in bye (raise SystemExit))

;; Arithmetic functions.
(forth-binary-built-in +)
(forth-binary-built-in *)
(forth-binary-built-in /)
(forth-binary-comparator <)
(forth-binary-comparator >)
(forth-binary-comparator =)
(forth-binary-comparator <> !=)

;; Stack Manipulation.
(forth-built-in dup (forth-push (forth-top)))
(forth-built-in drop (forth-pop))
(forth-built-in swap (let (x (forth-pop) y (forth-pop)) (forth-push x) (forth-push y)))
(forth-built-in rot (let (x (forth-pop) y (forth-pop) z (forth-pop)) (forth-push y) (forth-push x) (forth-push z)))
(forth-built-in nip (swap) (drop))
(forth-built-in tuck (swap) (over))

(function forth-eval (x)
  (if (forth-number? x) (push! x $stack)
      (forth-built-in? x) (forth-apply-built-in x)
      (forth-function? x) (forth-apply x)
      (forth-definition? x) (forth-define x)
      (forth-if? x) (forth-if x)
      (raise ForthError "illegal syntax")))

(function interpret ()
  (loop
    (catch (ForthError (f (e) (write-line (.to-s e))))
      (forth-eval (forth-read)))))

(function! main (args)
  (if (nil? args) (interpret)
      (with-open ($in (car args) :read)
        (interpret))))
