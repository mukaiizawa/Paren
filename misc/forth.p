; forth interpreter.

(<- $stack nil
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
  (&lexer! self (.new ForthLexer)))

(method ForthReader .scan ()
  (&token! self (.lex (&lexer self))))

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
      (<- else then then nil)
      (while (!= (<- x (.parse (.scan self))) :then)
        (push! x then)))
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

(function forth-builtin? (x)
  (&& (symbol? x) (function? ({} $dictionary x))))

(function forth-function? (x)
  (&& (symbol? x) (list? ({} $dictionary x))))

(function forth-definition? (x)
  (&& (list? x) (== (car x) :definition)))

(function forth-if? (x)
  (&& (list? x) (== (car x) :if)))

(function forth-apply-builtin (x)
  (apply ({} $dictionary x) '()))

(function forth-apply (x)
  (foreach forth-eval ({} $dictionary x)))

(function forth-define (x)
  (let ((key (name :rest body)) x)
    (assert (== key :definition))
    (if (! (symbol? name)) (raise ForthError "illegal function name") 
        ({} $dictionary name body))))

(function forth-if (x)
  (let ((key then :opt else) x)
    (assert (== key :if))
    (foreach forth-eval (if (forth-pop) then else))))

(function forth-top ()
  (car $stack))

(function forth-pop ()
  (if (nil? $stack) (raise ForthError "stack under flow")
      (pop! $stack)))

(function forth-push (x)
  (push! x $stack))

; Builtin-functions.

(macro forth-builtin (name :rest body)
  `({} $dictionary ',name (f () ,@body)))

(forth-builtin .s
  (dolist (x (reverse $stack)) (write x :end " "))
  (write-line "ok"))

(forth-builtin . (write (forth-top)))
(forth-builtin bye (raise SystemExit))

;; Arithmetics.
(macro forth-binary-builtin (name sym)
  `(forth-builtin ,name (forth-push (,sym (forth-pop) (forth-pop)))))

(forth-binary-builtin + +)
(forth-binary-builtin * *)
(forth-binary-builtin / /)
(forth-binary-builtin < <)
(forth-binary-builtin > >)
(forth-binary-builtin = =)
(forth-binary-builtin <> !=)
(forth-builtin negate (forth-push (- (forth-pop))))

;; Stack Manipulation.
(forth-builtin dup (forth-push (forth-top)))
(forth-builtin drop (forth-pop))
(forth-builtin swap (let (x (forth-pop) y (forth-pop)) (forth-push x) (forth-push y)))
(forth-builtin rot (let (x (forth-pop) y (forth-pop) z (forth-pop)) (forth-push y) (forth-push x) (forth-push z)))
(forth-builtin nip (swap) (drop))
(forth-builtin tuck (swap) (over))

(function forth-eval (x)
  (if (forth-number? x) (push! x $stack)
      (forth-builtin? x) (forth-apply-builtin x)
      (forth-function? x) (forth-apply x)
      (forth-definition? x) (forth-define x)
      (forth-if? x) (forth-if x)
      (raise ForthError "illegal syntax")))

(function interpret ()
  (while true
    (catch (ForthError (f (e) (write-line (.to-s e))))
      (forth-eval (forth-read)))))

(function! main (args)
  (if (nil? args) (interpret)
      (with-open ($in (car args) :read)
        (interpret))))
