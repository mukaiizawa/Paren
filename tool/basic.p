; basic interpreter.

(<- $statements '(DEF DIM END FOR TO STEP NEXT GOSUB RETURN GOTO IF THEN INPUT
                      LET ON GOTO ON GOSUB PRINT READ DATA RESTORE REM STOP)
    $functions '(ABS ASC ATN CHR$ COS EXP INT LEFT$ LEN LOG MID$ NOT RIGHT$ RND
                     SGN SIN SPC SQR STR$ TAN TAB VAL)
    $operators '(^ * / + - = < > <> <= >= AND OR)
    $built-in (concat $statements $functions $operators)
    $code nil
    $ip 0
    $vars (dict)
    $token nil
    $for-stack nil)

;; Lexer.

(class BasicLexer (AheadReader)
  key val)

(method BasicLexer .lex-lt ()
  (.skip self)
  (if (= (.next self) "=") (begin (.skip self) '<=)
      (= (.next self) ">") (begin (.skip self) '<>)
      '<))

(method BasicLexer .let-gt ()
  (.skip self)
  (if (= (.next self) "=") (begin (.skip self) '>=)
      '>))

(method BasicLexer .lex-string ()
  (.skip self)
  (while (!= (&next self) "\"") (.get-escape self))
  (.skip self "\"")
  (.token self))

(method BasicLexer .lex-identifier ()
  (.get self)
  (while (|| (.next? self alpha?) (.next? self digit?)) (.get self))
  (if (= (.next self) "$") (.get self))
  (let (value (symbol (upper (.token self))))
    (if (in? value $built-in) value
        (list :identifier value))))

(method BasicLexer .lex0 ()
  (let (next (.next (.skip-space self)))
    (if (nil? next) :EOL
        (= next ":") (begin (.skip self) :colon)
        (= next ";") (begin (.skip self) :semicolon)
        (= next ",") (begin (.skip self) :comma)
        (= next "(") (begin (.skip self) :open-paren)
        (= next ")") (begin (.skip self) :close-paren)
        (= next "<") (.lex-lt self)
        (= next ">") (.let-gt self)
        (= next "\"") (list :string (.lex-string self))
        (.next? self digit?) (list :number (.skip-number self))
        (.next? self alpha?) (.lex-identifier self)
        (raise SyntaxError "illegal token"))))

(method BasicLexer .lex ()
  (let ((key :opt val) (->list (.lex0 self)))
    (&key! self key)
    (&val! self val)
    key))

(function get-token (rd :opt expected)
  (let (token (if (nil? $token) (.lex rd) (pop! $token)))
    (if (&& expected (!= token expected)) (raise SyntaxError "unexpected token")
        token)))

(function get-token-value (rd)
  (&val rd))

(function peek-token (rd)
  (let (token (get-token rd))
    (unget-token token)
    token))

(function unget-token (token)
  (assert (nil? $token))
  (push! token $token))

;; Parser.

(macro parse-binary (level :rest syms)
  (let (parse-n (symbol (str 'parse- level)) parse-n+1 (symbol (str 'parse- (++ level))))
    `(function! ,parse-n (rd)
       (let (expr (,parse-n+1 rd) token nil)
         (while (in? (<- token (get-token rd)) ',syms)
           (<- expr (list token expr (,parse-n+1 rd))))
         (unget-token token)
         expr))))

(parse-binary 0 OR)
(parse-binary 1 AND)
(parse-binary 2 = < > <> <= >=)
(parse-binary 3 + -)
(parse-binary 4 * /)
(parse-binary 5 ^)

(function parse-6 (rd)
  ;; Unary `-`.
  (let (token (get-token rd))
    (if (== token '-) (list token (parse-6 rd))
        (begin
          (unget-token token)
          (parse-7 rd)))))

(function parse-7 (rd)
  (let (token (get-token rd))
    (if (== token :open-paren) (begin0 (parse-expr rd) (get-token rd :close-paren))
        (== token :identifier) (parse-identifier rd)
        (in? token '(:string :number)) (get-token-value rd)
        (in? token $functions) (cons token (parse-paren rd))
        (raise SyntaxError (str "unknown token " token)))))

(function parse-identifier (rd)
  (let (identifier (get-token-value rd))
    (if (== (peek-token rd) :open-paren) (cons identifier (parse-paren rd))
        identifier)))

(function parse-paren (rd)
  (let (exprs nil)
    (get-token rd :open-paren)
    (push! (parse-expr rd) exprs)
    (while (== (peek-token rd) :comma)
      (get-token rd)
      (push! (parse-expr rd) exprs))
    (get-token rd :close-paren)
    (reverse! exprs)))

(function parse-expr (rd)
  (parse-0 rd))

(function parse-print (rd)
  (let (exprs nil token nil)
    (loop
      (if (in? (<- token (peek-token rd)) '(:EOL :colon)) (break)
          (in? token '(:semicolon :comma)) (push! (get-token rd) exprs)
          (push! (parse-expr rd) exprs)))
    `(PRINT ,@(reverse! exprs))))

; (function parse-for (rd)
;   (let ((:opt var from to step) nil)
;     (<- var (.get-identifier (.skip-space rd)))
;     (.skip rd "=")
;     (<- from (.skip-number (.skip-space rd)))
;     `(let (var ,var from ,from to ,to step ,step)
;        ([] $vars var from)
;        (push! (list :ip $ip :var var :to to :step step) $for-stack))))

; (function parse-next (rd)
;   (let (vars nil)
;     (push! (.get-identifier (.skip-space rd)) vars)
;     (while (= (.next (.skip-space rd)) ",") (push! (.get-identifier rd) vars))
;     (map (f (x)
;            `(let ((:key ip var to step) (car $for-stack))
;               (if (!= var ,x) (raise SyntaxError (str "unexpected NEXT " x))
;                   (let (val ([] $vars var))
;                     ([] $vars var (<- val (+ step val)))
;                     (if (|| (&& (> step 0) (> val to)) (&& (< step 0) (< val to))) (pop! $for-stack)
;                         (<- $ip ip))))))
;          (reverse! vars))))

(function parse-goto (rd)
  (let (line-no (.skip-uint (.skip-space rd)))
    `(begin
       (<- $ip (find (f (i) (if (= (car ([] $code i)) ,line-no) i))
                     (.. (len $code))))
       (continue))))

(function parse-stmt (rd)
  (let (stmt (get-token rd))
    (if (== stmt 'PRINT) (parse-print rd)
        (== stmt 'END) '(END)
        (== stmt 'FOR) :for
        (== stmt 'NEXT) :next
        (raise SyntaxError (str "unknown statement " stmt)))))

(function parse-line (x)
  (let ((line-no line) x)
    (with-memory-stream ($in line)
      (let (rd (.new BasicLexer) stmts nil)
        (loop
          (push! (parse-stmt rd) stmts)
          (if (== (peek-token rd) :colon) (get-token rd)
              (begin
                (get-token rd :EOL)
                (break))))
        (cons line-no (reverse! stmts))))))

;; Evaluater.

(macro basic-built-in (name args :rest body)
  `(begin
     (function ,name ,args ,@body)
     ([] $vars ',name ,name)))

(basic-built-in TAB (width)
  (with-memory-stream ($out)
    (dotimes (_ width) (write-bytes " "))))

(basic-built-in END ()
  (quit))

(basic-built-in PRINT (:rest args)
  (let (newline? true)
    (dolist (x args)
      (if (== x :semicolon) (<- newline? nil)
          (== x :comma) (begin (write-bytes " todo comma ") (<- newline? nil))
          (write-bytes (str (basic-eval x)))))
    (if newline? (write-line))
    (<- $ip (++ $ip))))

(function basic-eval (x)
  (if (atom? x) x
      (let ((operator :rest args) x)
        (basic-apply operator (map basic-eval args)))))

(function basic-apply (operator args)
  (apply ([] $vars operator) args))

(function interpret (code)
  (foreach write (array->list code))
  ; (quit)
  (loop
    (let ((line-no :rest stmts) ([] code $ip))
      (foreach eval stmts))))

;; Loader.

(function split-line (line)
  (let (i (memmem line " "))
    (list (int (slice line 0 i)) (slice line (++ i)))))

(function load-code (file)
  (with-open ($in file :read)
    (return (<- $code
                (array (map parse-line
                            (sort! (map split-line (collect read-line))
                                   :key car)))))))

(function! main (args)
  (if (nil? args) (raise ArgumentError "require basic source code")
      (interpret (load-code (car args)))))
