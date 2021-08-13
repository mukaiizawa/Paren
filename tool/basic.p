; basic interpreter.

(<- $statements '(DEF DIM END FOR TO STEP NEXT GOSUB RETURN GOTO IF THEN INPUT
                      LET ON GOTO ON GOSUB PRINT READ DATA RESTORE REM STOP)
    $functions '(ABS ASC ATN CHR$ COS EXP INT LEFT$ LEN LOG MID$ NOT RIGHT$ RND
                     SGN SIN SPC SQR STR$ TAN TAB VAL)
    $operators '(^ * / + - = < > <> <= >= AND OR)
    $built-in (concat $statements $functions $operators)
    $token nil
    ;; runtime
    $code nil
    $ip 0    ; instruction pointer.
    $sp 0    ; statement pointer.
    $sc 0    ; screen cursor.
    $vars (dict)
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
        (in? (symbol next) $operators) (symbol (.skip self))
        (raise SyntaxError (str "illegal token " next)))))

(method BasicLexer .lex ()
  (let ((key :opt val) (->list (.lex0 self)))
    (&key! self key)
    (&val! self val)
    key))

(function get-token (rd :opt expected)
  (let (token (if (nil? $token) (.lex rd) (pop! $token)))
    (if (&& expected (!= token expected)) (raise SyntaxError (str "unexpected token " token " expected " expected))
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

(function parse-for (rd)
  (let ((:opt var from to step) nil)
    (get-token rd :identifier)
    (<- var (get-token-value rd))
    (get-token rd '=)
    (<- from (parse-expr rd))
    (get-token rd 'TO)
    (<- to (parse-expr rd))
    (if (!= (peek-token rd) 'STEP) (<- step 1)
        (begin
          (get-token rd 'STEP)
          (<- step (parse-expr rd))))
    `(FOR :VAR ,var :FROM ,from :TO ,to :STEP ,step)))

(function parse-next (rd)
  (let (vars nil)
    (if (== (peek-token rd) :identifier)
        (loop
          (get-token rd :identifier)
          (push! (get-token-value rd) vars)
          (if (== (peek-token rd) :comma) (get-token rd)
              (break))))
    `(NEXT ,@vars)))

(function parse-stmt (rd)
  (let (stmt (get-token rd))
    (if (== stmt 'PRINT) (parse-print rd)
        (== stmt 'END) '(END)
        (== stmt 'FOR) (parse-for rd)
        (== stmt 'NEXT) (parse-next rd)
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
  `([] $vars ',name (f ,args ,@body)))

(basic-built-in + (x y) (+ x y))
(basic-built-in - (x y) (- x y))
(basic-built-in * (x y) (* x y))
(basic-built-in / (x y) (/ x y))

(basic-built-in TAB (width)
  (with-memory-stream ($out)
    (dotimes (_ width) (write-bytes " "))))

(basic-built-in END ()
  (quit))

(basic-built-in FOR (:key VAR FROM TO STEP)
  ([] $vars VAR (basic-eval FROM))
  (push! (list $ip $sp :VAR VAR :TO (basic-eval TO) :STEP (basic-eval STEP)) $for-stack))

(basic-built-in NEXT (:rest vars)
  (dolist (var (|| vars (list (assoc (car $for-stack) :VAR))))
    (let ((ip sp :key VAR TO STEP) (car $for-stack) val nil)
      (if (!= VAR var) (raise SyntaxError (str "missing FOR of NEXT " var))
          (begin
            ([] $vars VAR (<- val (+ ([] $vars VAR) STEP)))
            (if (|| (&& (> STEP 0) (> val TO))
                    (&& (< STEP 0) (< val TO)))
                (pop! $for-stack)
                (basic-jump ip (++ sp))))))))

(basic-built-in PRINT (:rest args)
  (let (newline? true)
    (dolist (x args)
      (if (== x :semicolon) (<- newline? nil)
          (== x :comma)
          (begin
            (loop (basic-write " ") (if (= (% $sc 14) 0) (break)))
            (<- newline? nil))
          (begin
            (basic-write (basic-eval x))
            (<- newline? true))))
    (if newline? (basic-write "\n"))))

(function basic-write (x)
  (dostring (ch (str x))
    (if (= ch "\n") (<- $sc 0)
        (<- $sc (+ $sc (wcwidth ch))))
    (write-bytes ch)))

(function basic-eval (x)
  (if (symbol? x)
      (let (val ([] $vars x))
        (if (nil? val) ([] $vars x 0)
            val))
      (atom? x) x
      (let ((operator :rest args) x)
        (basic-apply operator (map basic-eval args)))))

(function basic-apply (operator args)
  (apply ([] $vars operator) args))

(class BasicJump (Exception) ip sp)

(function basic-jump (ip :opt sp)
  (throw (&sp! (&ip! (.new BasicJump) ip) (|| sp 0))))

(function interpret (code)
  (foreach write (array->list code))
  (loop
    (catch (BasicJump (f (e) (<- $ip (&ip e) $sp (&sp e))))
      (let ((line-no :rest stmts) ([] code $ip))
        (dolist (stmt stmts)
          (if (> $sp 0) (begin (<- $sp (-- $sp)) (continue))
              (apply ([] $vars (car stmt)) (cdr stmt))))
        (<- $ip (++ $ip) $sp 0)))))

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
