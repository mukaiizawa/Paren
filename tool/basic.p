; basic interpreter.

(import :rand)

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

(method BasicLexer .lex-gt ()
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
        (= next ">") (.lex-gt self)
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

;;; Expression.

(function parse-number (rd)
  (get-token rd :number)
  (get-token-value rd))

(function parse-identifier (rd)
  (get-token rd :identifier)
  (get-token-value rd))

(function parse-string (rd)
  (get-token rd :string)
  (get-token-value rd))

(function parse-var (rd)
  (let (var (parse-identifier rd))
    (if (!= (peek-token rd) :open-paren) var
        (cons var (parse-paren rd (f (rd) (parse-csv rd parse-expr)))))))

(function parse-csv (rd parse1)
  (let (acc nil)
    (push! (parse1 rd) acc)
    (while (== (peek-token rd) :comma)
      (get-token rd :comma)
      (push! (parse1 rd) acc))
    (reverse! acc)))

(function parse-paren (rd parse1)
  (get-token rd :open-paren)
  (begin0
    (parse1 rd)
    (get-token rd :close-paren)))

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
  (let (token (peek-token rd))
    (if (== token :open-paren) (parse-paren rd parse-expr)
        (== token :identifier) (parse-var rd)
        (== token :number) (parse-number rd)
        (== token :string) (parse-string rd)
        (in? token $functions) (cons (get-token rd) (parse-paren rd (f (rd) (parse-csv rd parse-expr))))
        (raise SyntaxError (str "unknown token " token)))))

(function parse-expr (rd)
  (parse-0 rd))

;;; Statement.

(function parse-data (rd)
  ;; DATA expr [, expr] ...
  ;; expr = number | identifier | string
  `(DATA ,@(parse-csv rd
                      (f (rd)
                        (let (token (peek-token rd))
                          (if (== token :identifier) (str (parse-identifier rd))
                              (== token :number) (parse-number rd)
                              (== token :string) (parse-string rd)
                              (== token '-) (begin (get-token rd) (- (parse-number rd)))
                              (raise SyntaxError "invalid DATA statement")))))))

(function parse-def (rd)
  ;; DEF name(params)=body
  ;; params = [param] ...
  (let (name (parse-identifier rd)
             params (parse-paren rd (f (rd) (parse-csv rd parse-var))))
    (get-token rd '=)
    `(DEF :NAME ,name :PARAMS ,params :BODY ,(parse-expr rd))))

(function parse-dim (rd)
  ;; DIM declaration [, declaration] ...
  ;; declaration = identifier(dimension)
  ;; dimension = expr [, expr] ...
  `(DIM ,@(parse-csv rd
                     (f (rd)
                       (list :NAME (parse-identifier rd)
                             :DIM (parse-paren rd (f (rd) (parse-csv rd parse-expr))))))))

(function parse-for (rd)
  ;; FOR identifier = expr TO expr [STEP expr]
  (let ((:opt var from to step) nil)
    (<- var (parse-identifier rd))
    (get-token rd '=)
    (<- from (parse-expr rd))
    (get-token rd 'TO)
    (<- to (parse-expr rd))
    (if (!= (peek-token rd) 'STEP) (<- step 1)
        (begin
          (get-token rd 'STEP)
          (<- step (parse-expr rd))))
    `(FOR :VAR ,var :FROM ,from :TO ,to :STEP ,step)))

(function parse-goxx (rd name)
  ;; GOTO addr
  ;; GOSUB addr
  `(,name ,(parse-number rd)))

(function parse-if (rd)
  ;; IF test [THEN] { line-no | statement }
  (let (test (parse-expr rd) then nil)
    (if (== (peek-token rd) 'THEN) (get-token rd))
    (if (!= (peek-token rd) :number) (<- then (parse-stmt rd))
        (begin
          (get-token rd)
          (<- then (get-token-value rd))))
    `(IF :TEST ,test :THEN ,then)))

(function parse-input (rd)
  ;; INPUT [prompt ;] var [,var]...
  (let (prompt nil)
    (when (== (peek-token rd) :string)
      (get-token rd)
      (<- prompt (get-token-value rd))
      (get-token rd :semicolon))
    `(INPUT :PROMPT ,prompt :VARS ,(parse-csv rd parse-var))))

(function parse-let (rd)
  ;; LET var=expr
  `(LET :VAR ,(parse-var rd)
        :VAL ,(begin (get-token rd '=) (parse-expr rd))))

(function parse-on (rd)
  ;; ON expr stmt addr [, addr] ...
  ;; stmt = GOTO | GOSUB
  (let (expr (parse-expr rd) stmt (get-token rd))
    (if (! (in? stmt '(GOTO GOSUB))) (raise SyntaxError "require GOTO or GOSUB")
        `(ON :EXPR ,expr :STMT ,stmt :ADDRESSES ,(parse-csv rd parse-number)))))

(function parse-next (rd)
  ;; NEXT [identifier [, identifier] ...]
  `(NEXT ,@(if (== (peek-token rd) :identifier) (parse-csv rd parse-identifier))))

(function parse-print (rd)
  ;; PRINT [expr | , | ;] ...
  (let (exprs nil token nil)
    (loop
      (if (in? (<- token (peek-token rd)) '(:EOL :colon)) (break)
          (in? token '(:semicolon :comma)) (push! (get-token rd) exprs)
          (push! (parse-expr rd) exprs)))
    `(PRINT ,@(reverse! exprs))))

(function parse-read (rd)
  ;; READ var [, var] ...
  `(READ ,@(parse-csv rd parse-var)))

(function parse-rem (rd)
  ;; REM char ...
  (while (.next rd) (.skip rd))
  '(REM))

(function parse-restore (rd)
  ;; RESTORE [addr]
  `(RESTORE ,(if (== (peek-token rd) :number) (parse-number rd))))

(function parse-stmt (rd)
  (let (stmt (get-token rd))
    (if (== stmt 'DATA) (parse-data rd)
        (== stmt 'DEF) (parse-def rd)
        (== stmt 'DIM) (parse-dim rd)
        (== stmt 'FOR) (parse-for rd)
        (== stmt 'IF) (parse-if rd)
        (== stmt 'INPUT) (parse-input rd)
        (== stmt 'LET) (parse-let rd)
        (== stmt 'NEXT) (parse-next rd)
        (== stmt 'ON) (parse-on rd)
        (== stmt 'PRINT) (parse-print rd)
        (== stmt 'READ) (parse-read rd)
        (== stmt 'REM) (parse-rem rd)
        (== stmt 'RESTORE) (parse-restore rd)
        (in? stmt '(GOTO GOSUB)) (parse-goxx rd stmt)
        (in? stmt '(END RETURN STOP)) (list stmt)
        (== stmt :identifier) (begin (unget-token stmt) (parse-let rd))    ; implicit let.
        (raise SyntaxError (str "unknown statement " stmt (get-token-value rd))))))

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

(function basic-write (x)
  (dostring (ch (str x))
    (if (= ch "\n") (<- $sc 0)
        (<- $sc (+ $sc (wcwidth ch))))
    (write-bytes ch)))

(function basic-bool (x)
  (if x -1 0))

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

(macro basic-built-in (name args :rest body)
  `([] $vars ',name (f ,args ,@body)))

;;; Statements.

(basic-built-in DATA () nil)

(basic-built-in DEF (:key NAME PARAMS BODY)
  ([] $vars NAME (eval (list f PARAMS BODY))))

(basic-built-in DIM (:rest args)
  (dolist (arg args)
    (let ((:key NAME DIM) arg
              size (apply * (map ++ (map basic-eval DIM)))
              init-val (if (= (last (str NAME)) "$") "" 0)
              arr (array size))
      (dotimes (i size) ([] arr i init-val))
      ([] $vars NAME arr))))

(basic-built-in END () (quit))

(basic-built-in FOR (:key VAR FROM TO STEP)
  ([] $vars VAR (basic-eval FROM))
  (push! (list $ip $sp :VAR VAR :TO (basic-eval TO) :STEP (basic-eval STEP)) $for-stack))

(basic-built-in GOSUB () (quit))

(basic-built-in GOTO () (quit))

(basic-built-in IF () (quit))

(basic-built-in INPUT () (quit))

(basic-built-in NEXT (:rest vars)
  (dolist (var (|| vars (list (assoc (car $for-stack) :VAR))))
    (if (nil? $for-stack) (raise SyntaxError (str "invalid NEXT statement"))
        (let ((ip sp :key VAR TO STEP) (car $for-stack) val nil)
          (if (!= VAR var) (raise SyntaxError (str "missing FOR of NEXT " var))
              (begin
                ([] $vars VAR (<- val (+ ([] $vars VAR) STEP)))
                (if (|| (&& (> STEP 0) (> val TO))
                        (&& (< STEP 0) (< val TO)))
                    (pop! $for-stack)
                    (basic-jump ip (++ sp)))))))))

(basic-built-in ON () (quit))

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

(basic-built-in READ () (quit))

(basic-built-in REM () (quit))

(basic-built-in RESTORE () (quit))

(basic-built-in RETURN () (quit))

(basic-built-in STOP () (quit))

;;; Functions.

(function make-space (x)
  (with-memory-stream ($out)
    (dotimes (_ x) (write-bytes " "))))

(basic-built-in ABS (x) (abs x))
(basic-built-in ASC (x) (ord x))
(basic-built-in ATN (x) (atan x))
(basic-built-in CHR$ (x) (chr x))
(basic-built-in COS (x) (cos x))
(basic-built-in EXP (x) (exp x))
(basic-built-in INT (x) (if (int? x) x (< x 0) (int (-- x)) (int x)))
(basic-built-in LEFT$ (x y) (slice x 0 y))
(basic-built-in LEN (x) (len x))
(basic-built-in LOG (x) (log x))
(basic-built-in MID$ (x y z) (slice x y z))
(basic-built-in NOT (x) (basic-bool (= x 0)))
(basic-built-in RIGHT$ (x y) (slice x (- (len x) y) (len x)))
(basic-built-in RND (x) (rand.val))
(basic-built-in SGN (x) (if (= x 0) 0 (> x 0) 1 -1))
(basic-built-in SIN (x) (sin x))
(basic-built-in SPC (x) (make-space x))
(basic-built-in SQR (x) (sqrt x))
(basic-built-in STR$ (x) (str x))
(basic-built-in TAN (x) (tan x))
(basic-built-in TAB (x) (make-space (- x $sc)))
(basic-built-in VAL (x) (catch (Error (f (e) 0)) (float x)))

;;; Operators.

(function numtobm (x)
  (if (< (<- x (int x)) 0) (+ 0x100000000 x)
      x))

(function bmtonum (x)
  (if (>= x 0x80000000) (- x 0x100000000)
      x))

(basic-built-in ^ (x y) (int (pow x y)))
(basic-built-in * (x y) (* x y))
(basic-built-in / (x y) (/ x y))
(basic-built-in + (x y) (+ x y))
(basic-built-in - (x :opt y) (if (nil? y) (- x) (- x y)))
(basic-built-in = (x y) (basic-bool (= x y)))
(basic-built-in < (x y) (basic-bool (< x y)))
(basic-built-in > (x y) (basic-bool (> x y)))
(basic-built-in <> (x y) (basic-bool (!= x y)))
(basic-built-in <= (x y) (basic-bool (<= x y)))
(basic-built-in >= (x y) (basic-bool (>= x y)))
(basic-built-in AND (x y) (bmtonum (apply & (map numtobm (list x y)))))
(basic-built-in OR (x y) (bmtonum (apply | (map numtobm (list x y)))))

(class BasicJump (Exception) ip sp)

(function basic-jump (ip :opt sp)
  (throw (&sp! (&ip! (.new BasicJump) ip) (|| sp 0))))

(function interpret (code)
  (foreach write (array->list code))
  (quit)
  (loop
    (catch (BasicJump (f (e) (<- $ip (&ip e) $sp (&sp e))))
      (let ((line-no :rest stmts) ([] code $ip))
        (dolist (stmt (slice stmts $sp))
          (apply ([] $vars (car stmt)) (cdr stmt))
          (<- $sp (++ $sp)))
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
