; basic interpreter.

(import :optparse)
(import :rand)

(<- $statements '(DEF DIM END FOR TO STEP NEXT GOSUB RETURN GOTO IF THEN INPUT
                      LET ON GOTO ON GOSUB PRINT READ DATA RESTORE REM STOP)
    $functions '(ABS ASC ATN CHR$ COS EXP INT LEFT$ LEN LOG MID$ NOT RIGHT$ RND
                     SGN SIN SPC SQR STR$ TAN TAB VAL)
    $operators '(^ * / + - = < > <> <= >= AND OR)
    $reserved-words (concat $statements $functions $operators)
    $token nil
    ;; runtime
    $code nil
    $ip 0    ; instruction pointer.
    $sp 0    ; statement pointer.
    $dp '(0) ; data pointer. (line-no :rest data)
    $sc 0    ; screen cursor.
    $built-ins (dict)
    $vars (dict)
    $arrays (dict)
    $procs (dict)
    $basic-true -1
    $basic-nil 0
    $call-stack nil
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
    (if (in? value $reserved-words) value
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
    `(DEF :name ,name :params ,params :body ,(parse-expr rd))))

(function parse-dim (rd)
  ;; DIM declaration [, declaration] ...
  ;; declaration = identifier(dimension)
  ;; dimension = expr [, expr] ...
  `(DIM ,@(parse-csv rd
                     (f (rd)
                       (list :name (parse-identifier rd)
                             :dim (parse-paren rd (f (rd) (parse-csv rd parse-expr))))))))

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
    `(FOR :var ,var :from ,from :to ,to :step ,step)))

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
    `(IF :test ,test :then ,then)))

(function parse-input (rd)
  ;; INPUT [prompt ;] var [,var]...
  (let (prompt nil)
    (when (== (peek-token rd) :string)
      (get-token rd)
      (<- prompt (get-token-value rd))
      (get-token rd :semicolon))
    `(INPUT :prompt ,prompt :vars ,(parse-csv rd parse-var))))

(function parse-let (rd)
  ;; LET var=expr
  `(LET :var ,(parse-var rd)
        :val ,(begin (get-token rd '=) (parse-expr rd))))

(function parse-on (rd)
  ;; ON expr stmt addr [, addr] ...
  ;; stmt = GOTO | GOSUB
  (let (expr (parse-expr rd) stmt (get-token rd))
    (if (! (in? stmt '(GOTO GOSUB))) (raise SyntaxError "require GOTO or GOSUB")
        `(ON :expr ,expr :stmt ,stmt :addrs ,(parse-csv rd parse-number)))))

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
  (while (.next rd) (.get rd))
  `(REM ,(.token rd)))

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

(function ->ip (lno)
  (dotimes (i (len $code))
    (let ((line-no :rest stmts) ([] $code i))
      (if (= lno line-no) (return i))))
  (raise ArgumentError (str "missing code:" lno)))

(function basic-string-var? (name)
  (= (last (str name)) "$"))

(function basic-default-value (var)
  (if (basic-string-var? var) "" 0))

(function basic-coerce (var val)
  (catch (Exception (f (e) (return (basic-default-value var))))
    (if (basic-string-var? var) (str val) (float val))))

(function basic-get-var (var)
  (let (val ([] $vars var))
    (if (nil? val) (basic-set-var var (basic-default-value var))
        val)))

(function basic-set-var (var val)
  ([] $vars var (basic-coerce var val)))

(function basic-array (name :opt message indices val)
  (if (== message :at)
      (let (val ([] (basic-array name) indices))
        (if (nil? val) (basic-array name :put (basic-default-value name))
            val))
      (== message :put)
      ([] (basic-array name) indices (basic-coerce name val))
      :defualt
      (let (arr ([] $arrays name))
        (if (nil? arr) ([] $arrays name (dict))
            arr))))

(function basic-proc (name :opt val)
  (if (nil? val)
      (let (proc ([] $procs name))
        (if (nil? proc) (raise StateError (str "undefined procedure " name))
            proc))
      ([] $procs name val)))

(function basic-assign (expr val)
  (if (atom? expr) (basic-set-var expr val)
      (basic-array (car expr) :put (map basic-eval-expr (cdr expr)) val)))

(function basic-seek (:opt start)
  (let (lno (|| start (car $dp)))
    (<- $dp nil)
    (doarray (code $code)
      (let ((line-no (stmt :rest data) :rest stmts) code)
        (if (&& (== stmt 'DATA) (> line-no lno))
            (return (<- $dp (cons line-no data))))))))

(function basic-read ()
  (if (nil? $dp) 0
      (let ((line-no :rest data) $dp)
        (if (nil? data)
            (begin
              (basic-seek)
              (basic-read))
            (begin0
              (car data)
              (<- $dp (cons line-no (cdr data))))))))

(function basic-write (x)
  (dostring (ch (str x))
    (if (= ch "\n") (<- $sc 0)
        (<- $sc (+ $sc (wcwidth ch))))
    (write-bytes ch)))

(function basic-bool (x)
  (if x $basic-true $basic-nil))

(class BasicJump (Exception) ip sp)

(function basic-jump (ip :opt sp)
  (throw (&sp! (&ip! (.new BasicJump) ip) (|| sp 0))))

(function basic-goto (addr)
  (basic-jump (->ip addr) 0))

(function basic-gosub (addr)
  (push! (list $ip $sp) $call-stack)
  (basic-goto addr))

(function basic-apply (proc args)
  (let ((params body) proc)
    (eval `(let (,params ',args) (basic-eval-expr ',body)))))

(function basic-eval-expr (x)
  (if (symbol? x) (basic-get-var x)
      (atom? x) x
      (let (ope (car x) args (map basic-eval-expr (cdr x)))
        (if (in? ope $built-ins) (apply ([] $built-ins ope) args)
            (in? ope $procs) (basic-apply ([] $procs ope) args)
            (basic-array ope :at args)))))

(function basic-eval-stmt (x)
  (apply ([] $built-ins (car x)) (cdr x)))

;;; Statements.

(macro basic-built-in (name args :rest body)
  `([] $built-ins ',name (f ,args ,@body)))

(basic-built-in DATA (:rest vars)
  nil)

(basic-built-in DEF (:key name params body)
  (basic-proc name (list params body)))

(basic-built-in DIM (:rest args)
  nil)    ; dynamically allocate memory.

(basic-built-in END ()
  (quit))

(basic-built-in FOR (:key var from to step)
  (basic-set-var var (basic-eval-expr from))
  (push! (list $ip $sp :var var :to (basic-eval-expr to) :step (basic-eval-expr step)) $for-stack))

(basic-built-in GOSUB (addr)
  (basic-gosub addr))

(basic-built-in GOTO (addr)
  (basic-goto addr))

(basic-built-in IF (:key test then)
  (if (= (basic-eval-expr test) $basic-nil) (basic-jump (++ $ip))
      (if (number? then) (basic-jump (->ip then))
          (basic-eval-stmt then))))

(basic-built-in INPUT (:key prompt vars)
  (loop
    (catch (Error (f (e) (basic-write (.to-s e)) (<- $sc 0)))
      (if prompt (basic-write prompt))
      (basic-write "\nINPUT> ")
      (let (vals (split (upper (read-line)) ",") vlen (len vals))
        (if (!= (len vars) vlen)
            (raise Error (str "require " (len vars) " arguments, given " vlen " arguments")))
        (dotimes (i vlen)
          (let (var ([] vars i))
            (basic-assign ([] vars i) ([] vals i)))))
      (return nil))))

(basic-built-in LET (:key var val)
  (basic-assign var (basic-eval-expr val)))

(basic-built-in NEXT (:rest vars)
  (dolist (next (|| vars (list (assoc (car $for-stack) :var))))
    (if (nil? $for-stack) (raise SyntaxError (str "invalid NEXT statement"))
        (let ((ip sp :key var to step) (car $for-stack) val nil)
          (if (!= next var) (raise SyntaxError (str "missing FOR of NEXT " next $for-stack))
              (begin
                (basic-set-var var (<- val (+ (basic-get-var var) step)))
                (if (|| (&& (> step 0) (> val to))
                        (&& (< step 0) (< val to)))
                    (pop! $for-stack)
                    (basic-jump ip (++ sp)))))))))

(basic-built-in ON (:key expr stmt addrs)
  (let (val (basic-eval-expr expr) addr ([] addrs (-- val)))
    (if addr
        (if (== stmt 'GOTO) (basic-goto addr)
            (basic-gosub addr)))))

(basic-built-in PRINT (:rest args)
  (let (newline? true)
    (dolist (x args)
      (if (== x :semicolon) (<- newline? nil)
          (== x :comma)
          (begin
            (loop (basic-write " ") (if (= (% $sc 14) 0) (break)))
            (<- newline? nil))
          (begin
            (basic-write (basic-eval-expr x))
            (<- newline? true))))
    (if newline? (basic-write "\n"))))

(basic-built-in READ (:rest vars)
  (dolist (var vars)
    (basic-assign var (basic-read))))

(basic-built-in REM (str) nil)

(basic-built-in RESTORE (:opt addr)
  (basic-seek addr))

(basic-built-in RETURN ()
  (if (nil? $call-stack) (raise StateError "invalid RETURN statement")
      (let ((ip sp) (pop! $call-stack))
        (basic-jump ip (++ sp)))))

(basic-built-in STOP ()
  (write-line "STOP")
  (write (list :vars $vars
               :arrays $arrays
               :procs $procs
               :call-stack $call-stack
               :for-stack $for-stack))
  (quit))

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
(basic-built-in INT (x) (int (if (< x 0) (-- x) x)))
(basic-built-in LEFT$ (x y) (slice x 0 (min (len x) y)))
(basic-built-in LEN (x) (len x))
(basic-built-in LOG (x) (log x))
(basic-built-in MID$ (x y :opt z) (slice x (-- y) (if (nil? z) (len x) (min (+ y z) (len x)))))
(basic-built-in NOT (x) (basic-bool (= x 0)))
(basic-built-in RIGHT$ (x y) (slice x (max 0 (- (len x) y))))
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
(basic-built-in + (x y) (if (string? x) (str x y) (+ x y)))
(basic-built-in - (x :opt y) (if (nil? y) (- x) (- x y)))
(basic-built-in = (x y) (basic-bool (= x y)))
(basic-built-in < (x y) (basic-bool (< x y)))
(basic-built-in > (x y) (basic-bool (> x y)))
(basic-built-in <> (x y) (basic-bool (!= x y)))
(basic-built-in <= (x y) (basic-bool (<= x y)))
(basic-built-in >= (x y) (basic-bool (>= x y)))
(basic-built-in AND (x y) (bmtonum (apply & (map numtobm (list x y)))))
(basic-built-in OR (x y) (bmtonum (apply | (map numtobm (list x y)))))

(function interpret ()
  (loop
    (catch (BasicJump (f (e) (<- $ip (&ip e) $sp (&sp e))))
      (let ((line-no :rest stmts) ([] $code $ip))
        (dolist (stmt (slice stmts $sp))
          (basic-eval-stmt stmt)
          (<- $sp (++ $sp)))
        (<- $ip (++ $ip) $sp 0)))))

;; Loader.

(function split-line (line)
  (let (i (memmem line " "))
    (list (int (slice line 0 i)) (slice line (++ i)))))

(function load-code (file)
  (with-open ($in file :read)
    (<- $code (array (map parse-line
                          (sort! (map split-line (collect read-line))
                                 :key car))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "i") args))
    (if (nil? args) (raise ArgumentError "require basic source code")
        (begin
          (load-code (car args))
          (if (.get op "i") (foreach write (array->list $code))
              (interpret))))))
