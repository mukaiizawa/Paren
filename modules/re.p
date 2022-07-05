; regex module.

;; Regex.

(class Re ()
  elements
  text
  reader
  match-start
  match-end
  anchored-start?
  anchored-end?)

(method Re .step (elt i)
  (if (< i (.text-length self))
      (let (key elt->key)
        (if (== key :char) (if (= (.char-at self i) elt->val) (++ i))
            (== key :alternate) (keep1 (f (x) (if (.try self x i) self->match-end)) elt->val)
            (== key :any) (++ i)
            (== key :char-class) (if (in? (.char-at self i) elt->val) (++ i))
            (== key :exclude-char-class) (if (! (in? (.char-at self i) elt->val)) (++ i))
            (assert nil)))))

(method Re .try-n-times (elt i n)
  (dotimes (j n)
    (if (! (<- i (.step self elt i))) (return nil)))
  i)

(method Re .try (elements i)
  (when (nil? elements)
    (<- self->match-end i)
    (if (! self->anchored-end?) (return true)
        (return (&& (= i (.text-length self))))))
  (let (elt (car elements) n elt->n m (|| elt->m (- (.text-length self) i)) next-i nil)
    (if elt->greedy?
        (while (<= n m)
          (if (&& (<- next-i (.try-n-times self elt i m))
                  (.try self (cdr elements) next-i))
              (return true))
          (<- m (-- m)))
        (while (<= n m)
          (if (&& (<- next-i (.try-n-times self elt i n))
                  (.try self (cdr elements) next-i))
              (return true))
          (<- n (++ n))))))

(method Re .char-at (i)
  ([] self->text i))

(method Re .text-length ()
  (len self->text))

(method Re .subtext (start :opt end)
  (with-memory-stream ($out)
    (for (i start end (|| end (.text-length self))) (< i end) (i (++ i))
      (write-bytes (.char-at self i)))))

(method Re .match-start ()
  self->match-start)

(method Re .match-end ()
  self->match-end)

(method Re .match-string ()
  (.subtext self self->match-start self->match-end))

(method Re .match? (s :opt start)
  (<- self->match-start (|| start (<- start 0))
      self->text (array s))
  (if self->anchored-start? (return (&& (= start 0) (.try self self->elements 0))))
  (for (i start e (.text-length self)) (<= i e) (i (++ i))
    (<- self->match-start i)
    (if (.try self self->elements i) (return true))))

;; Regex Element.

(class Re.Elt ()
  key val n m greedy?)

(method Re.Elt .init (key :opt val)
  (<- self->key key
      self->val val)
  self)

;; Regex Compiler.

(class Re.Compiler ()
  reader)

(method Re.Compiler .parse-any ()
  (.skip self->reader)
  (.init (.new Re.Elt) :any))

(method Re.Compiler .parse-group ()
  (.skip self->reader)
  (let (val nil)
    (push! (.parse self) val)
    (while (= (.next self->reader) "|")
      (.skip self->reader)
      (push! (.parse self) val))
    (.skip self->reader ")")
    (.init (.new Re.Elt) :alternate (reverse! val))))

(method Re.Compiler .parse-charset ()
  (.skip self->reader)
  (let (key (if (= (.next self->reader) "^") (begin (.skip self->reader) :exclude-char-class) :char-class))
    (while (! (= (.next self->reader) "]"))
      (<- c (.skip-escape self->reader))
      (if (= (.next self->reader) "-")
          (begin (.skip self->reader)
                 (for (s (ord c) e (ord (.skip self->reader))) (<= s e) (s (++ s))
                   (.put self->reader (chr s))))
          (.put self->reader c)))
    (.skip self->reader)
    (.init (.new Re.Elt) key (.token self->reader))))

(method Re.Compiler .parse-char ()
  (.init (.new Re.Elt) :char (.skip-escape self->reader)))

(method Re.Compiler .parse-quantifier (elt)
  (let (ch (.next self->reader) n 1 m 1 greedy? true)
    (if (= ch "*") (begin (.skip self->reader) (<- n 0 m nil))
        (= ch "?") (begin (.skip self->reader) (<- n 0 m 1))
        (= ch "+") (begin (.skip self->reader) (<- n 1 m nil))
        (= ch "{") (begin
                     (.skip self->reader)
                     (<- n (.skip-uint self->reader))
                     (if (= (.next self->reader) "}") (<- m n)
                         (begin
                           (.skip self->reader ",")
                           (if (= (.next self->reader) "}") (<- m nil)
                               (<- m (.skip-uint self->reader)))))
                     (.skip self->reader "}")))
    (when (= (.next self->reader) "?")
      (.skip self->reader)
      (<- greedy? nil))
    (<- elt->n n
        elt->m m
        elt->greedy? greedy?)
    elt))

(method Re.Compiler .parse ()
  (let (elements nil ch nil)
    (while (&& (<- ch (.next self->reader)) (! (in? ch '("|" ")"))))
      (push! (.parse-quantifier self (if (= ch ".") (.parse-any self)
                                         (= ch "(") (.parse-group self)
                                         (= ch "[") (.parse-charset self)
                                         (.parse-char self)))
             elements))
    (reverse! elements)))

(method Re.Compiler .compile (expr)
  ; Returns the instance on Re corresponds to the specified regular expression expr.
  ; The supported regular expressions is follows.
  ;     # regular expression
  ;         ^ -- start of string
  ;         $ -- end of string
  ;         . -- any single character
  ;         [...] -- any character in set
  ;         [^...]  -- not any character in the set
  ;         (x | y ...)   match any expression, y, y... in order
  ;     # quantifiers
  ;         greedy  lazy
  ;         ----------------------------------------------------------------------------------
  ;         *       *?       match zero or more times.
  ;         ?       ??       match one or more times.
  ;         +       +?       match zero or one time.
  ;         {n}     {n}?     match exactly n times.
  ;         {n,}    {n,}?    match at least n times.
  ;         {n,m}   {n,m}?   match from n to m times.
  (let (re (.new Re))
    (if (prefix? expr "^") (<- re->anchored-start? true expr (slice expr 1)))
    (if (suffix? expr "$") (<- re->anchored-end? true expr (slice expr 0 (-- (len expr)))))
    (with-memory-stream ($in expr)
      (<- self->reader (.new AheadReader)
          re->elements (.parse self)))
    re))

;; API

(function re.compile (expr)
  (if (is-a? expr Re) expr
      (.compile (.new Re.Compiler) expr)))

(function re.match (expr s :opt start)
  ; Returns Regular expression expr matched string.
  ; If the s does not match the expr, returns nil.
  ; If start is specified, match from the start th.
  (let (re (re.compile expr))
    (if (.match? re s start) (.match-string re))))

(function re.match-all (expr s :opt start)
  ; Same as re.match except that it returns a list of all matching strings.
  (let (match1 (f (re start acc)
                 (if (.match? re s start) (match1 re (.match-end re) (cons (.match-string re) acc))
                     (reverse! acc))))
    (match1 (re.compile expr) start nil)))

(function re.split (expr s :opt max-split)
  ; Returns list of strings delimited by the string that matches the regular expression.
  ; If max-split is specified, the maximum length of the list of return values is max-split.
  (let (split (f (re start max-split acc)
                (if (&& (!= max-split 0) (.match? re s start))
                    (begin
                      (push! (.subtext re start (.match-start re)) acc)
                      (split re (.match-end re) (&& (number? max-split) (-- max-split)) acc))
                    (begin
                      (push! (.subtext re start) acc)
                      (reverse! acc)))))
    (split (re.compile expr) 0 max-split nil)))

(function re.replace (expr src dest :opt max-replace)
  ; Returns the string that matches the regular expression is replaced with the specified string.
  ; If max-replace is specified, the upper limit of the number of replacements is max-replace.
  (let (replace (f (re start max-replace mem)
                  (if (&& (!= max-replace 0) (.match? re src start))
                      (begin
                        (.write-bytes mem (.subtext re start (.match-start re)))
                        (.write-bytes mem dest)
                        (replace re (.match-end re) (&& (number? max-replace) (-- max-replace)) mem))
                      (begin
                        (.write-bytes mem (.subtext re start))
                        (.to-s mem)))))
    (replace (re.compile expr) 0 max-replace (.new MemoryStream))))

(function! main (args)
  ;; anchore start
  (assert (= (re.match (re.compile "^") "") ""))
  (assert (= (re.match (re.compile "a^") "a^") "a^"))
  (assert (! (re.match (re.compile "^a") "ba")))
  ;; anchore end
  (assert (= (re.match (re.compile "$") "") ""))
  (assert (= (re.match (re.compile "$a") "$a$") "$a"))
  (assert (! (re.match (re.compile "a$") "ab")))
  ;; char
  (let (re (re.compile "a"))
    (assert (= (re.match re "a") "a"))
    (assert (= (re.match re "za") "a"))
    (assert (! (re.match re "b"))))
  ;; any
  (let (re (re.compile "a.c"))
    (assert (= (re.match re "aabc") "abc"))
    (assert (= (re.match re ".axc") "axc"))
    (assert (! (re.match re "ac"))))
  ;; character class
  (let (re (re.compile "[a-c]"))
    (assert (= (re.match re "0a9") "a"))
    (assert (= (re.match re "0b9") "b"))
    (assert (= (re.match re "0c9") "c"))
    (assert (! (re.match re "0"))))
  (let (re (re.compile "[^a-c]"))
    (assert (= (re.match re "a0b") "0"))
    (assert (! (re.match re "abc"))))
  ;; alternate
  (let (re (re.compile "(ab|cd)"))
    (assert (= (re.match re "aab") "ab"))
    (assert (= (re.match re "acd") "cd"))
    (assert (! (re.match re "bbc"))))
  (let (re (re.compile "(ab*|cd*)"))
    (assert (= (re.match re "a") "a"))
    (assert (= (re.match re "abb") "abb"))
    (assert (= (re.match re "ca") "c"))
    (assert (= (re.match re "cdd") "cdd"))
    (assert (= (re.match re "abbcd") "abb"))
    (assert (! (re.match re "bd"))))
  ;; quantifiers
  (let (re (re.compile "^a*$"))
    (assert (= (re.match re "") ""))
    (assert (= (re.match re "a") "a"))
    (assert (= (re.match re "aaa") "aaa"))
    (assert (! (re.match re "aaab"))))
  (let (re (re.compile "a{2}"))
    (assert (= (re.match re "aa") "aa"))
    (assert (! (re.match re "a"))))
  (assert (= (re.compile "a?") (re.compile "a{0,1}")))
  (assert (= (re.compile "a??") (re.compile "a{0,1}?")))
  (assert (= (re.compile "a*") (re.compile "a{0,}")))
  (assert (= (re.compile "a*?") (re.compile "a{0,}?")))
  (assert (= (re.compile "a+") (re.compile "a{1,}")))
  (assert (= (re.compile "a+?") (re.compile "a{1,}?")))
  ; api
  (assert (= (re.match "o+" "fooo") "ooo"))
  (assert (! (re.match "o{3}" "foo")))
  (let ((:opt foo bar buzz) (re.match-all "[fb].*?," "foo,bar,buzz"))
    (assert (= foo "foo,"))
    (assert (= bar "bar,"))
    (assert (nil? buzz)))
  (let (re (re.compile "[,:]"))
    (let ((:opt foo bar buzz) (re.split re "foo,bar,buzz"))
      (assert (= foo "foo"))
      (assert (= bar "bar"))
      (assert (= buzz "buzz")))
    (let ((foo bar-buzz) (re.split re "foo:bar:buzz" 1))
      (assert (= foo "foo"))
      (assert (= bar-buzz "bar:buzz"))))
  (let (re (re.compile "[,\\-:]"))
    (assert (= (re.replace re "foo,bar-buzz" "|||") "foo|||bar|||buzz"))
    (assert (= (re.replace re "foo-bar:buzz" "|||" 1) "foo|||bar:buzz"))))
