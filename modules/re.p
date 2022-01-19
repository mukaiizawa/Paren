; regex module.

(class Re ()
  elements
  text
  start end
  anchored-start?
  anchored-end?)

(class Re.Elt ()
  key val n m greedy?)

(method Re.Elt .init (key :opt val)
  (&key! self key)
  (&val! self val))

(function re.parse-any (ar)
  (.skip ar)
  (.init (.new Re.Elt) :any))

(function re.parse-group (ar)
  (.skip ar)
  (let (val nil)
    (push! (re.parse ar) val)
    (while (= (&next ar) "|")
      (.skip ar)
      (push! (re.parse ar) val))
    (.skip ar ")")
    (.init (.new Re.Elt) :alternate (reverse! val))))

(function re.parse-charset (ar)
  (.skip ar)
  (let (key (if (= (&next ar) "^") (begin (.skip ar) :exclude-char-class) :char-class))
    (while (! (= (&next ar) "]"))
      (<- c (.skip-escape ar))
      (if (= (&next ar) "-")
          (begin (.skip ar)
                 (for (s (ord c) e (ord (.skip ar))) (<= s e) (s (++ s))
                   (.put ar (chr s))))
          (.put ar c)))
    (.skip ar)
    (.init (.new Re.Elt) key (.token ar))))

(function re.parse-char (ar)
  (.init (.new Re.Elt) :char (.skip-escape ar)))

(function re.parse-quantifier (ar expr)
  (let (c (&next ar) n 1 m 1 greedy? true)
    (if (= c "*") (begin (.skip ar) (<- n 0 m nil))
        (= c "?") (begin (.skip ar) (<- n 0 m 1))
        (= c "+") (begin (.skip ar) (<- n 1 m nil))
        (= c "{") (begin
                    (.skip ar)
                    (<- n (.skip-uint ar))
                    (if (= (&next ar) "}") (<- m n)
                        (begin
                          (.skip ar ",")
                          (if (= (&next ar) "}") (<- m nil)
                              (<- m (.skip-uint ar)))))
                    (.skip ar "}")))
    (when (= (&next ar) "?")
      (.skip ar)
      (<- greedy? nil))
    (&greedy?! (&m! (&n! expr n) m) greedy?)))

(function re.parse (ar)
  (let (elements nil expr nil c nil)
    (while (&& (<- c (&next ar)) (! (strstr "|)" c)))
      (<- expr (if (= c ".") (re.parse-any ar)
                   (= c "(") (re.parse-group ar)
                   (= c "[") (re.parse-charset ar)
                   (re.parse-char ar)))
      (push! (re.parse-quantifier ar expr) elements))
    (reverse! elements)))

(method Re .test (elt i)
  (if (< i (.text-length self))
      (let (key (&key elt))
        (if (= key :char) (if (= (.text-at self i) (&val elt)) (++ i))
            (= key :alternate)
            (dolist (elements (&val elt))
              (if (.try self elements i) (return (&end self))))
            (= key :any) (++ i)
            (= key :char-class) (if (strstr (&val elt) (.text-at self i)) (++ i))
            (= key :exclude-char-class) (if (! (strstr (&val elt) (.text-at self i))) (++ i))
            (assert nil)))))

(method Re .try-n-times (elt i n)
  (dotimes (j n)
    (if (! (<- i (.test self elt i))) (return nil)))
  i)

(method Re .try (elements i)
  (if (nil? elements)
      (if (&anchored-end? (&end! self i)) (return (&& (= i (.text-length self))))
          (return true)))
  (let (elt (car elements) n (&n elt) m (|| (&m elt) (- (.text-length self) i)) next-i nil)
    (if (&greedy? elt)
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

(method Re .text-at (i)
  ([] (&text self) i))

(method Re .text-length ()
  (len (&text self)))

(method Re .subtext (start :opt end)
  (let (text (list... (slice (&text self) start (|| end (.text-length self)))))
    (if (nil? text) ""
        (apply memcat text))))

(method Re .match-string ()
  (.subtext self (&start self) (&end self)))

(method Re .match? (s :opt start)
  (&start! self (|| start (<- start 0)))
  (&text! self (array s))
  (if (&anchored-start? self)
      (return (&& (= start 0) (.try self (&elements self) 0))))
  (for (i start e (.text-length self)) (<= i e) (i (++ i))
    (&start! self i)
    (if (.try self (&elements self) i) (return true))))

(function re.compile (expr)
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
  (if (is-a? expr Re) expr
      (let (r (.new Re) s 0 e (len expr) anchored? nil)
        (when (prefix? expr "^")
          (&anchored-start?! r true)
          (<- s (++ s) anchored? true))
        (when (suffix? expr "$")
          (&anchored-end?! r true)
          (<- e (-- e) anchored? true))
        (if anchored? (<- expr (slice expr s e)))
        (with-memory-stream ($in expr)
          (&elements! r (re.parse (.new AheadReader)))))))

(function re.match (expr s :opt start)
  ; Returns Regular expression expr matched string.
  ; If the s does not match the expr, returns nil.
  ; If start is specified, match from the start th.
  (let (re (re.compile expr))
    (if (.match? re s start) (.match-string re))))

(function re.match-all (expr s :opt start)
  ; Same as re.match except that it returns a list of all matching strings.
  (let (rec (f (re start acc)
              (if (.match? re s start) (rec re (&end re) (cons (.match-string re) acc))
                  (reverse! acc))))
    (rec (re.compile expr) start nil)))

(function re.split (expr s :opt max-split)
  ; Returns list of strings delimited by the string that matches the regular expression.
  ; If max-split is specified, the maximum length of the list of return values is max-split.
  (let (split (f (re start max-split acc)
                (if (&& (!= max-split 0) (.match? re s start))
                    (begin
                      (push! (.subtext re start (&start re)) acc)
                      (split re (&end re) (&& (number? max-split) (-- max-split)) acc))
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
                        (.write-bytes mem (.subtext re start (&start re)))
                        (.write-bytes mem dest)
                        (replace re (&end re) (&& (number? max-replace) (-- max-replace)) mem))
                      (begin
                        (.write-bytes mem (.subtext re start))
                        (.to-s mem)))))
    (replace (re.compile expr) 0 max-replace (.new MemoryStream))))

(function! main (args)
  ;; anchore start
  (assert (.match? (re.compile "^") ""))
  (assert (nil? (.match? (re.compile "^a") "ba")))
  (assert (.match? (re.compile "a^") "a^"))
  ;; anchore end
  (assert (.match? (re.compile "$") ""))
  (assert (.match? (re.compile "$a") "$a"))
  (assert (nil? (.match? (re.compile "a$") "ab")))
  ;; char
  (let (re (re.compile "a"))
    (assert (.match? re "a"))
    (assert (= (&start re) 0))
    (assert (= (&end re) 1))
    (assert (.match? re "za"))
    (assert (= (&start re) 1))
    (assert (= (&end re) 2))
    (assert (! (.match? re ""))))
  ;; any
  (let (re (re.compile "a.c"))
    (assert (.match? re "abc"))
    (assert (= (&start re) 0))
    (assert (= (&end re) 3))
    (assert (! (.match? re "ac"))))
  ;; character class
  (let (re (re.compile "[a-c]"))
    (assert (! (.match? re "0")))
    (assert (.match? re "a"))
    (assert (.match? re "b"))
    (assert (.match? re "c")))
  (let (re (re.compile "[^a-c]"))
    (assert (.match? re "0"))
    (assert (! (.match? re "a")))
    (assert (! (.match? re "b")))
    (assert (! (.match? re "c"))))
  ;; alternate
  (let (re (re.compile "(ab|cd)"))
    (assert (.match? re "ab"))
    (assert (! (.match? re "bc")))
    (assert (.match? re "cd")))
  (let (re (re.compile "(ab*|cd*)"))
    (assert (.match? re "a"))
    (assert (= (&end re) 1))
    (assert (.match? re "abb"))
    (assert (= (&end re) 3))
    (assert (.match? re "c"))
    (assert (= (&end re) 1))
    (assert (.match? re "cdd"))
    (assert (= (&end re) 3))
    (assert (.match? re "abbcd"))
    (assert (= (.match-string re) "abb")))
  ;; quantifiers
  (let (re (re.compile "^a*$"))
    (assert (.match? re ""))
    (assert (.match? re "a"))
    (assert (.match? re "aa"))
    (assert (.match? re "aaa"))
    (assert (! (.match? re "aaab"))))
  (let (re (re.compile "a{2}"))
    (assert (! (.match? re "a")))
    (assert (.match? re "aa")))
  (let (re (re.compile "a{0,}"))
    (assert (.match? re ""))
    (assert (.match? re "a"))
    (assert (.match? re "aa")))
  ; api
  (= (re.match "o+" "fooo") "oo")
  (nil? (re.match "o{3}" "foo"))
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
