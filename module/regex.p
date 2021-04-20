; regex module.

(class Regex ()
  elements
  text
  start end
  anchored-start?
  anchored-end?)

(class Regex.Elt ()
  key val n m greedy?)

(method Regex.Elt .init (key :opt val)
  (&key! self key)
  (&val! self val))

(function Regex.parse-any (ar)
  (.skip ar)
  (.init (.new Regex.Elt) :any))

(function Regex.parse-group (ar)
  (.skip ar)
  (let (val nil)
    (push! (Regex.parse ar) val)
    (while (= (&next ar) "|")
      (.skip ar)
      (push! (Regex.parse ar) val))
    (.skip ar ")")
    (.init (.new Regex.Elt) :alternate (reverse! val))))

(function Regex.parse-charset (ar)
  (.skip ar)
  (let (key (if (= (&next ar) "^") (begin (.skip ar) :exclude-char-class) :char-class))
    (while (! (= (&next ar) "]"))
      (<- c (.skip-escape ar))
      (if (= (&next ar) "-")
          (begin (.skip ar)
                 (for (s (str->code c) e (str->code (.skip ar))) (<= s e) (s (++ s))
                   (.put ar (code->str s))))
          (.put ar c)))
    (.skip ar)
    (.init (.new Regex.Elt) key (.token ar))))

(function Regex.parse-char (ar)
  (.init (.new Regex.Elt) :char (.skip-escape ar)))

(function Regex.parse-quantifier (ar expr)
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

(function Regex.parse (ar)
  (let (elements nil expr nil c nil)
    (while (&& (<- c (&next ar)) (! (strstr "|)" c)))
      (<- expr (if (= c ".") (Regex.parse-any ar)
                   (= c "(") (Regex.parse-group ar)
                   (= c "[") (Regex.parse-charset ar)
                   (Regex.parse-char ar)))
      (push! (Regex.parse-quantifier ar expr) elements))
    (reverse! elements)))

(method Regex .test (elt i)
  (if (< i (.text-length self))
      (switch (&key elt)
        :char (if (= (.text-at self i) (&val elt)) (++ i))
        :alternate (dolist (elements (&val elt))
                     (if (.try self elements i) (return (&end self))))
        :any (++ i)
        :char-class (if (strstr (&val elt) (.text-at self i)) (++ i))
        :exclude-char-class (if (! (strstr (&val elt) (.text-at self i))) (++ i)))))

(method Regex .try-n-times (elt i n)
  (dotimes (j n)
    (if (! (<- i (.test self elt i))) (return nil)))
  i)

(method Regex .try (elements i)
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

(method Regex .text-at (i)
  ([] (&text self) i))

(method Regex .text-length ()
  (arrlen (&text self)))

(method Regex .subtext (start :opt end)
  (let (text (arr->list (subarr (&text self) start (|| end (.text-length self)))))
    (if (nil? text) ""
        (apply memcat text))))

(method Regex .match-string ()
  (.subtext self (&start self) (&end self)))

(method Regex .match? (s :opt start)
  (&start! self (|| start (<- start 0)))
  (&text! self (if (array? s) s (str->arr s)))
  (if (&anchored-start? self)
      (return (&& (= start 0) (.try self (&elements self) 0))))
  (for (i start e (.text-length self)) (<= i e) (i (++ i))
    (&start! self i)
    (if (.try self (&elements self) i) (return true))))

(function Regex.compile (expr)
  ; Returns the instance on Regex corresponds to the specified regular expression expr.
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
  (if (is-a? expr Regex) expr
      (let (r (.new Regex) s 0 e (memlen expr) anchored? nil)
        (when (= ([] expr 0) 0x5e)
          (&anchored-start?! r true)
          (<- s (++ s) anchored? true))
        (when (= ([] expr (-- e)) 0x24)
          (&anchored-end?! r true)
          (<- e (-- e) anchored? true))
        (if anchored? (<- expr (submem expr s e)))
        (with-memory-stream ($in expr)
          (&elements! r (Regex.parse (.new AheadReader)))))))

(function regex.match (expr s :opt start)
  ; Returns Regular expression expr matched string.
  ; If the s does not match the expr, returns nil.
  ; If start is specified, match from the start th.
  (let (re (Regex.compile expr))
    (if (.match? re s start) (.match-string re))))

(function regex.match-all (expr s :opt start)
  ; Same as regex.match except that it returns a list of all matching strings.
  (let (rec (f (re start acc)
              (if (.match? re s start) (rec re (&end re) (cons (.match-string re) acc))
                  (reverse! acc))))
    (rec (Regex.compile expr) start nil)))

(function regex.split (expr s :opt max-split)
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
    (split (Regex.compile expr) 0 max-split nil)))

(function regex.replace (expr src dest :opt max-replace)
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
    (replace (Regex.compile expr) 0 max-replace (.new MemoryStream))))

(function! main (args)
  ;; anchore start
  (assert (.match? (Regex.compile "^") ""))
  (assert (nil? (.match? (Regex.compile "^a") "ba")))
  (assert (.match? (Regex.compile "a^") "a^"))
  ;; anchore end
  (assert (.match? (Regex.compile "$") ""))
  (assert (.match? (Regex.compile "$a") "$a"))
  (assert (nil? (.match? (Regex.compile "a$") "ab")))
  ;; char
  (let (re (Regex.compile "a"))
    (assert (.match? re "a"))
    (assert (= (&start re) 0))
    (assert (= (&end re) 1))
    (assert (.match? re "za"))
    (assert (= (&start re) 1))
    (assert (= (&end re) 2))
    (assert (! (.match? re ""))))
  ;; any
  (let (re (Regex.compile "a.c"))
    (assert (.match? re "abc"))
    (assert (= (&start re) 0))
    (assert (= (&end re) 3))
    (assert (! (.match? re "ac"))))
  ;; character class
  (let (re (Regex.compile "[a-c]"))
    (assert (! (.match? re "0")))
    (assert (.match? re "a"))
    (assert (.match? re "b"))
    (assert (.match? re "c")))
  (let (re (Regex.compile "[^a-c]"))
    (assert (.match? re "0"))
    (assert (! (.match? re "a")))
    (assert (! (.match? re "b")))
    (assert (! (.match? re "c"))))
  ;; alternate
  (let (re (Regex.compile "(ab|cd)"))
    (assert (.match? re "ab"))
    (assert (! (.match? re "bc")))
    (assert (.match? re "cd")))
  (let (re (Regex.compile "(ab*|cd*)"))
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
  (let (re (Regex.compile "^a*$"))
    (assert (.match? re ""))
    (assert (.match? re "a"))
    (assert (.match? re "aa"))
    (assert (.match? re "aaa"))
    (assert (! (.match? re "aaab"))))
  (let (re (Regex.compile "a{2}"))
    (assert (! (.match? re "a")))
    (assert (.match? re "aa")))
  (let (re (Regex.compile "a{0,}"))
    (assert (.match? re ""))
    (assert (.match? re "a"))
    (assert (.match? re "aa")))
  ; api
  (= (regex.match "o+" "fooo") "oo")
  (nil? (regex.match "o{3}" "foo"))
  (let ((:opt foo bar buzz) (regex.match-all "[fb].*?," "foo,bar,buzz"))
    (assert (= foo "foo,"))
    (assert (= bar "bar,"))
    (assert (nil? buzz)))
  (let (re (Regex.compile "[,:]"))
    (let ((:opt foo bar buzz) (regex.split re "foo,bar,buzz"))
      (assert (= foo "foo"))
      (assert (= bar "bar"))
      (assert (= buzz "buzz")))
    (let ((foo bar-buzz) (regex.split re "foo:bar:buzz" 1))
      (assert (= foo "foo"))
      (assert (= bar-buzz "bar:buzz"))))
  (let (re (Regex.compile "[,:]"))
    (assert (= (regex.replace re "foo,bar,buzz" "|||") "foo|||bar|||buzz"))
    (assert (= (regex.replace re "foo:bar:buzz" "|||" 1) "foo|||bar:buzz"))))
