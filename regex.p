; regex module.

; # regular expression
;
;     ^ -- start of string
;     $ -- end of string
;     . -- any single character
;     [...] -- any character in set
;     [^...]  -- not any character in the set
;     (x | y ...)   match any expression, y, y... in order
;
; # quantifiers
;
;     greedy  lazy
;     ----------------------------------------------------------------------------------
;     *       *?       match zero or more times.
;     ?       ??       match one or more times.
;     +       +?       match zero or one time.
;     {n}     {n}?     match exactly n times.
;     {n,}    {n,}?    match at least n times.
;     {n,m}   {n,m}?   match from n to m times.

(class Regex ()
  text
  elements
  start
  end
  anchored-start?
  anchored-end?)

(class Regex.Elt ()
  key val n m greedy?)

(method Regex.Elt .init (key :opt val)
  (&key<- self key)
  (&val<- self val))

(function Regex.parse-any (ar)
  (.skip ar)
  (.init (.new Regex.Elt) :any))

(function Regex.parse-group (ar)
  (.skip ar)
  (let (val nil)
    (push! val (Regex.parse ar))
    (while (string= (&next ar) "|")
      (.skip ar)
      (push! val (Regex.parse ar)))
    (.ensured-skip ar ")")
    (.init (.new Regex.Elt) :alternate (reverse! val))))

(function Regex.parse-charset (ar)
  (.skip ar)
  (.reset ar)
  (let (key (if (string= (&next ar) "^") (begin (.skip ar) :exclude-char-class) :char-class))
    (while (! (string= (&next ar) "]"))
      (<- c (.skip-escape ar))
      (if (string= (&next ar) "-")
          (begin (.skip ar)
                 (for (s (string->code c) e (string->code (.skip ar))) (<= s e) (<- s (++ s))
                   (.put ar (code->string s))))
          (.put ar c)))
    (.skip ar)
    (.init (.new Regex.Elt) key (.token ar))))

(function Regex.parse-char (ar)
  (.init (.new Regex.Elt) :char (.skip-escape ar)))

(function Regex.parse-quantifier (ar expr)
  (let (c (&next ar) n 1 m 1 greedy? true)
    (if (string= c "*") (begin (.skip ar) (<- n 0 m nil))
        (string= c "?") (begin (.skip ar) (<- n 0 m 1))
        (string= c "+") (begin (.skip ar) (<- n 1 m nil))
        (string= c "{") (begin (.skip ar)
                                  (<- n (.skip-unsigned-integer ar))
                                  (if (string= (&next ar) "}") (<- m n)
                                      (begin (.ensured-skip ar ",")
                                             (if (string= (&next ar) "}") (<- m nil)
                                                 (<- m (.skip-unsigned-integer ar)))))
                                  (.ensured-skip ar "}")))
    (when (string= (&next ar) "?")
      (.skip ar)
      (<- greedy? nil))
    (&greedy?<- (&m<- (&n<- expr n) m) greedy?)))

(function Regex.parse (ar)
  (let (elements nil expr nil c nil)
    (while (&& (<- c (&next ar)) (! (string-index "|)" c)))
      (<- expr (if (string= c ".") (Regex.parse-any ar)
                   (string= c "(") (Regex.parse-group ar)
                   (string= c "[") (Regex.parse-charset ar)
                   (Regex.parse-char ar)))
      (push! elements (Regex.parse-quantifier ar expr)))
    (reverse! elements)))

(function Regex.compile (expr)
  (let (r (.new Regex) s 0 e (bytes-length expr) anchored? nil)
    (when (= ([] expr 0) 0x5e)
      (&anchored-start?<- r true)
      (<- s (++ s) anchored? true))
    (when (= ([] expr (-- e)) 0x24)
      (&anchored-end?<- r true)
      (<- e (-- e) anchored? true))
    (if anchored? (<- expr (bytes->string! (bytes-slice expr s e))))
    (&elements<- r (Regex.parse (.init (.new AheadReader) expr)))))

(method Regex .test (elt i)
  (if (< i (.text-length self))
      (switch (&key elt)
        :char (if (string= (.text-at self i) (&val elt)) (++ i))
        :alternate (dolist (elements (&val elt))
                     (if (.try self elements i) (return (&end self))))
        :any (++ i)
        :char-class (if (string-index (&val elt) (.text-at self i)) (++ i))
        :exclude-char-class (if (! (string-index (&val elt) (.text-at self i))) (++ i)))))

(method Regex .try-n-times (elt i n)
  (dotimes (j n)
    (if (nil? (<- i (.test self elt i))) (return nil)))
  i)

(method Regex .try (elements i)
  (if (nil? elements)
      (if (&anchored-end? (&end<- self i)) (return (&& (= i (.text-length self))))
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
  (array-length (&text self)))

(method Regex .match-start ()
  (&start self))

(method Regex .match-end ()
  (&end self))

(method Regex .match? (s :opt start)
  (&start<- self (|| start (<- start 0)))
  (&text<- self (string->array s))
  (if (&anchored-start? self)
      (return (&& (= start 0) (.try self (&elements self) 0))))
  (for (i start e (.text-length self)) (<= i e) (&start<- self (<- i (++ i)))
    (if (.try self (&elements self) i) (return true))))

(function! main ()
  ; anchore start
  (assert (.match? (Regex.compile "^") ""))
  (assert (nil? (.match? (Regex.compile "^a") "ba")))
  (assert (.match? (Regex.compile "a^") "a^"))
  ; anchore end
  (assert (.match? (Regex.compile "$") ""))
  (assert (.match? (Regex.compile "$a") "$a"))
  (assert (nil? (.match? (Regex.compile "a$") "ab")))
  ; char
  (assert (let (re (Regex.compile "a"))
            (&& (.match? re "a") (= (&start re) 0) (= (&end re) 1)
                (.match? re "za") (= (&start re) 1) (= (&end re) 2)
                (! (.match? re "")))))
  ; any
  (assert (let (re (Regex.compile "a.c"))
            (&& (.match? re "abc") (= (&start re) 0) (= (&end re) 3)
                (! (.match? re "ac")))))
  ; character class
  (assert (let (re (Regex.compile "[a-c]"))
            (&& (.match? re "a")
                (.match? re "b")
                (.match? re "c"))))
  (assert (let (re (Regex.compile "[^a-c]"))
            (&& (! (.match? re "a"))
                (! (.match? re "b"))
                (! (.match? re "c")))))
  ; alternate
  (assert (let (re (Regex.compile "(ab|cd)"))
            (&& (.match? re "ab")
                (! (.match? re "bc"))
                (.match? re "cd"))))
  (assert (let (re (Regex.compile "(ab*|cd*)"))
            (&& (.match? re "a") (= (&end re) 1)
                (.match? re "abb") (= (&end re) 3)
                (.match? re "c") (= (&end re) 1)
                (.match? re "cdd") (= (&end re) 3))))
  ; quantifiers
  (assert (let (re (Regex.compile "^a*$"))
            (&& (.match? re "")
                (.match? re "a")
                (.match? re "aa")
                (.match? re "aaa")
                (! (.match? re "aaab")))))
  (assert (let (re (Regex.compile "a{2}"))
            (&& (! (.match? re "a"))
                (.match? re "aa"))))
  (assert (let (re (Regex.compile "a{0,}"))
            (&& (.match? re "")
                (.match? re "a")
                (.match? re "aa")))))
