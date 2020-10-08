; regex module.

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
    (while (memeq? (&next ar) "|")
      (.skip ar)
      (push! val (Regex.parse ar)))
    (.skip ar ")")
    (.init (.new Regex.Elt) :alternate (reverse! val))))

(function Regex.parse-charset (ar)
  (.skip ar)
  (let (key (if (memeq? (&next ar) "^") (begin (.skip ar) :exclude-char-class) :char-class))
    (while (! (memeq? (&next ar) "]"))
      (<- c (.skip-escape ar))
      (if (memeq? (&next ar) "-")
          (begin (.skip ar)
                 (for (s (str->code c) e (str->code (.skip ar))) (<= s e) (<- s (++ s))
                   (.put ar (code->str s))))
          (.put ar c)))
    (.skip ar)
    (.init (.new Regex.Elt) key (.token ar))))

(function Regex.parse-char (ar)
  (.init (.new Regex.Elt) :char (.skip-escape ar)))

(function Regex.parse-quantifier (ar expr)
  (let (c (&next ar) n 1 m 1 greedy? true)
    (if (memeq? c "*") (begin (.skip ar) (<- n 0 m nil))
        (memeq? c "?") (begin (.skip ar) (<- n 0 m 1))
        (memeq? c "+") (begin (.skip ar) (<- n 1 m nil))
        (memeq? c "{") (begin (.skip ar)
                               (<- n (.skip-uint ar))
                               (if (memeq? (&next ar) "}") (<- m n)
                                   (begin (.skip ar ",")
                                          (if (memeq? (&next ar) "}") (<- m nil)
                                              (<- m (.skip-uint ar)))))
                               (.skip ar "}")))
    (when (memeq? (&next ar) "?")
      (.skip ar)
      (<- greedy? nil))
    (&greedy?<- (&m<- (&n<- expr n) m) greedy?)))

(function Regex.parse (ar)
  (let (elements nil expr nil c nil)
    (while (&& (<- c (&next ar)) (! (strstr "|)" c)))
      (<- expr (if (memeq? c ".") (Regex.parse-any ar)
                   (memeq? c "(") (Regex.parse-group ar)
                   (memeq? c "[") (Regex.parse-charset ar)
                   (Regex.parse-char ar)))
      (push! elements (Regex.parse-quantifier ar expr)))
    (reverse! elements)))

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
  (let (r (.new Regex) s 0 e (memlen expr) anchored? nil)
    (when (= ([] expr 0) 0x5e)
      (&anchored-start?<- r true)
      (<- s (++ s) anchored? true))
    (when (= ([] expr (-- e)) 0x24)
      (&anchored-end?<- r true)
      (<- e (-- e) anchored? true))
    (if anchored? (<- expr (submem expr s e)))
    (with-memory-stream ($in expr)
      (&elements<- r (Regex.parse (.new AheadReader))))))

(method Regex .test (elt i)
  (if (< i (.text-length self))
      (switch (&key elt)
        :char (if (memeq? (.text-at self i) (&val elt)) (++ i))
        :alternate (dolist (elements (&val elt))
                     (if (.try self elements i) (return (&end self))))
        :any (++ i)
        :char-class (if (strstr (&val elt) (.text-at self i)) (++ i))
        :exclude-char-class (if (! (strstr (&val elt) (.text-at self i))) (++ i)))))

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
  (arrlen (&text self)))

(method Regex .match-start ()
  ; Returns the matched start position.
  ; The previous .match must return true when calling.
  (&start self))

(method Regex .match-end ()
  ; Returns the matched end position.
  ; The previous .match must return true when calling.
  (&end self))

(method Regex .match-string ()
  ; Returns whether the string s matched this instance.
  (with-memory-stream (out)
    (dotimes (i (.text-length self))
      (if (< i (&start self)) (continue)
          (< i (&end self)) (.write-mem out ([] (&text self) i))))))

(method Regex .match? (s :opt start)
  ; Returns whether the string s matched this instance.
  (&start<- self (|| start (<- start 0)))
  (&text<- self (str->arr s))
  (if (&anchored-start? self)
      (return (&& (= start 0) (.try self (&elements self) 0))))
  (for (i start e (.text-length self)) (<= i e) (&start<- self (<- i (++ i)))
    (if (.try self (&elements self) i) (return true))))

(function! main (args)
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
            (&& (.match? re "a") (= (.match-start re) 0) (= (.match-end re) 1)
                (.match? re "za") (= (.match-start re) 1) (= (.match-end re) 2)
                (! (.match? re "")))))
  ; any
  (assert (let (re (Regex.compile "a.c"))
            (&& (.match? re "abc") (= (.match-start re) 0) (= (.match-end re) 3)
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
            (&& (.match? re "a") (= (.match-end re) 1)
                (.match? re "abb") (= (.match-end re) 3)
                (.match? re "c") (= (.match-end re) 1)
                (.match? re "cdd") (= (.match-end re) 3))))
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
