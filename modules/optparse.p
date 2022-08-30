; option parser module.

(class OptionParser ()
  table)

(method OptionParser .init (option)
  ; Specifies a character string listing option characters to be interpreted as optionsArg.
  ; Options with ':' immediately after the specified character have option arguments.
  ; Returns this object.
  (let (i 0 table nil len (len (<- option (array option))))
    (while (< i len)
      (let (opt ([] option i) optarg? (&& (< (<- i (++ i)) len) (= ([] option i) ":")))
        (if optarg? (<- i (++ i)))
        (push! (list opt optarg? nil) table)))
    (<- self->table table)
    self))

(method OptionParser .lookup (opt)
  (let (record (select1 (f (x) (= opt (car x))) self->table))
    (if (nil? record) (raise ArgumentError (format "unknown option `%s`, available options are %v" opt (sort! (map car self->table) :key lower)))
        record)))

(method OptionParser .parse (args)
  ; Parse command line arguments.
  ; Returns list. -- (self parsed-args)
  (while args
    (let (arg (car args))
      (if (= arg "--") (return (list self (cdr args)))
          (!= ([] arg 0) "-") (break)    ; end of option.
          (for (i 1 end (len arg)) (< i end) (i (++ i))
            (let (record (.lookup self ([] arg i))
                         (opt optarg? optval) record
                         put (f (record val) (car! (cddr record) val)))
              (if optval (raise SyntaxError (str "duplicate option " opt))
                  (nil? optarg?) (begin (put record true) (continue))
                  (< (++ i) end) (begin (put record (slice arg (++ i))) (break))
                  (<- args (cdr args)) (begin (put record (car args)) (break))
                  (raise SyntaxError (str "required option argument of " opt))))))
      (<- args (cdr args))))
  (list self args))

(method OptionParser .get (opt :opt default-value)
  ; Returns option value specified opt.
  (|| (caddr (.lookup self opt)) default-value))

(method OptionParser .get-float (opt :opt default-value)
  ; Returns option float value.
  (let (val (.get self opt))
    (if (nil? val) default-value
        (float val))))

(method OptionParser .get-int (opt :opt default-value)
  ; Returns option integer value.
  (let (val (.get self opt))
    (if (nil? val) default-value
        (int val))))

(method OptionParser .get-ints (opt :opt default-value)
  ; Returns list of positive integer.
  ;; "1-5,7,9" -> '(1 2 3 4 5 7 9)
  (let (val (.get self opt))
    (if (nil? val) default-value
        (flatten (map (f (x)
                        (if (! (index "-" x)) (int x)
                            (let ((s e) (split x "-"))
                              (.. (int s) (++ (int e))))))
                      (split val ","))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "b:c:d:r:s:") '("-b30" "-r" "1,2-4,8" "-d249")))
    (assert (= (.get-int op "b" 329) 30))
    (assert (= (.get-int op "c" 329) 329))
    (assert (= (.get-int op "d") 249))
    (assert (= (.get-ints op "r") '(1 2 3 4 8)))
    (assert (= (.get-ints op "s" '(3)) '(3))))
  (let (option "abc:d:")
    (let ((op args) (.parse (.init (.new OptionParser) option) '("-a" "-c" "carg" "arg")))
      (assert (= (car args) "arg"))
      (assert (.get op "a"))
      (assert (nil? (.get op "b")))
      (assert (= (.get op "c") "carg")))
    (let ((op args) (.parse (.init (.new OptionParser) option) '("-accarg")))
      (assert (nil? args))
      (assert (.get op "a"))
      (assert (nil? (.get op "b")))
      (assert (= (.get op "c") "carg")))
    (let ((op args) (.parse (.init (.new OptionParser) option) '("-a" "--" "-b")))
      (assert (= (car args) "-b"))
      (assert (.get op "a"))
      (assert (nil? (.get op "b")))
      (assert (nil? (.get op "c"))))))
