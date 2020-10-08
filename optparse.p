; option parser module.

(class OptionParser ()
  table)

(method OptionParser .init (option)
  ; Specifies a character string listing option characters to be interpreted as optionsArg.
  ; Options with ':' immediately after the specified character have option arguments.
  ; Returns this object.
  (<- option (str->arr option))
  (let (table nil i 0 len (arrlen option))
    (while (< i len)
      (let (opt ([] option i) optarg? (&& (< (<- i (++ i)) len) (string= ([] option i) ":")))
        (if optarg? (<- i (++ i))) 
        (push! table (list opt optarg? nil))))
    (&table<- self table)))

(method OptionParser .lookup (opt)
  (let (record (find-if (f (record) (string= opt (car record))) (&table self)))
    (if (nil? record) (.raise self "unknown option " opt)
        record)))

(method OptionParser .parse (args)
  ; Parse command line arguments.
  ; Returns list. -- (self parsed-args)
  (while args
    (let (arg (car args))
      (if (string= arg "--") (return (list self (cdr args))))
      (let (argarr (str->arr arg) arglen (arrlen argarr))
        (if (string/= ([] argarr 0) "-") (break))    ; end of option.
        (for (i 1) (< i arglen) (<- i (++ i))
          (let (record (.lookup self ([] argarr i))
                       (opt optarg? optval) record
                       put (f (record val) (car! (cddr record) val)))
            (if optval (.raise self "duplicate option " opt)
                (nil? optarg?) (begin (put record true) (continue))
                (< (++ i) arglen) (begin (put record (string-slice arg (++ i))) (break))
                (<- args (cdr args)) (begin (put record (car args)) (break))
                (.raise self "required option argument of " opt))))
        (<- args (cdr args)))))
  (list self args))

(method OptionParser .get (opt)
  ; Returns option value specified opt.
  (caddr (.lookup self opt)))

(function! main (args)
  (let (option "abc:d:")
    (let ((op args) (.parse (.init (.new OptionParser) option) '("-a" "-c" "carg" "arg")))
      (assert (string= (car args) "arg"))
      (assert (.get op "a"))
      (assert (nil? (.get op "b")))
      (assert (string= (.get op "c") "carg")))
    (let ((op args) (.parse (.init (.new OptionParser) option) '("-accarg")))
      (assert (nil? args))
      (assert (.get op "a"))
      (assert (nil? (.get op "b")))
      (assert (string= (.get op "c") "carg")))
    (let ((op args) (.parse (.init (.new OptionParser) option) '("-a" "--" "-b")))
      (assert (string= (car args) "-b"))
      (assert (.get op "a"))
      (assert (nil? (.get op "b")))
      (assert (nil? (.get op "c"))))))
