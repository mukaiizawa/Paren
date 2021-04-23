; option parser module.

(class OptionParser ()
  table)

(method OptionParser .init (option)
  ; Specifies a character string listing option characters to be interpreted as optionsArg.
  ; Options with ':' immediately after the specified character have option arguments.
  ; Returns this object.
  (<- option (str->arr option))
  (let (table nil i 0 len (len option))
    (while (< i len)
      (let (opt ([] option i) optarg? (&& (< (<- i (++ i)) len) (= ([] option i) ":")))
        (if optarg? (<- i (++ i))) 
        (push! (list opt optarg? nil) table)))
    (&table! self table)))

(method OptionParser .lookup (opt)
  (let (record (find (f (x) (if (= opt (car x)) x))
                     (&table self)))
    (if (nil? record) (.raise self "unknown option " opt)
        record)))

(method OptionParser .parse (args)
  ; Parse command line arguments.
  ; Returns list. -- (self parsed-args)
  (while args
    (let (arg (car args))
      (if (= arg "--") (return (list self (cdr args))))
      (let (argarr (str->arr arg) arglen (len argarr))
        (if (!= ([] argarr 0) "-") (break))    ; end of option.
        (for (i 1) (< i arglen) (i (++ i))
          (let (record (.lookup self ([] argarr i))
                       (opt optarg? optval) record
                       put (f (record val) (car! (cddr record) val)))
            (if optval (.raise self "duplicate option " opt)
                (nil? optarg?) (begin (put record true) (continue))
                (< (++ i) arglen) (begin (put record (substr arg (++ i))) (break))
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
