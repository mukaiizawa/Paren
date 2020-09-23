; option parser module

(class OptionParser ()
  table)

(method OptionParser .init (option)
  ; Specifies a character string listing option characters to be interpreted as optionsArg.
  ; Options with ':' immediately after the specified character have option arguments.
  ; Returns this object.
  (<- option (string->array option))
  (let (table nil i 0 len (array-length option))
    (while (< i len)
      (let (opt ([] option i) optarg? (&& (< (<- i (++ i)) len) (string= ([] option i) ":")))
        (if optarg? (<- i (++ i))) 
        (push! table (list opt optarg? nil))))
    (&table<- self table)))

(method OptionParser .find-record (opt)
  (let (record (find-if (lambda (record)
                          (string= opt (car record)))
                        (&table self)))
    (if (nil? record) (.raise self "unknown option " opt)
        record)))

(method OptionParser .parse (args)
  ; Parse command line arguments.
  ; Returns list. -- (self parsed-args)
  (while args
    (let (arg (car args))
      (if (string= arg "--") (return (list self (cdr args))))
      (let (argarr (string->array arg) arglen (array-length argarr))
        (if (string/= ([] argarr 0) "-") (break))
        (for (i 1) (< i arglen) (<- i (++ i))
          (let (opt ([] argarr i) record (.find-record self opt))
            (if (caddr record) (.raise self "duplicate option " opt)
                (nil? (cadr record)) (car! (cddr record) true)
                (< (++ i) arglen) (begin
                                    (car! (cddr record) (string-slice arg (++ i)))
                                    (break))
                (nil? (<- args (cdr args))) (.raise self "required option argument of " opt)
                (begin (car! (cddr record) (car args))
                       (break)))))
        (<- args (cdr args)))))
  (list self args))

(method OptionParser .get (opt)
  ; Returns option value specified opt.
  (caddr (.find-record self opt)))

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
