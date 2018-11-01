;; error and exception

(basic-throw :a)
(print 1)
(<- assert (lambda (x) (if (same? x nil) (basic-throw :AssertFailedException))))
(print 2)

(assert nil)
(print 3)

(basic-try ((catch (:E4 e)
              (print 8)))
  (basic-try ((catch (:E1 :E2 e)
                (print e)
                (print 1))
              (catch (:E3 e)
                (print e)
                (print 2))
              (finally
                (print :x)
                (print y)))
    (print :before-throw)
    (basic-throw :E2 'foo)
    (print :not-reach)))
