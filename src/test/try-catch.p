;; error and exception

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
