; throw/catch

(catch (lambda (e)
         (print (+ e 1)))
  (catch (lambda (e)
           (print e)
           (throw e))
    (print 1)
    (print 2)
    (print 3)
    (print 4)
    (throw 5)))
(print 7)
