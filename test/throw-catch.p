; throw/catch

(basic-catch (lambda (e)
               (assert (= e 3)))
             1
             2
             (throw 3))

(assert (=
          (basic-catch (lambda (e)
                         (assert nil))
                       (basic-catch (lambda (e)
                                      e)
                                    (throw 1)))
          1))

(basic-catch (lambda (e)
               (assert (= e 2)))
             (basic-catch (lambda (e)
                            (throw 2))
                          (throw 1)))
