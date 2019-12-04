; throw/catch

(basic-catch (lambda (e)
               (assert (= e 3)))
  1
  2
  (basic-throw 3))

(assert (=
          (basic-catch (lambda (e)
                         (assert nil))
            (basic-catch (lambda (e)
                           e)
              (basic-throw 1)))
          1))

(basic-catch (lambda (e)
               (assert (= e 2)))
  (basic-catch (lambda (e)
                 (basic-throw 2))
    (basic-throw 1)))
