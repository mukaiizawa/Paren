; yes.

(function main (args)
  (catch (OSError (f (x) nil))
    (let (y (|| (car args) "y"))
      (loop (write-line y)))))
