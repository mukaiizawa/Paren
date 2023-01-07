; calculator.

(function parse-factor (rd)
  (if (!= (.next (.skip-space rd)) "(") (.skip-number rd)
      (begin
        (.skip rd)
        (begin0
          (parse-expr rd)
          (.skip rd ")")))))

(function parse-term (rd)
  (let (tree (parse-factor rd))
    (while (in? (.next (.skip-space rd)) '("*" "/"))
      (<- tree (list (symbol (.skip rd)) tree (parse-factor rd))))
    tree))

(function parse-expr (rd)
  (let (tree (parse-term rd))
    (while (in? (.next (.skip-space rd)) '("+" "-"))
      (<- tree (list (symbol (.skip rd)) tree (parse-term rd))))
    tree))

(function calc (expr)
  (catch
    (with-memory-stream ($in expr)
      (let (rd (.new AheadReader))
        (eval (parse-expr rd))))
    (f (e)
      (if (is-a? e SystemExit) (throw e)
          'NaN))))

(function! main (args)
  (if (nil? args) (loop (write (calc (read-line))))
      (write (calc (join args)))))
