; display summary of paren files.

(import :optparse)

(<- $all? nil)

(function peek (fn)
  (with-open ($in fn :read)
    (return (read-line))))

(function display (fn)
  (catch (Error (f (e) :skip))
    (let (line (peek fn) (left right) (if (nil? $all?) (list (.base-name fn) (slice line 2))
                                          (list (.name fn) line)))
      (write-line (format "%s -- %s" left right)))))

(function target? (fn)
  (&& (.file? fn) (|| $all? (= (.suffix fn) "p"))))

(function! main (args)
  ; paren whatis.p [OPTION]
  ; Read and display one line of Paren script file.
  ;     -a target all files
  (let ((op args) (.parse (.init (.new OptionParser) "a") args))
    (<- $all? (.get op "a"))
    (foreach display (select target? (.children (path.getcwd))))))
