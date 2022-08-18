; remove files.

(function rm (file)
  (catch
    (begin
      (if (.dir? file) (foreach rm (.children file)))
      (.remove file))
    (f (e)
      (if (is-a? e OSError) nil
          (throw e)))))

(function! main (args)
  (foreach rm (map path args)))
