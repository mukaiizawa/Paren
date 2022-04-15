; remove files.

(function rm (file)
  (catch (Error identity)
    (if (.dir? file) (foreach rm (.children file)))
    (.remove file)))

(function! main (args)
  (foreach rm (map path args)))
