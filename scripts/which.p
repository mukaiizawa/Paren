; which.

(import :optparse)

(function get-path ()
  (keep (f (x) (if (! (empty? x)) (path x)))
        (split (getenv "PATH") (if (== $hostname :windows) ";" ":"))))

(function which (cmd :opt all? strict?)
  (let (name (if strict? .name .base-name))
    (dolist (dir (select .readable? (get-path)))
      (dolist (file (.children dir))
        (when (&& (! (.dir? file)) (= (name file) cmd))
          (write-line (.to-s file))
          (if (nil? all?) (return true)))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "as") args))
    (if (nil? args) (raise ArgumentError "missing argument")
        (foreach (f (x) (which x (.get op "a") (.get op "s"))) args))))
