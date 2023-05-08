; clip module.

(function! startup ()
  (if (! (in? $hostname '(:windows))) (raise StateError "Unsupported OS")
      (! (bound? 'clip.copy)) (raise StateError "Requires clip option at compile time")))

(built-in-function clip.copy (x))
(built-in-function clip.paste ())
