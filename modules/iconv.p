; iconv module.

(<- $iconv.encodings '(:sjis :utf8))

(function! startup ()
  (if (! (in? $hostname '(:windows))) (raise StateError "Unsopport OS")))

(built-in-function iconv.encode (string enc0 :opt enc1))
