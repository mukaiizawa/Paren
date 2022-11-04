; iconv module.

(<- $iconv.encodings '(:sjis :utf8))

(function! startup ()
  (if (! (in? $hostname '(:windows))) (raise StateError "Unsopport OS")))

(built-in-function iconv.encode (string enc0 :opt enc1))

(function! main (args)
  (let (mbstr "あ い う" mbstr-sjis-bytes #< 0x82 0xa0 0x20 0x82 0xa2 0x20 0x82 0xa4 >)
    (assert (= (iconv.encode mbstr :sjis) (string mbstr-sjis-bytes)))
    (assert (= (iconv.encode mbstr :utf8 :sjis) (string mbstr-sjis-bytes)))
    (assert (= (iconv.encode (string mbstr-sjis-bytes) :sjis :utf8) mbstr))))
