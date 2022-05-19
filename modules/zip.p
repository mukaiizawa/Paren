; zip module.

(import :datetime)
(import :endian)

; [archive decryption header]
; [archive extra data record]
; [central directory header 1]
; ...
; [central directory header n]
; [zip64 end of central directory record]
; [zip64 end of central directory locator]
; [end of central directory record]

(<- $zip.local-file-header-signature 0x04034b50)

(class Zip ()
  buf pos entries)

(method Zip .init ()
  (&pos! self 0))

(method Zip .entries ()
  (&entries self))

(method Zip .read (file)
  (with-open ($in file :read)
    (&buf! self (read-bytes)))
  (&entries! self (collect (f () (if (= (.peek-u32 self) $zip.local-file-header-signature) (.read-entry self))))))

(method Zip .skip (n)
  (&pos! self (+ (&pos self) n)))

(method Zip .peek-u32 ()
  (endian.ui32LE (&buf self) (&pos self)))

(method Zip .u16 ()
  (begin0
    (endian.ui16LE (&buf self) (&pos self))
    (.skip self 2)))

(method Zip .u32 ()
  (begin0
    (.peek-u32 self)
    (.skip self 4)))

(method Zip .bytes (len)
  (begin0
    (bytes (&buf self) (&pos self) (+ (&pos self) len))
    (.skip self len)))

(method Zip .read-entry ()
  (let (entry (.new ZipEntry))
    (&local-file-header-signature! entry (.u32 self))
    (&version-needed-to-extract! entry (.u16 self))
    (&general-purpose-bit-flag! entry (.u16 self))
    (&compression-method! entry (.u16 self))
    (&last-mod-file-time! entry (.u16 self))
    (&last-mod-file-date! entry (.u16 self))
    (&crc-32! entry (.u32 self))
    (&compressed-size! entry (.u32 self))
    (&uncompressed-size! entry (.u32 self))
    (&file-name-length! entry (.u16 self))
    (&extra-field-length! entry (.u16 self))
    (&file-name! entry (.bytes self (&file-name-length entry)))
    (&extra-field! entry (.bytes self (&extra-field-length entry)))))

(class ZipEntry ()
  local-file-header-signature
  version-needed-to-extract
  general-purpose-bit-flag
  compression-method
  last-mod-file-time
  last-mod-file-date
  crc-32
  compressed-size
  uncompressed-size
  file-name-length
  extra-field-length
  file-name
  extra-field)

(method ZipEntry .timestamp ()
  (datetime.parse-msdos-datetime (&last-mod-file-date self) (&last-mod-file-time self)))

(method ZipEntry .file-name ()
  (string (&file-name self)))

(method ZipEntry .contents ()
  (string (&extra-field self)))

(function zip.entries (file)
  (.entries (.read (.new Zip) file)))

(function zip.entry-names (file)
  (map .file-name (zip.entries file)))

(function zip.compress ()
  nil)

(function zip.uncompress (file)
  (with-open ($in file :read)
    (let (rd (.new ZIPReader))
      )))

(function! main (args)
  nil)
