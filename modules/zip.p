; zip module.

(import :datetime)
(import :endian)

(<- $zip.local-file-header-signature                0x04034b50
    $zip.data-descriptor-signature                  0x08074b50
    $zip.archive-extra-data-signature               0x08064b50
    $zip.central-file-header-signature              0x02014b50
    $zip.digital-header-signature                   0x05054b50
    $zip.zip64-end-of-central-dir-signature         0x06064b50
    $zip.zip64-end-of-central-dir-locator-signature 0x07064b50
    $zip.end-of-central-dir-signature               0x06054b50
    $zip.headers (list $zip.local-file-header-signature
                       $zip.archive-extra-data-signature
                       $zip.central-file-header-signature
                       $zip.digital-header-signature
                       $zip.zip64-end-of-central-dir-signature
                       $zip.zip64-end-of-central-dir-locator-signature
                       $zip.end-of-central-dir-signature))

(class ZipError (Error))

;; Entry

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

(method ZipEntry .uncompressed-size ()
  (&uncompressed-size self))

(method ZipEntry .compressed-size ()
  (&compressed-size self))

(method ZipEntry .timestamp ()
  (datetime.parse-msdos-datetime (&last-mod-file-date self) (&last-mod-file-time self)))

(method ZipEntry .file-name ()
  (string (&file-name self)))

(method ZipEntry .contents ()
  (string (&extra-field self)))

;; Reader

(class ZipReader ()
  buf pos)

(method ZipReader .init ()
  (&pos! self 0)
  (&buf! self (read-bytes)))

(method ZipReader .skip (n)
  (&pos! self (+ (&pos self) n)))

(method ZipReader .skip-data-descriptor (n)
  (while (! (in? (.peek-u32 self) $zip.headers))
    (.skip 4))
  self)    ; 4 or 8

(method ZipReader .peek-u32 ()
  (endian.ui32LE (&buf self) (&pos self)))

(method ZipReader .u16 ()
  (begin0
    (endian.ui16LE (&buf self) (&pos self))
    (.skip self 2)))

(method ZipReader .u32 ()
  (begin0
    (.peek-u32 self)
    (.skip self 4)))

(method ZipReader .bytes (len)
  (begin0
    (bytes (&buf self) (&pos self) (+ (&pos self) len))
    (.skip self len)))

(method ZipReader .read1 ()
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
    (&extra-field! entry (.bytes self (&compressed-size entry)))))

(method ZipReader .read ()
  (let (signature (.peek-u32 self))
    (if (= signature $zip.local-file-header-signature) (.read1 self)
        (= signature $zip.data-descriptor-signature) (.read (.skip-data-descriptor self))
        (in? signature $zip.headers) nil
        (raise ZipError "bad zip"))))

;; Writer

(class ZipWriter ()
  entries)

(method ZipWriter .add (file)
  (let (entry (.new ZipEntry))
    (&entries! self (cons entry (&entries self)))))

(method ZipWriter .write ()
  (write-bytes (&buf self)))

;; API

(function zip.entries (zipfile)
  (with-open ($in zipfile :read)
    (let (rd (.new ZipReader))
      (collect (f () (.read rd))))))

(function zip.entry-names (zipfile)
  (map .file-name (zip.entries zipfile)))


(function zip.compress (dir zipfile)
  nil)

(function zip.uncompress (zipfile dir)
  nil)

(function! main (args)
  (write (zip.entry-names "../wk/y.zip")))
