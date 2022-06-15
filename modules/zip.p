; zip module.

(import :bin)
(import :crc32)
(import :datetime)
(import :deflate)

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
                       $zip.end-of-central-dir-signature)
    $zip.compression-method.no-compression 0
    $zip.compression-method.deflated 8
    $zip.version-need-to-extract 20
    $zip.made-by-unix 3
    $zip.version-made-by (| (<< $zip.made-by-unix 8) $zip.version-need-to-extract))

(class ZipError (Error))

;; Entry

(class ZipEntry ()
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
  extra-field
  file-data
  relative-offset-of-local-header)

(method ZipEntry .uncompressed-size ()
  (&uncompressed-size self))

(method ZipEntry .compressed-size ()
  (&compressed-size self))

(method ZipEntry .timestamp ()
  (datetime.parse-msdos-datetime (&last-mod-file-date self) (&last-mod-file-time self)))

(method ZipEntry .file-name ()
  (string (&file-name self)))

(method ZipEntry .uncompress ()
  (let (compression-method (&compression-method self) contents (&file-data self))
    (if (= compression-method $zip.compression-method.no-compression) contents
        (= compression-method $zip.compression-method.deflated) (deflate.uncompress contents)
        (raise ZipError "unsupported compression method"))))

(method ZipEntry .compress ()
  (let (compression-method (&compression-method self) contents (&file-data self))
    (if (= compression-method $zip.no-compression) contents
        (raise ZipError "unsupported compression method"))))

;; ZipStream

(class ZipStream ()
  buf pos)

(method ZipStream .skip (n)
  (&pos! self (+ (&pos self) n)))

;; Reader

(class ZipReader (ZipStream))

(method ZipReader .init ()
  (&pos! self 0)
  (&buf! self (read-bytes)))

(method ZipReader .skip-data-descriptor (n)
  (while (! (in? (.peek-u32 self) $zip.headers))
    (.skip 4))
  self)    ; 4 or 8

(method ZipReader .read-u16 ()
  (begin0
    (bin.ui16LE (&buf self) (&pos self))
    (.skip self 2)))

(method ZipReader .read-u32 ()
  (begin0
    (.peek-u32 self)
    (.skip self 4)))

(method ZipReader .peek-u32 ()
  (bin.ui32LE (&buf self) (&pos self)))

(method ZipReader .read-size (len)
  (begin0
    (bytes (&buf self) (&pos self) (+ (&pos self) len))
    (.skip self len)))

(method ZipReader .read1 ()
  (let (entry (.new ZipEntry))
    (.read-u32 self)
    (&version-needed-to-extract! entry (.read-u16 self))
    (&general-purpose-bit-flag! entry (.read-u16 self))
    (&compression-method! entry (.read-u16 self))
    (&last-mod-file-time! entry (.read-u16 self))
    (&last-mod-file-date! entry (.read-u16 self))
    (&crc-32! entry (.read-u32 self))
    (&compressed-size! entry (.read-u32 self))
    (&uncompressed-size! entry (.read-u32 self))
    (&file-name-length! entry (.read-u16 self))
    (&extra-field-length! entry (.read-u16 self))
    (&file-name! entry (.read-size self (&file-name-length entry)))
    (&extra-field! entry (.read-size self (&extra-field-length entry)))
    (&file-data! entry (.read-size self (&compressed-size entry)))))

(method ZipReader .read ()
  (let (signature (.peek-u32 self))
    (if (= signature $zip.local-file-header-signature) (.read1 self)
        (= signature $zip.data-descriptor-signature) (.read (.skip-data-descriptor self))
        (in? signature $zip.headers) nil
        (raise ZipError "bad zip"))))

;; Writer

(class ZipWriter (ZipStream)
  entries)

(method ZipWriter .init ()
  (&pos! self 0)
  (&buf! self (bytes 1024)))

(method ZipWriter .reserve (n)
  (let (pos (+ (&pos self) n) size (len (&buf self)))
    (when (< size pos)
      (while (< (<- size (* size 2)) pos))
      (let (buf (bytes size))
        (memcpy (&buf self) 0 buf 0 (&pos self))
        (&buf! self buf)))
    (&buf self)))

(method ZipWriter .write-u16 (val)
  (bin.ui16LE! (.reserve self 2) (&pos self) val)
  (.skip self 2))

(method ZipWriter .write-u32 (val)
  (bin.ui32LE! (.reserve self 4) (&pos self) val)
  (.skip self 4))

(method ZipWriter .write-bytes (val)
  (let (size (len val))
    (memcpy val 0 (.reserve self size) (&pos self) size)
    (.skip self size)))

(method ZipWriter .add (file :opt alias)
  (let (file (path file)
             file-name (bytes (|| alias (.name file)))
             mtime (.init (.new DateTime) (.mtime file))
             file-data (.contents file)
             entry (.new ZipEntry))
    (&general-purpose-bit-flag! entry 0)
    (&compression-method! entry 0)
    (&last-mod-file-time! entry (.msdos-time mtime))
    (&last-mod-file-date! entry (.msdos-date mtime))
    (&crc-32! entry (crc32.sum file-data))
    (&compressed-size! entry (len file-data))
    (&uncompressed-size! entry (.size file))
    (&file-name-length! entry (len file-name))
    (&extra-field-length! entry 0)
    (&file-name! entry file-name)
    (&extra-field! entry (bytes 0))
    (&file-data! entry file-data)
    (&entries! self (cons entry (&entries self)))))

(method ZipWriter .write-local-file-header (entry)
  (&relative-offset-of-local-header! entry (&pos self))
  (.write-u32 self $zip.local-file-header-signature)
  (.write-u16 self $zip.version-need-to-extract)
  (.write-u16 self (&general-purpose-bit-flag entry))
  (.write-u16 self (&compression-method entry))
  (.write-u16 self (&last-mod-file-time entry))
  (.write-u16 self (&last-mod-file-date entry))
  (.write-u32 self (&crc-32 entry))
  (.write-u32 self (&compressed-size entry))
  (.write-u32 self (&uncompressed-size entry))
  (.write-u16 self (&file-name-length entry))
  (.write-u16 self (&extra-field-length entry))
  (.write-bytes self (&file-name entry))
  (.write-bytes self (&extra-field entry))
  (.write-bytes self (&file-data entry)))

(method ZipWriter .write-central-directory-header (entry)
  (.write-u32 self $zip.central-file-header-signature)
  (.write-u16 self $zip.version-made-by)
  (.write-u16 self $zip.version-need-to-extract)
  (.write-u16 self (&general-purpose-bit-flag entry))
  (.write-u16 self (&compression-method entry))
  (.write-u16 self (&last-mod-file-time entry))
  (.write-u16 self (&last-mod-file-date entry))
  (.write-u32 self (&crc-32 entry))
  (.write-u32 self (&compressed-size entry))
  (.write-u32 self (&uncompressed-size entry))
  (.write-u16 self (&file-name-length entry))
  (.write-u16 self (&extra-field-length entry))
  (.write-u16 self 0)     ; extra field length
  (.write-u16 self 0)     ; file comment length
  (.write-u16 self 0)     ; disk number start
  (.write-u16 self 0)     ; internal file attributes
  (.write-u16 self 0)     ; external file attributes
  (.write-u32 self (&relative-offset-of-local-header entry))
  (.write-bytes self (&file-name entry)))

(method ZipWriter .write-end-of-central-directry-record (central-directory-header-start)
  (let (end-of-central-directory-record-start (&pos self) entry-count (len (&entries self)))
    (.write-u32 self $zip.end-of-central-dir-signature)
    (.write-u16 self 0)    ; number of this disk
    (.write-u16 self 0)    ; number of the disk with the start of the central directory
    (.write-u16 self entry-count)    ; total number of entries in the central directory on this disk
    (.write-u16 self entry-count)    ; total number of entries in the central directory
    (.write-u32 self (- end-of-central-directory-record-start central-directory-header-start))    ; size of the central directory
    (.write-u32 self central-directory-header-start)    ; offset of start of central directory with respect to the starting disk number
    (.write-u16 self 0)))    ; .ZIP file comment length

(method ZipWriter .write ()
  (let (entries (reverse (&entries self)) central-directory-header-start nil)
    (foreach (partial .write-local-file-header self) entries)
    (<- central-directory-header-start (&pos self))
    (foreach (partial .write-central-directory-header self) entries)
    (.write-end-of-central-directry-record self central-directory-header-start)
    (write-bytes (&buf self) 0 (&pos self))))

;; API

(function zip.entries (zipfile)
  (with-open ($in zipfile :read)
    (collect (partial .read (.new ZipReader)))))

(function zip.entry-names (zipfile)
  (map .file-name (zip.entries zipfile)))

(function zip.compress (dir zipfile)
  (let (root (path dir) wr (.new ZipWriter))
    (.walk root (f (x) (.add wr x (.to-s (.relativize root x)))))
    (with-open ($out zipfile :write)
      (.write wr))
    zipfile))

(function zip.uncompress (zipfile dir)
  (let (root (path dir))
    (dolist (entry (zip.entries zipfile))
      (when (pos? (.uncompressed-size entry))
        (let (file (.resolve root (.file-name entry)) parent (.parent file))
          (with-open ($out file :write)
            (write-bytes (.uncompress entry))))))
    root))

(function! main (args)
  (let (dir (path "../modules") wk (path "../wk/zip/") zipfile (.resolve wk "modules.zip"))
    (assert (= (map .name (.children dir))
               (zip.entry-names (zip.compress dir zipfile))))
    (assert (= (zip.uncompress zipfile wk) wk))
    (assert (.none? (.remove zipfile)))
    (assert (= (map .name (.children dir))
               (map .name (.children wk))))
    (assert (.none? (.remove wk)))))
