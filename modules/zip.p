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
    $zip64.end-of-central-dir-signature             0x06064b50    ; not yet supported.
    $zip.compression-method.no-compression 0
    $zip.compression-method.deflated 8
    $zip.version-need-to-extract 20
    $zip.made-by-unix 3
    $zip.version-made-by (| (<< $zip.made-by-unix 8) $zip.version-need-to-extract))

(class ZipError (Error))

;; Entry

(class ZipEntry ()
  version-made-by
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
  file-comment-length
  file-name
  extra-field
  file-data
  relative-offset-of-local-header)

(method ZipEntry .uncompressed-size ()
  self->uncompressed-size)

(method ZipEntry .compressed-size ()
  self->compressed-size)

(method ZipEntry .timestamp ()
  (datetime.parse-msdos-datetime self->last-mod-file-date self->last-mod-file-time))

(method ZipEntry .file-name ()
  (string self->file-name))

(method ZipEntry .uncompress ()
  (let (compression-method self->compression-method contents self->file-data)
    (if (= compression-method $zip.compression-method.no-compression) contents
        (= compression-method $zip.compression-method.deflated) (deflate.uncompress contents)
        (raise ZipError "unsupported compression method"))))

(method ZipEntry .compress ()
  (let (compression-method self->compression-method contents self->file-data)
    (if (= compression-method $zip.no-compression) contents
        (raise ZipError "unsupported compression method"))))

;; ZipStream

(class ZipStream ()
  buf pos)

(method ZipStream .skip (n)
  (<- self->pos (+ self->pos n)))

;; Reader

(class ZipReader (ZipStream)
  entry-count)

(method ZipReader .init ()
  (<- self->buf (read-bytes)
      self->pos (- (len self->buf) 22))
  ;; scan end of central directory record
  (loop
    (let (u32 (.peek-u32 self))
      (if (= u32 $zip.end-of-central-dir-signature) (begin (.parse-end-of-central-directory self) (break))
          (= u32 $zip64.end-of-central-dir-signature) (raise ArgumentError "zip64 format not supported yet")
          (< (<- self->pos (- self->pos 8)) 0) (raise ArgumentError "missing end of central directory"))))
  self)

(method ZipReader .parse-end-of-central-directory ()
  (.skip self 4)    ; end of central dir signature
  (.skip self 2)    ; number of this disk
  (.skip self 2)    ; number of the disk with the start of the central directory
  (<- self->entry-count (.read-u16 self))    ; total number of entries in the central directory on this disk
  (.skip self 2)    ; total number of entries in the central directory
  (.skip self 4)    ; size of the central directory
  (<- self->pos (.read-u32 self))) ; offset of start of central directory with respect to the starting disk number        4 bytes

(method ZipReader .read-u16 ()
  (begin0
    (bin.ui16LE self->buf self->pos)
    (.skip self 2)))

(method ZipReader .read-u32 ()
  (begin0
    (.peek-u32 self)
    (.skip self 4)))

(method ZipReader .peek-u32 ()
  (bin.ui32LE self->buf self->pos))

(method ZipReader .read-size (len)
  (begin0
    (bytes self->buf self->pos (+ self->pos len))
    (.skip self len)))

(method ZipReader .read ()
  (if (< (<- self->entry-count (-- self->entry-count)) 0) nil
      (let (entry (.new ZipEntry) pos nil)
        ;; central directory header
        (if (!= (.read-u32 self) $zip.central-file-header-signature) (raise StateError "expected central directory header"))
        (<- entry->version-made-by (.read-u16 self)
            entry->version-needed-to-extract (.read-u16 self)
            entry->general-purpose-bit-flag (.read-u16 self)
            entry->compression-method (.read-u16 self)
            entry->last-mod-file-time (.read-u16 self)
            entry->last-mod-file-date (.read-u16 self)
            entry->crc-32 (.read-u32 self)
            entry->compressed-size (.read-u32 self)
            entry->uncompressed-size (.read-u32 self)
            entry->file-name-length (.read-u16 self)
            entry->extra-field-length (.read-u16 self)
            entry->file-comment-length (.read-u16 self))
        (.skip self 2)    ; disk number start
        (.skip self 2)    ; internal file attributes
        (.skip self 4)    ; external file attributes
        (<- entry->relative-offset-of-local-header (.read-u32 self))
        (.skip self entry->file-name-length)
        (.skip self entry->extra-field-length)
        (.skip self entry->file-comment-length)
        (<- pos self->pos
            self->pos entry->relative-offset-of-local-header)
        ;; local file header
        (if (!= (.read-u32 self) $zip.local-file-header-signature) (raise StateError "expected local file header"))
        (.skip self 22)
        (<- entry->file-name-length (.read-u16 self)
            entry->extra-field-length (.read-u16 self)
            entry->file-name (.read-size self entry->file-name-length)
            entry->extra-field (.read-size self entry->extra-field-length)
            entry->file-data (.read-size self entry->compressed-size)
            self->pos pos)
        entry)))

;; Writer

(class ZipWriter (ZipStream)
  entries)

(method ZipWriter .init ()
  (<- self->pos 0
      self->buf (bytes 1024))
  self)

(method ZipWriter .reserve (n)
  (let (pos (+ self->pos n) size (len self->buf))
    (when (< size pos)
      (while (< (<- size (* size 2)) pos))
      (let (buf (bytes size))
        (memcpy self->buf 0 buf 0 self->pos)
        (<- self->buf buf)))
    self->buf))

(method ZipWriter .write-u16 (val)
  (bin.ui16LE! (.reserve self 2) self->pos val)
  (.skip self 2))

(method ZipWriter .write-u32 (val)
  (bin.ui32LE! (.reserve self 4) self->pos val)
  (.skip self 4))

(method ZipWriter .write-bytes (val)
  (let (size (len val))
    (memcpy val 0 (.reserve self size) self->pos size)
    (.skip self size)))

(method ZipWriter .add (file :opt alias)
  (let (file-name (bytes (|| alias (.name file)))
                  mtime (.init (.new DateTime) (.mtime file))
                  file-data (.contents file)
                  entry (.new ZipEntry))
    (<- entry->general-purpose-bit-flag 0
        entry->compression-method 0
        entry->last-mod-file-time (.msdos-time mtime)
        entry->last-mod-file-date (.msdos-date mtime)
        entry->crc-32 (crc32.sum file-data)
        entry->compressed-size (len file-data)
        entry->uncompressed-size (.size file)
        entry->file-name-length (len file-name)
        entry->extra-field-length 0
        entry->file-name file-name
        entry->extra-field (bytes 0)
        entry->file-data file-data
        self->entries (cons entry self->entries))
    self))

(method ZipWriter .write-local-file-header (entry)
  (<- entry->relative-offset-of-local-header self->pos)
  (.write-u32 self $zip.local-file-header-signature)
  (.write-u16 self $zip.version-need-to-extract)
  (.write-u16 self entry->general-purpose-bit-flag)
  (.write-u16 self entry->compression-method)
  (.write-u16 self entry->last-mod-file-time)
  (.write-u16 self entry->last-mod-file-date)
  (.write-u32 self entry->crc-32)
  (.write-u32 self entry->compressed-size)
  (.write-u32 self entry->uncompressed-size)
  (.write-u16 self entry->file-name-length)
  (.write-u16 self entry->extra-field-length)
  (.write-bytes self entry->file-name)
  (.write-bytes self entry->extra-field)
  (.write-bytes self entry->file-data))

(method ZipWriter .write-central-directory-header (entry)
  (.write-u32 self $zip.central-file-header-signature)
  (.write-u16 self $zip.version-made-by)
  (.write-u16 self $zip.version-need-to-extract)
  (.write-u16 self entry->general-purpose-bit-flag)
  (.write-u16 self entry->compression-method)
  (.write-u16 self entry->last-mod-file-time)
  (.write-u16 self entry->last-mod-file-date)
  (.write-u32 self entry->crc-32)
  (.write-u32 self entry->compressed-size)
  (.write-u32 self entry->uncompressed-size)
  (.write-u16 self entry->file-name-length)
  (.write-u16 self entry->extra-field-length)
  (.write-u16 self 0)     ; extra field length
  (.write-u16 self 0)     ; file comment length
  (.write-u16 self 0)     ; disk number start
  (.write-u16 self 0)     ; internal file attributes
  (.write-u16 self 0)     ; external file attributes
  (.write-u32 self entry->relative-offset-of-local-header)
  (.write-bytes self entry->file-name))

(method ZipWriter .write-end-of-central-directry-record (central-directory-header-start)
  (let (end-of-central-directory-record-start self->pos entry-count (len self->entries))
    (.write-u32 self $zip.end-of-central-dir-signature)
    (.write-u16 self 0)    ; number of this disk
    (.write-u16 self 0)    ; number of the disk with the start of the central directory
    (.write-u16 self entry-count)    ; total number of entries in the central directory on this disk
    (.write-u16 self entry-count)    ; total number of entries in the central directory
    (.write-u32 self (- end-of-central-directory-record-start central-directory-header-start))    ; size of the central directory
    (.write-u32 self central-directory-header-start)    ; offset of start of central directory with respect to the starting disk number
    (.write-u16 self 0)))    ; .ZIP file comment length

(method ZipWriter .write ()
  (let (entries (reverse self->entries) central-directory-header-start nil)
    (foreach (partial .write-local-file-header self) entries)
    (<- central-directory-header-start self->pos)
    (foreach (partial .write-central-directory-header self) entries)
    (.write-end-of-central-directry-record self central-directory-header-start)
    (write-bytes self->buf 0 self->pos)))

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
    (assert (= (map (compose (f (x) (str "./" x)) .name)
                    (.children dir))
               (zip.entry-names (zip.compress dir zipfile))))
    (assert (= (zip.uncompress zipfile wk) wk))
    (assert (.none? (.remove zipfile)))
    (assert (= (map .name (.children dir))
               (map .name (.children wk))))
    (assert (.none? (.remove wk)))))
