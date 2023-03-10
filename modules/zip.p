; zip module.

(import :bin)
(import :crc32)
(import :datetime)
(import :deflate)

(<- $zip.local-file-header-signature                0x04034b50
    $zip.central-file-header-signature              0x02014b50
    $zip.end-of-central-dir-signature               0x06054b50
    $zip.compression-method.no-compression 0
    $zip.compression-method.deflated 8
    $zip.version-need-to-extract 20
    $zip.made-by.unix 3
    $zip.version-made-by (| (<< $zip.made-by.unix 8) $zip.version-need-to-extract))

;; Zip

(class Zip ()
  stream entries)

;; accessor

(method Zip .add-file (file :opt alias)
  (.add-entry self (ZipEntry::from (<- file (path file)) (|| alias (.name file)))))

(method Zip .add-entry (entry)
  (<- entry->zip self
      self->entries (cons entry self->entries))
  self)

(method Zip .entries ()
  self->entries)

;; stream wrapper

(method Zip .seek-set (offset)
  (.seek self->stream offset))

(method Zip .seek-cur (offset)
  (.seek self->stream (+ (.tell self->stream) offset)))

(method Zip .seek-end (offset)
  (.seek self->stream (+ (.tell (.seek self->stream -1)) offset)))

(method Zip .tell ()
  (.tell self->stream))

(method Zip .read-bytes (size)
  (let (buf (bytes size))
    (.read-bytes self->stream buf 0 size)
    buf))

(method Zip .read-u16 ()
  (bin.ui16LE (.read-bytes self 2) 0))

(method Zip .read-u32 ()
  (bin.ui32LE (.read-bytes self 4) 0))

(method Zip .write-bytes (bytes)
  (.write-bytes self->stream bytes))

(method Zip .write-u16 (val)
  (.write-bytes self (bin.ui16LE! (bytes 2) 0 val)))

(method Zip .write-u32 (val)
  (.write-bytes self (bin.ui32LE! (bytes 4) 0 val)))

(method Zip .read (:opt in)
  (<- self->stream (|| in (dynamic $in)))
  (.seek-end self -22)
  (catch
    (while (!= (.read-u32 self) $zip.end-of-central-dir-signature)
      (.seek-cur self -5))
    (f (e)
      (raise ArgumentError "failed to read end of central directory")))
  (.parse-end-of-central-directory self)
  self)

;; reader

(method Zip .parse-end-of-central-directory ()
  (.seek-cur self 4)
  (let (entry-count (.read-u16 self) entries nil)
    (.seek-cur self 6)
    (.seek-set self (.read-u32 self))
    (dotimes (_ entry-count)
      (push! (.parse-entry self) entries))
    (<- self->entries (reverse! entries))
    self))

(method Zip .parse-entry ()
  (let (entry (.init (.new ZipEntry) self))
    ;; central directory header
    (if (!= (.read-u32 self) $zip.central-file-header-signature) (raise StateError "expected central directory header at %d" (- (.tell self) 4)))
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
        entry->file-comment-length (.read-u16 self)
        entry->disk-number-start (.read-u16 self)
        entry->internal-file-attributes (.read-u16 self)
        entry->external-file-attributes (.read-u32 self)
        entry->relative-offset-of-local-header (.read-u32 self))
    ;; local file header
    (let (return-addr (+ (.tell self)
                         entry->file-name-length
                         entry->extra-field-length
                         entry->file-comment-length))
      (.seek-set self entry->relative-offset-of-local-header)
      (if (!= (.read-u32 self) $zip.local-file-header-signature) (raise StateError "expected local file header"))
      (.seek-cur self 22)
      (<- entry->file-name-length (.read-u16 self)
          entry->extra-field-length (.read-u16 self)
          entry->file-name (.read-bytes self entry->file-name-length)
          entry->extra-field (.read-bytes self entry->extra-field-length))
      (.seek-set self return-addr)
      entry)))

(method Zip .read-entry-data (entry)
  (catch
    (begin
      (.seek-set self 0)
      (.seek-set self (+ entry->relative-offset-of-local-header
                         30
                         entry->file-name-length
                         entry->extra-field-length))
      (.read-bytes self entry->compressed-size))
    (f (e)
      (if (is-a? e OSError) (raise StateError "probably a closed zip file")
          (throw e)))))

;; writer

(method Zip .write-local-file-header (entry)
  (<- entry->relative-offset-of-local-header (.tell self))
  (.write-u32 self $zip.local-file-header-signature)
  (.write-u16 self entry->version-needed-to-extract)
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
  (.write-bytes self (.compress entry)))

(method Zip .write-central-directory-header (entry)
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
  (.write-u16 self entry->file-comment-length)
  (.write-u16 self entry->disk-number-start)
  (.write-u16 self entry->internal-file-attributes)
  (.write-u32 self entry->external-file-attributes)
  (.write-u32 self entry->relative-offset-of-local-header)
  (.write-bytes self entry->file-name))

(method Zip .write-end-of-central-directry-record (central-directory-header-start)
  (let (end-of-central-directory-record-start (.tell self) entry-count (len self->entries))
    (.write-u32 self $zip.end-of-central-dir-signature)
    (.write-u16 self 0)    ; number of this disk
    (.write-u16 self 0)    ; number of the disk with the start of the central directory
    (.write-u16 self entry-count)    ; total number of entries in the central directory on this disk
    (.write-u16 self entry-count)    ; total number of entries in the central directory
    (.write-u32 self (- end-of-central-directory-record-start central-directory-header-start))    ; size of the central directory
    (.write-u32 self central-directory-header-start)    ; offset of start of central directory with respect to the starting disk number
    (.write-u16 self 0)))    ; .ZIP file comment length

(method Zip .write ()
  (let (entries (reverse self->entries) central-directory-header-start nil)
    (<- self->stream (dynamic $out))
    (foreach (partial .write-local-file-header self) entries)
    (<- central-directory-header-start (.tell self))
    (foreach (partial .write-central-directory-header self) entries)
    (.write-end-of-central-directry-record self central-directory-header-start)))

;; ZipEntry

(class ZipEntry ()
  zip
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
  internal-file-attributes
  external-file-attributes
  disk-number-start
  file-name
  extra-field
  file-data    ; lazy fetch
  relative-offset-of-local-header)

(method ZipEntry .init (zip)
  (<- self->zip zip)
  self)

(function ZipEntry::from (file file-name)
  (with-arrow-syntax
    (let (entry (.new ZipEntry) mtime (.init (.new DateTime) (.mtime file)) file-data (.contents file))
      (<- entry->version-needed-to-extract $zip.version-need-to-extract
          entry->general-purpose-bit-flag 0
          entry->compression-method $zip.compression-method.no-compression
          entry->last-mod-file-time (.msdos-time mtime)
          entry->last-mod-file-date (.msdos-date mtime)
          entry->crc-32 (crc32.sum file-data)
          entry->compressed-size (len file-data)
          entry->uncompressed-size (.size file)
          entry->file-name-length (len file-name)
          entry->extra-field-length 0
          entry->file-comment-length 0
          entry->disk-number-start 0
          entry->internal-file-attributes 0
          entry->external-file-attributes 2176188416
          entry->file-name file-name
          entry->extra-field (bytes 0)
          entry->file-data file-data)
      entry)))

(method ZipEntry .uncompressed-size ()
  self->uncompressed-size)

(method ZipEntry .compressed-size ()
  self->compressed-size)

(method ZipEntry .timestamp ()
  (datetime.parse-msdos-datetime self->last-mod-file-date self->last-mod-file-time))

(method ZipEntry .file-name ()
  (string self->file-name))

(method ZipEntry .uncompress ()
  (let (compression-method self->compression-method
                           file-data (|| self->file-data (<- self->file-data (.read-entry-data self->zip self))))
    (if (= compression-method $zip.compression-method.no-compression) file-data
        (= compression-method $zip.compression-method.deflated) (deflate.uncompress file-data)
        (raise NotImplementedError "unsupported compression method"))))

(method ZipEntry .compress ()
  (let (compression-method self->compression-method file-data self->file-data)
    (if (= compression-method $zip.compression-method.no-compression) file-data
        (raise NotImplementedError "unsupported compression method"))))

;; API

(function zip.compress (dir zipfile)
  (let (root (path dir) zip (.new Zip))
    (.walk root (f (x) (.add-file zip x (.to-s (.relativize root x)))))
    (with-open ($out zipfile :write)
      (.write zip))
    zipfile))

(function zip.uncompress (zipfile dir)
  (let (root (path dir))
    (with-open ($in zipfile :read)
      (dolist (entry (.entries (.read (.new Zip))))
        (when (pos? (.uncompressed-size entry))
          (let (file (.resolve root (.file-name entry)) parent (.parent file))
            (with-open ($out file :write)
              (write-bytes (.uncompress entry)))))))
    root))

(function! main (args)
  (let (dir (path "../modules") wk (path "../wk/zip/") zipfile (.resolve wk "modules.zip"))
    (assert (= (map (f (x) (str "./" (.name x))) (.children dir))
               (map .file-name (zip.entries (zip.compress dir zipfile)))))
    (assert (= (zip.uncompress zipfile wk) wk))
    (assert (.none? (.remove zipfile)))
    (assert (= (map .name (.children dir))
               (map .name (.children wk))))
    (assert (.none? (.remove wk)))))
