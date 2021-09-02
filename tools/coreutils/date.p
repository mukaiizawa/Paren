; date.

(import :datetime)

(function! main (args)
  (write-line (.to-s (datetime.now))))
