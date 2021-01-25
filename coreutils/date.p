; date and time.

(import :datetime)

(function! main (args)
  (write-line (.to-s (DateTime.now))))
