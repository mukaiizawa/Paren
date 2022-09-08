; ASCII table.

;; -----------------------------
;; DEC HEX CTR ESC SYM
;; -----------------------------
;; 0   00  ^@   -  NUL
;; 1   01  ^A   -  SOH
;; 2   02  ^B   -  STX
;; 3   03  ^C   -  ETX interrupt
;; 4   04  ^D   -  EOT eof
;; 5   05  ^E   -  ENQ
;; 6   06  ^F   -  ACK
;; 7   07  ^G  \a  BEL
;; 8   08  ^H  \b  BS  backspace
;; 9   09  ^I  \t  HT  tab
;; 10  0A  ^J  \n  LF
;; 11  0B  ^K  \v  VT
;; 12  0C  ^L  \f  FF  form feed
;; 13  0D  ^M  \r  CR  enter
;; 14  0E  ^N   -  SO
;; 15  0F  ^O   -  SI
;; 16  10  ^P   -  DLE
;; 17  11  ^Q   -  DC1 XON
;; 18  12  ^R   -  DC2
;; 19  13  ^S   -  DC3 XOFF
;; 20  14  ^T   -  DC4
;; 21  15  ^U   -  NAK
;; 22  16  ^V   -  SYN
;; 23  17  ^W   -  ETB
;; 24  18  ^X   -  CAN
;; 25  19  ^Y   -  EM
;; 26  1A  ^Z   -  SUB
;; 27  1B  ^[  \e  ESC escape
;; 28  1C  ^\   -  FS
;; 29  1D  ^]   -  GS
;; 30  1E  ^^   -  RS
;; 31  1F  ^_   -  US
;; 127 7F   -   -  DEL delete
;; -----------------------------

(function! main (args)
  (dolist (line (.to-l (.resolve $paren-home "scripts/asctable.p")))
    (if (prefix? line ";;") (write-line (slice line 3))))
  (write-line)
  (write-line "   0 1 2 3 4 5 6 7 8 9 a b c d e f")
  (for (i 0 ch (chr i)) (<= i 0x7f) (i (++ i) ch (chr i))
    (when (= (% i 0x10) 0)
      (write-bytes (format "%02x " i)))
    (write-bytes (if (print? ch) ch "."))
    (write-bytes (if (= (% i 0x10) 0x0f) "\n" " "))))
