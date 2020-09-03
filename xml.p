; xml module.

(class XML.Node ())
(class XML.DOCTYPE (XML.Node) value)
(class XML.Decl (XML.Node) value)
(class XML.Comment (XML.Node) value)
(class XML.Text (XML.Node) value)
(class XML.Element (XML.Node) name attrs children)

(method XML.Node .to-s ()
  (assert nil))

(method XML.DOCTYPE .to-s ()
  (string "<!DOCTYPE " (&value self) ">"))

(method XML.Decl .to-s ()
  (string "<? " (&value self) " ?>"))

(method XML.Comment .to-s ()
  (string "<!--" (&value self) "-->"))

(method XML.Text .to-s ()
  (&value self))

(method XML.Element .to-s ()
  (string "<" (&name self) (list->string (map (lambda (attr)
                                                (let (key (bytes->string (car attr)) val (cadr attr))
                                                  (if val (string " " key "='" val "'")
                                                      (string " " key))))
                                              (group (&attrs self) 2))) ">"
          (list->string (map .to-s (&children self)))
          "</" (&name self) ">"))
