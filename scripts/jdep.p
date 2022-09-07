; java dependency.

(import :optparse)
(import :graphviz)

(<- $std (map str '(java javax org))
    $except-classes nil
    $except-packages $std
    $edges nil)

(function parse-java (java)
  (let (package nil imports nil)
    (dolist (line (.to-l java))
      (if (prefix? line "package") (<- package (butlast (last (split line " "))))
          (prefix? line "import") (push! (butlast (last (split line " "))) imports)))
    (list package imports)))

(function walker (file)
  (when (= (.suffix file) "java")
    (let (class-name (.base-name file) (package imports) (parse-java file))
      (when (&& (! (in? class-name $except-classes))
                (! (in? package $except-packages)))
        (foreach (f (x) (push! (list class-name x) $edges))
                 (keep (f (x)
                         (let (class-name (last (split x ".")))
                           (if (&& (none? (partial prefix? x) $except-packages)
                                   (none? (partial = class-name) $except-classes))
                               class-name)))
                       imports))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "c:o:p:") args)
                  c (.get op "c") o (.get op "o") p (.get op "p"))
    (if c (<- $except-classes (split c ",")))
    (if p (<- $except-packages (concat $std (split p ","))))
    (.walk (path ".") walker)
    (if (nil? o) (write $edges)
        (graphviz.dot $edges `((T png) (o ,o))))))
