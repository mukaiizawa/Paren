; vba module.

(class VBARange () expr)
(class VBACell (VBARange) row col)
(class VBASheet () name)
(class VBAFirstSheet (VBASheet) ())
(class VBALastSheet (VBASheet) ())
(class VBAActiveSheet (VBASheet) ())

(<- $vba-active-sheet (.new VBAActiveSheet)
    $vba-first-sheet (.new VBALastSheet)
    $vba-last-sheet (.new VBALastSheet))

; range

(method VBARange .init (expr)
  (&expr! self expr))

(method VBACell .init (row col)
  (&row! self row)
  (&col! self col))

(method VBARange .to-vbastr ()
  (string "Range(\"" (&expr self) "\")"))

(method VBACell .to-vbastr ()
  (string "Cells(" (++ (&row self)) ", " (++ (&col self)) ")"))

; sheet

(method VBASheet .init (name)
  (&name! self name))

(method VBASheet .to-vbastr ()
  (string "Worksheets(\"" (&name self) "\")"))

(method VBAFirstSheet .to-vbastr ()
  "Worksheets(1)")

(method VBALastSheet .to-vbastr ()
  "Worksheets(Worksheets.Count)")

(method VBAActiveSheet .to-vbastr ()
  "ActiveSheet")

(method VBASheet .activate ()
  ; Returns vba that activates the sheet.
  (string (.to-vbastr self) ".Activate\n"))

(method VBASheet .hide ()
  ; Returns vba to hide this sheet.
  (string (.to-vbastr self) ".Visible = xlVeryHidden\n"))

(method VBASheet .show ()
  ; Returns vba to show this sheet.
  (string (.to-vbastr self) ".Visible = True\n"))

(method VBASheet .remove ()
  ; Returns vba to show this sheet.
  (with-vba-vars (x)
    (string "For Each " x " In Worksheets\n"
            "If " x ".Name = \"" (&name self) "\" Then " x ".Delete: Exit For\n"
            "Next\n")))

(method VBASheet .add-last ()
  ; Returns vba to add this sheet to the end of the workbook.
  (string "Worksheets.Add after:=" (.to-vbastr $vba-last-sheet) "\n"
          (.to-vbastr $vba-active-sheet) ".Name = \"" (&name self) "\"\n"))

(method VBASheet .copy-to-last (dest-sheet)
  ; Returns vba that copies this sheet to the specified sheet.
  (string (.to-vbastr self) ".Copy after:=" (.to-vbastr $vba-last-sheet) "\n"
          (.to-vbastr $vba-last-sheet) ".Name = \"" (&name dest-sheet) "\"\n"))

(method VBASheet .select-range (r)
  ; Returns vba to select the specified range for this sheet.
  (string (.activate self)
          (.to-vbastr self) "." (.to-vbastr r) ".Select\n"))

(method VBASheet .select-A1 ()
  ; Returns vba to select cell A1 of this sheet.
  (.select-range self (cell 0 0)))

(method VBASheet .put (r val)
  ; Returns vba that sets the value val to the specified range.
  ; If val is a list, treat each element as a row.
  (let (lines (->list val))
    (with-vba-vars (var)
      var " = \"" (car lines) "\"\n"
      (join (map (f (x) (string var " = " var " & vblf & \"" x "\"\n"))
                 (cdr lines)))
      (.to-vbastr self) "." (.to-vbastr r) ".Value = " var "\n")))

(method VBASheet .copy-range (src-range dest-sheet dest-range)
  ; Returns vba that copies the value of the specified range to the specified range.
  (string (.to-vbastr self) "." (.to-vbastr src-range) ".Copy\n"
          (.to-vbastr dest-sheet) "." (.to-vbastr dest-range) ".Select\n"
          (.to-vbastr $vba-active-sheet) ".Paste\n"))

(method VBASheet .border (r)
  ; Returns vba that draws a ruled line in the specified range.
  (string (.to-vbastr self) "." (.to-vbastr r) ".Borders.LineStyle = True\n"))

(method VBASheet .print-title-rows (r)
  ; Returns vba that sets the rows that contain the cells to be repeated at the top of each page.
  (string (.to-vbastr self) ".PageSetup.PrintTitleRows = " (.to-vbastr r) ".Address\n"))

(method VBASheet .print-area (r)
  ; Returns vba that sets the specified range as the print target.
  (string (.to-vbastr self) ".PageSetup.PrintArea = " (.to-vbastr r) ".Address\n"))

(method VBASheet .auto-fit-row (r)
  ; Returns vba that automatically adjusts the row height of cells for the entire row in the specified range.
  (string (.to-vbastr self) "." (.to-vbastr r) ".EntireRow.AutoFit\n"))

(method VBASheet .wrap-text (r)
  ; Returns vba that sets wrapping for the specified range.
  (string (.to-vbastr self) "." (.to-vbastr r) ".WrapText = True\n"))

(method VBASheet .fix-header (r)
  ; Returns vba that fixes the window frame relative to the specified range.
  (string (.select-range self r)
          "ActiveWindow.FreezePanes = True\n"))

(function vba-alignment (alignment)
  (switch alignment
    :bottom "xlBottom"
    :center "xlCenter"
    :left "xlLeft"
    :right "xlRight"
    :top "xlTop"
    :default "xlGeneral"))

(method VBASheet .horizontal-align (r :opt alignment)
  ; Returns vba that sets the specified range to the specified horizontal alignment.
  (string (.to-vbastr self) "." (.to-vbastr r) ".HorizontalAlignment = " (vba-alignment alignment) "\n"))

(method VBASheet .vertical-align (r alignment)
  ; Returns vba that sets the specified range to the specified vertical alignment.
  (string (.to-vbastr self) "." (.to-vbastr r) ".VerticalAlignment = " (vba-alignment alignment) "\n"))

(method VBASheet .font-size (r n)
  ; Returns vba that sets the font size of the specified range to the specified size.
  (string (.to-vbastr self) "."(.to-vbastr r) ".Font.Size = " n "\n"))

; api

(let (sym-counts 0)
  (function vba-gensym ()
    (string "G" (<- sym-counts (++ sym-counts)))))

(macro with-vba-vars ((:rest vars) :rest exprs)
  ; Create a context that uses vba variables.
  ; All evaluation results of argument expressions must be vba.
  `(let ,(reduce (f (x y) `(,y (vba-gensym) ,@x)) (cons nil vars))
     (string ,@(reduce (f (x y) `("Dim " ,y "\n" ,@x)) (cons nil vars))
             ,@exprs)))

(function cell (row col)
  ; Returns the VBACell instance corresponding to the specified row and column.
  (.init (.new VBACell) row col))

(function range (expr)
  ; Returns the vbarange instance corresponding to the specified expression.
  ; Expressions that can be specified follow vba.
  (.init (.new VBARange) expr))

(function sheet (name)
  ; Returns the vba instance corresponding to the sheet name of the specified string.
  (.init (.new VBASheet) name))

(function vba-sub (name :rest exprs)
  (string "Sub " name "()\n"
          (join exprs)
          "End Sub\n"))

(function vba (:rest exprs)
  ; Returns vba that defines the main function to execute the tree of the specified vba expression.
  ; Ignore if the leaves contain nil.
  (let (parse-exprs
         (f (exprs counts subroutines mem)
           (if (nil? exprs) (list (++ counts)
                                  (reverse! (cons (vba-sub (string "sub" counts) (.to-s mem))
                                                  subroutines)))
               (begin
                 (.write-mem mem (car exprs))
                 (if (< (.size mem) 2000) (parse-exprs (cdr exprs) counts subroutines mem)
                     (parse-exprs (cdr exprs) (++ counts)
                                  (cons (vba-sub (string "sub" counts) (.to-s mem)) subroutines)
                                  (.reset mem))))))
         (subroutine-counts subroutines) (parse-exprs (except nil? (flatten exprs))
                                                      0 nil (.new MemoryStream)))
    (string "Option Explicit\n"
            (join subroutines)
            (vba-sub :main
                     "Application.ScreenUpdating = False\n"
                     "Application.DisplayAlerts = False\n"
                     (join (map (f (x) (string "Call sub" x "\n"))
                                (.. subroutine-counts)))
                     "Application.DisplayAlerts = True\n"
                     "Application.ScreenUpdating = True\n"))))

(function! main (args)
  (let (sheet1 (sheet "Sheet1") sheet2 (sheet "Sheet2")
               A1 (range "A1") origin (cell 0 0))
    (assert (memeq? (.activate sheet1) "Worksheets(\"Sheet1\").Activate\n"))
    (assert (memeq? (.show sheet1) "Worksheets(\"Sheet1\").Visible = True\n"))
    (assert (memeq? (.hide sheet1) "Worksheets(\"Sheet1\").Visible = xlVeryHidden\n"))
    (assert (memeq? (.remove sheet1)
                    (join '("Dim G1"
                            "For Each G1 In Worksheets"
                            "If G1.Name = \"Sheet1\" Then G1.Delete: Exit For"
                            "Next" "") "\n")))
    (assert (memeq? (.add-last sheet1)
                    (join '("Worksheets.Add after:=Worksheets(Worksheets.Count)"
                            "ActiveSheet.Name = \"Sheet1\"" "") "\n")))
    (assert (memeq? (.copy-to-last sheet1 sheet2)
                    (join '("Worksheets(\"Sheet1\").Copy after:=Worksheets(Worksheets.Count)"
                            "Worksheets(Worksheets.Count).Name = \"Sheet2\"" "") "\n")))
    (assert (memeq? (.select-range sheet1 origin) (.select-A1 sheet1)))
    (assert (memeq? (.put sheet1 origin '("foo" "bar"))
                    (join '("Dim G2"
                            "G2 = \"foo\""
                            "G2 = G2 & vblf & \"bar\""
                            "Worksheets(\"Sheet1\").Cells(1, 1).Value = G2" "") "\n")))
    (assert (memeq? (.copy-range sheet1 A1 sheet2 A1)
                    (join '("Worksheets(\"Sheet1\").Range(\"A1\").Copy"
                            "Worksheets(\"Sheet2\").Range(\"A1\").Select"
                            "ActiveSheet.Paste" "") "\n")))
    (assert (memeq? (.border sheet1 origin)
                    "Worksheets(\"Sheet1\").Cells(1, 1).Borders.LineStyle = True\n"))
    (assert (memeq? (.print-title-rows sheet1 origin)
                    "Worksheets(\"Sheet1\").PageSetup.PrintTitleRows = Cells(1, 1).Address\n"))
    (assert (memeq? (.print-area sheet1 origin)
                    "Worksheets(\"Sheet1\").PageSetup.PrintArea = Cells(1, 1).Address\n"))
    (assert (memeq? (.auto-fit-row sheet1 origin)
                    "Worksheets(\"Sheet1\").Cells(1, 1).EntireRow.AutoFit\n"))
    (assert (memeq? (.wrap-text sheet1 origin)
                    "Worksheets(\"Sheet1\").Cells(1, 1).WrapText = True\n"))
    (assert (memeq? (.fix-header sheet1 origin)
                    (join '("Worksheets(\"Sheet1\").Activate"
                            "Worksheets(\"Sheet1\").Cells(1, 1).Select"
                            "ActiveWindow.FreezePanes = True" "") "\n")))
    (assert (memeq? (.horizontal-align sheet1 origin :right)
                    "Worksheets(\"Sheet1\").Cells(1, 1).HorizontalAlignment = xlRight\n"))
    (assert (memeq? (.vertical-align sheet1 origin :top)
                    "Worksheets(\"Sheet1\").Cells(1, 1).VerticalAlignment = xlTop\n"))
    (assert (memeq? (.font-size sheet1 origin 9)
                    "Worksheets(\"Sheet1\").Cells(1, 1).Font.Size = 9\n"))
    (assert (memeq? (vba (.hide sheet1))
                    (join '("Option Explicit"
                            "Sub sub0()"
                            "Worksheets(\"Sheet1\").Visible = xlVeryHidden"
                            "End Sub"
                            "Sub main()"
                            "Application.ScreenUpdating = False"
                            "Application.DisplayAlerts = False"
                            "Call sub0"
                            "Application.DisplayAlerts = True"
                            "Application.ScreenUpdating = True"
                            "End Sub" "") "\n")))))
