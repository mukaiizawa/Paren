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
  (str "Range(\"" (&expr self) "\")"))

(method VBACell .to-vbastr ()
  (str "Cells(" (++ (&row self)) ", " (++ (&col self)) ")"))

; sheet

(method VBASheet .init (name)
  (&name! self name))

(method VBASheet .to-vbastr ()
  (str "Worksheets(\"" (&name self) "\")"))

(method VBAFirstSheet .to-vbastr ()
  "Worksheets(1)")

(method VBALastSheet .to-vbastr ()
  "Worksheets(Worksheets.Count)")

(method VBAActiveSheet .to-vbastr ()
  "ActiveSheet")

(method VBASheet .activate ()
  ; Returns vba that activates the sheet.
  (str (.to-vbastr self) ".Activate\n"))

(method VBASheet .hide ()
  ; Returns vba to hide this sheet.
  (str (.to-vbastr self) ".Visible = xlVeryHidden\n"))

(method VBASheet .show ()
  ; Returns vba to show this sheet.
  (str (.to-vbastr self) ".Visible = True\n"))

(method VBASheet .remove ()
  ; Returns vba to show this sheet.
  (with-vba-vars (x)
                 (str "For Each " x " In Worksheets\n"
                      "If " x ".Name = \"" (&name self) "\" Then " x ".Delete: Exit For\n"
                      "Next\n")))

(method VBASheet .rename (to)
  ; Returns vba to rename sheet.
  (str (.to-vbastr self) ".Name = \"" (&name to) "\"\n"))

(method VBASheet .add-last ()
  ; Returns vba to add this sheet to the end of the workbook.
  (str "Worksheets.Add after:=" (.to-vbastr $vba-last-sheet) "\n"
       (.rename $vba-active-sheet self)))

(method VBASheet .copy-to-last (dest-sheet)
  ; Returns vba that copies this sheet to the specified sheet.
  (str (.to-vbastr self) ".Copy after:=" (.to-vbastr $vba-last-sheet) "\n"
       (.rename $vba-last-sheet dest-sheet)))

(method VBASheet .select-range (r)
  ; Returns vba to select the specified range for this sheet.
  (str (.activate self)
       (.to-vbastr self) "." (.to-vbastr r) ".Select\n"))

(method VBASheet .select-A1 ()
  ; Returns vba to select cell A1 of this sheet.
  (.select-range self (vba.cell 0 0)))

(method VBASheet .put (r val)
  ; Returns vba that sets the value val to the specified range.
  ; If val is a list, treat each element as a row.
  (let (lines (atom->list val))
    (with-vba-vars (var)
                   var " = \"" (car lines) "\"\n"
                   (join (map (f (x) (str var " = " var " & vblf & \"" x "\"\n"))
                              (cdr lines)))
                   (.to-vbastr self) "." (.to-vbastr r) ".Value = " var "\n")))

(method VBASheet .copy-range (src-range dest-sheet dest-range)
  ; Returns vba that copies the value of the specified range to the specified range.
  (str (.to-vbastr self) "." (.to-vbastr src-range) ".Copy\n"
       (.to-vbastr dest-sheet) "." (.to-vbastr dest-range) ".Select\n"
       (.to-vbastr $vba-active-sheet) ".Paste\n"))

(method VBASheet .border (r)
  ; Returns vba that draws a ruled line in the specified range.
  (str (.to-vbastr self) "." (.to-vbastr r) ".Borders.LineStyle = True\n"))

(method VBASheet .print-title-rows (r)
  ; Returns vba that sets the rows that contain the cells to be repeated at the top of each page.
  (str (.to-vbastr self) ".PageSetup.PrintTitleRows = " (.to-vbastr r) ".Address\n"))

(method VBASheet .print-area (r)
  ; Returns vba that sets the specified range as the print target.
  (str (.to-vbastr self) ".PageSetup.PrintArea = " (.to-vbastr r) ".Address\n"))

(method VBASheet .auto-fit-row (r)
  ; Returns vba that automatically adjusts the row height of cells for the entire row in the specified range.
  (str (.to-vbastr self) "." (.to-vbastr r) ".EntireRow.AutoFit\n"))

(method VBASheet .wrap-text (r)
  ; Returns vba that sets wrapping for the specified range.
  (str (.to-vbastr self) "." (.to-vbastr r) ".WrapText = True\n"))

(method VBASheet .fix-header (r)
  ; Returns vba that fixes the window frame relative to the specified range.
  (str (.select-range self r)
       "ActiveWindow.FreezePanes = True\n"))

(function vba.alignment (alignment)
  (if (== alignment :bottom) "xlBottom"
      (== alignment :center) "xlCenter"
      (== alignment :left) "xlLeft"
      (== alignment :right) "xlRight"
      (== alignment :top) "xlTop"
      (== alignment :default) "xlGeneral"
      (raise ArgumentError "unexpected alignment")))

(method VBASheet .horizontal-align (r :opt alignment)
  ; Returns vba that sets the specified range to the specified horizontal alignment.
  (str (.to-vbastr self) "." (.to-vbastr r) ".HorizontalAlignment = " (vba.alignment alignment) "\n"))

(method VBASheet .vertical-align (r alignment)
  ; Returns vba that sets the specified range to the specified vertical alignment.
  (str (.to-vbastr self) "." (.to-vbastr r) ".VerticalAlignment = " (vba.alignment alignment) "\n"))

(method VBASheet .font-size (r n)
  ; Returns vba that sets the font size of the specified range to the specified size.
  (str (.to-vbastr self) "."(.to-vbastr r) ".Font.Size = " n "\n"))

; api

(macro with-vba-vars ((:rest vars) :rest exprs)
  ; Create a context that uses vba variables.
  ; All evaluation results of argument expressions must be vba.
  (let (vbasym (f () (str "G" (slice (str (gensym)) 3))))
    `(let ,(reduce (f (x y) `(,y ,(vbasym) ,@x)) (cons nil vars))
       (str ,@(reduce (f (x y) `("Dim " ,y "\n" ,@x)) (cons nil vars))
            ,@exprs))))

(function vba.cell (row col)
  ; Returns the VBACell instance corresponding to the specified row and column.
  (.init (.new VBACell) row col))

(function vba.range (expr)
  ; Returns the vbarange instance corresponding to the specified expression.
  ; Expressions that can be specified follow vba.
  (.init (.new VBARange) expr))

(function vba.sheet (name)
  ; Returns the vba instance corresponding to the sheet name of the specified string.
  (.init (.new VBASheet) name))

(function vba.sub (name :rest exprs)
  (str "Sub " name "()\n"
       (join exprs)
       "End Sub\n"))

(function vba (:rest exprs)
  ; Returns vba that defines the main function to execute the tree of the specified vba expression.
  ; Ignore if the leaves contain nil.
  (let (parse-exprs
         (f (exprs counts subroutines mem)
           (if (nil? exprs) (list (++ counts)
                                  (reverse! (cons (vba.sub (str "sub" counts) (.to-s mem))
                                                  subroutines)))
               (begin
                 (.write-bytes mem (car exprs))
                 (if (< (.size mem) 2000) (parse-exprs (cdr exprs) counts subroutines mem)
                     (parse-exprs (cdr exprs) (++ counts)
                                  (cons (vba.sub (str "sub" counts) (.to-s mem)) subroutines)
                                  (.reset mem))))))
         (subroutine-counts subroutines) (parse-exprs (reject nil? (flatten exprs))
                                                      0 nil (.new MemoryStream)))
    (str "Option Explicit\n"
         (join subroutines)
         (vba.sub :main
                  "Application.ScreenUpdating = False\n"
                  "Application.DisplayAlerts = False\n"
                  (join (map (f (x) (str "Call sub" x "\n"))
                             (.. subroutine-counts)))
                  "Application.DisplayAlerts = True\n"
                  "Application.ScreenUpdating = True\n"))))

(function! main (args)
  (let (sheet1 (vba.sheet "Sheet1") sheet2 (vba.sheet "Sheet2")
               A1 (vba.range "A1") origin (vba.cell 0 0))
    (assert (= (.activate sheet1) "Worksheets(\"Sheet1\").Activate\n"))
    (assert (= (.show sheet1) "Worksheets(\"Sheet1\").Visible = True\n"))
    (assert (= (.hide sheet1) "Worksheets(\"Sheet1\").Visible = xlVeryHidden\n"))
    (assert (= (.rename sheet1 sheet2) "Worksheets(\"Sheet1\").Name = \"Sheet2\"\n"))
    (assert (= (.add-last sheet1)
               (join '("Worksheets.Add after:=Worksheets(Worksheets.Count)"
                       "ActiveSheet.Name = \"Sheet1\"" "") "\n")))
    (assert (= (.copy-to-last sheet1 sheet2)
               (join '("Worksheets(\"Sheet1\").Copy after:=Worksheets(Worksheets.Count)"
                       "Worksheets(Worksheets.Count).Name = \"Sheet2\"" "") "\n")))
    (assert (= (.select-range sheet1 origin) (.select-A1 sheet1)))
    (assert (= (.copy-range sheet1 A1 sheet2 A1)
               (join '("Worksheets(\"Sheet1\").Range(\"A1\").Copy"
                       "Worksheets(\"Sheet2\").Range(\"A1\").Select"
                       "ActiveSheet.Paste" "") "\n")))
    (assert (= (.border sheet1 origin)
               "Worksheets(\"Sheet1\").Cells(1, 1).Borders.LineStyle = True\n"))
    (assert (= (.print-title-rows sheet1 origin)
               "Worksheets(\"Sheet1\").PageSetup.PrintTitleRows = Cells(1, 1).Address\n"))
    (assert (= (.print-area sheet1 origin)
               "Worksheets(\"Sheet1\").PageSetup.PrintArea = Cells(1, 1).Address\n"))
    (assert (= (.auto-fit-row sheet1 origin)
               "Worksheets(\"Sheet1\").Cells(1, 1).EntireRow.AutoFit\n"))
    (assert (= (.wrap-text sheet1 origin)
               "Worksheets(\"Sheet1\").Cells(1, 1).WrapText = True\n"))
    (assert (= (.fix-header sheet1 origin)
               (join '("Worksheets(\"Sheet1\").Activate"
                       "Worksheets(\"Sheet1\").Cells(1, 1).Select"
                       "ActiveWindow.FreezePanes = True" "") "\n")))
    (assert (= (.horizontal-align sheet1 origin :right)
               "Worksheets(\"Sheet1\").Cells(1, 1).HorizontalAlignment = xlRight\n"))
    (assert (= (.vertical-align sheet1 origin :top)
               "Worksheets(\"Sheet1\").Cells(1, 1).VerticalAlignment = xlTop\n"))
    (assert (= (.font-size sheet1 origin 9)
               "Worksheets(\"Sheet1\").Cells(1, 1).Font.Size = 9\n"))
    (assert (= (vba (.hide sheet1))
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
