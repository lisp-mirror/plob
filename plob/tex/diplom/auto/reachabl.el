(TeX-add-style-hook "reachabl"
 (function
  (lambda ()
    (TeX-add-symbols
     "colsep"
     "rlineh"
     "etcetc"
     "mkbox"
     "hr"
     "begptr")
    (TeX-run-style-hooks
     "diagdefs"))))

