(TeX-add-style-hook "rperlay"
 (function
  (lambda ()
    (TeX-add-symbols
     "columnsep"
     "Nultext"
     "nframe")
    (TeX-run-style-hooks
     "diagdefs"))))

