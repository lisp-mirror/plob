(TeX-add-style-hook "extent"
 (function
  (lambda ()
    (TeX-add-symbols
     "coliw"
     "colvw"
     "colviw"
     "rowh"
     "frbox"
     "frtxt")
    (TeX-run-style-hooks
     "diagdefs"))))

