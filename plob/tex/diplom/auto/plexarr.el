(TeX-add-style-hook "plexarr"
 (function
  (lambda ()
    (TeX-add-symbols
     "rlineh"
     "frbox"
     "colibox"
     "coliiibox"
     "frtxt"
     "frptr"
     "idx")
    (TeX-run-style-hooks
     "diagdefs"))))

