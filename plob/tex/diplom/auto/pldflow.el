(TeX-add-style-hook "pldflow"
 (function
  (lambda ()
    (TeX-add-symbols
     "objnamei"
     "objnameii"
     "objnameiii")
    (TeX-run-style-hooks
     "diagdefs"))))

