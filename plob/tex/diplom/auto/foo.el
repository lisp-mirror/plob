(TeX-add-style-hook "foo"
 (function
  (lambda ()
    (TeX-run-style-hooks
     "dipldefs"
     "crossref"
     "babel"
     "latex2e"
     "pretty11"
     "pretty"
     "a4paper"
     "wide"
     "11pt"
     "titlepage"
     "twoside"
     "nothink"))))

