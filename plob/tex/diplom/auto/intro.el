(TeX-add-style-hook "intro"
 (function
  (lambda ()
    (LaTeX-add-labels
     "chap:intro"
     "sec:task"
     "fig:mopcls")
    (TeX-add-symbols
     "name"
     "mkline"))))

