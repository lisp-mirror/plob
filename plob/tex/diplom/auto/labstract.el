(TeX-add-style-hook "labstract"
 (function
  (lambda ()
    (LaTeX-add-bibitems
     "bib:amop91"
     "bib:at89"
     "bib:br92"
     "bib:cl91"
     "bib:st90"
     "bib:co79"
     "bib:ki94a"
     "bib:ki94b"
     "bib:ki95"
     "bib:ma93"
     "bib:on94"
     "bib:schm80"
     "bib:we93")
    (LaTeX-add-labels
     "fig:incldbme"
     "fn:fig"
     "fig:inclme"
     "fig:rperlaye"
     "fig:ploblaye")
    (TeX-add-symbols
     '("inpos" 1)
     '("inpea" 1)
     "showfigrule"
     "etal")
    (TeX-run-style-hooks
     "picinpar"
     "epsfig"
     "timestt"
     "dipldefs"
     "crossref"
     "babel"
     "latex2e"
     "art10"
     "article"
     "a4paper"))))

