# ----------------------------------------------------------------------
# Makefile fuer Diplomarbeit
# 21.09.1993 Heiko Kirschke, Fachbereich Informatik, Uni Hamburg
# e-mail: kirschke@informatik.uni-hamburg.de
#
# Um LaTeX im Batchmode auszufuehren, dieses Makeskript mit
#	make BATCHMODE=T [<Target> [<More targets>]]
# aufrufen oder die Environmentvariable BATCHMODE auf T setzen
# (exportieren nicht vergessen).
# Alle hier benutzten Standard-Regeln sind in plob/conf/default.mk
# definiert.
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Konstanten:
# ----------------------------------------------------------------------
HOMEDIR=/home/kirschke
PLOBDIR=$(HOMEDIR)/plob
PSDIR=$(PLOBDIR)/ps

DIR=tenpt
EXT=eps
FIGURES=$(DIR)/adridx.$(EXT)	$(DIR)/adrname.$(EXT)	\
	$(DIR)/adrpath.$(EXT)	$(DIR)/clicmc.$(EXT)	\
	$(DIR)/clolout.$(EXT)	$(DIR)/clshier.$(EXT)	\
	$(DIR)/dadr.$(EXT)	$(DIR)/exidrepr.$(EXT)	\
	$(DIR)/extent.$(EXT)	$(DIR)/gctrpe.$(EXT)	\
	$(DIR)/hlsecst.$(EXT)	$(DIR)/hperlay.$(EXT)	\
	$(DIR)/idtaxon.$(EXT)	$(DIR)/incldbmo.$(EXT)	\
	$(DIR)/inclmo.$(EXT)	$(DIR)/indadr.$(EXT)	\
	$(DIR)/llsecst.$(EXT)	$(DIR)/noiden.$(EXT)	\
	$(DIR)/ooiden.$(EXT)	$(DIR)/oordbref.$(EXT)	\
	$(DIR)/operlay.$(EXT)	$(DIR)/plicmc.$(EXT)	\
	$(DIR)/ploblay.$(EXT)	$(DIR)/periden.$(EXT)	\
	$(DIR)/pldflow.$(EXT)	$(DIR)/plexarr.$(EXT)	\
	$(DIR)/plroots.$(EXT)	$(DIR)/plsche.$(EXT)	\
	$(DIR)/pltlacls.$(EXT)	$(DIR)/pltlcls.$(EXT)	\
	$(DIR)/pltlmcls.$(EXT)	$(DIR)/pltlscls.$(EXT)	\
	$(DIR)/reachabl.$(EXT)	$(DIR)/reldbref.$(EXT)	\
	$(DIR)/rperlay.$(EXT)	$(DIR)/sohlay.$(EXT)	\
	$(DIR)/stridcs.$(EXT)	$(DIR)/stridtyp.$(EXT)	\
	$(DIR)/stridval.$(EXT)	$(DIR)/sttrans.$(EXT)	\
	$(DIR)/suridnty.$(EXT)	$(DIR)/trtime.$(EXT)	\
	$(DIR)/usridnty.$(EXT)
ABSTRACTFIGS=\
	$(DIR)/incldbme.$(EXT)	$(DIR)/inclme.$(EXT)	\
	$(DIR)/rperlaye.$(EXT)	$(DIR)/ploblaye.$(EXT)
BOOKFIGS=$(DIR)/karteic.$(EXT)
DIPLFILES=main.tex	title.tex	intro.tex 	backgrnd.tex \
	identity.tex	arch.tex	cltodb.tex 	soh.tex \
	plob.tex	protocol.tex	perform.tex 	exit.tex \
	bibliogr.tex
DIPLFIGS=$(DIR)/kartei.$(EXT)
ALLFIGS=$(FIGURES) $(ABSTRACTFIGS) $(BOOKFIGS) $(DIPLFIGS)

# LaTeX-Optionen fuer jeden Lauf (z.B. \batchmode):
TEXOPTS=
# Standard-includes fuer alle .tex- und .dgr-Dateien:
STDINCLUDE=$(TEXIDIR)/avb.sty		\
	   $(TEXIDIR)/dipldefs.sty	\
	   $(TEXIDIR)/pretty.cls	\
	   $(TEXIDIR)/timestt.sty	\
	   $(LISPDOCDIR)/crossref.sty

# ----------------------------------------------------------------------
# Flags
# ----------------------------------------------------------------------
DVIPSFLAGS=-D 600

# ----------------------------------------------------------------------
# Targets
# ----------------------------------------------------------------------

all		:: ps
doc		:: all
always		:
	rm -f bericht.dvi $(PSDIR)/bericht.ps buch.dvi $(PSDIR)/buch.ps diplom.dvi $(PSDIR)/diplom.ps; \
	$(MAKE) $(MAKEFLAGS)

# diplom.ps mit garantiert korrektem Inhaltsverzeichnis (LaTeX wird
# ggf. ein zweites Mal aufgerufen):
toc		: bericht.dvi buch.dvi diplom.dvi
	$(MAKE) toc-2

toc-2		: bericht.dvi buch.dvi diplom.dvi
	$(MAKE) ps

# Diplom PostScript-Datei:
ps		: $(PSDIR)/bericht.ps $(PSDIR)/bericht.ps.gz \
		  $(PSDIR)/buch.ps $(PSDIR)/buch.ps.gz \
		  $(PSDIR)/diplom.ps $(PSDIR)/diplom.ps.gz \
		  $(PSDIR)/labstract.ps $(PSDIR)/labstract.ps.gz

$(PSDIR)/bericht.ps $(PSDIR)/diplom.ps	: plinesna.eps
$(PSDIR)/buch.ps			: plinesna.eps

# Abbildungen:
DIR=tenpt
EXT=eps
$(DIR) f figures	:: $(ALLFIGS)
DIR=twelvept
EXT=eps
$(DIR) f figures	:: $(ALLFIGS)

# ----------------------------------------------------------------------
# Abhaengigkeiten
# ----------------------------------------------------------------------

DIR=tenpt
EXT=eps
DVIDEPS=$(STDINCLUDE) $(DIPLFILES) $(FIGURES)
# .dvi-Dateien:
bericht.dvi	: $(DVIDEPS) $(DIPLFIGS)
	@cd bericht; TEXINPUTS="..::$${TEXINPUTS}"; export TEXINPUTS; $(LATEXCMD) bericht; mv $@ ..
diplom.dvi	: $(DVIDEPS) $(DIPLFIGS)
	@cd arbeit; TEXINPUTS="..:$${TEXINPUTS}"; export TEXINPUTS; $(LATEXCMD) diplom; mv $@ ..
labstract.dvi	: $(ABSTRACTFIGS)

testdgr.dvi	: $(STDINCLUDE) $(TEXIDIR)/diagdefs.tex
tdgr		:
	rm -f testdgr.dvi $(PSDIR)/testdgr.ps; \
	$(MAKE) $(MAKEFLAGS) $(PSDIR)/testdgr.ps

DIR=twelvept
EXT=eps
buch.dvi	: $(DVIDEPS) $(BOOKFIGS)
	@cd buch; TEXINPUTS="..:$${TEXINPUTS}"; export TEXINPUTS; $(LATEXCMD) buch; mv $@ ..


DIR=tenpt
EXT=dvi
$(ALLFIGS)	:: $(STDINCLUDE) $(TEXIDIR)/diagdefs.tex
$(DIR) figuresdvi	:: $(ALLFIGS)

DIR=twelvept
$(ALLFIGS)	:: $(STDINCLUDE) $(TEXIDIR)/diagdefs.tex
$(DIR) figuresdvi	:: $(ALLFIGS)

# ----------------------------------------------------------------------
# Standard targets
# ----------------------------------------------------------------------
$(STDTARGETS)	::
	@$(MAKE) default-$@
