# ----------------------------------------------------------------------
# Makefile for LispWorks object processing files
# 27.10.1993 Heiko Kirschke, Fachbereich Informatik, Uni Hamburg
# e-mail: kirschke@kogs26.informatik.uni-hamburg.de
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Macros
# ----------------------------------------------------------------------
LIBRARY=c-postore.a
OBJECTS=$(LIBRARY)(_global.o)	$(LIBRARY)(_hash.o) \
    	$(LIBRARY)(lwobject.o)	$(LIBRARY)(c-postore.o)
STDINCLUDE=_global.h
LDLIBS=$(LIBRARY)

# ----------------------------------------------------------------------
# Standard rules
# ----------------------------------------------------------------------
%.a:%.o
	ranlib $@

$(LIBRARY)(%.o):%.o
	$(AR) $(ARFLAGS) $@ $%

# ----------------------------------------------------------------------
# Main target
# ----------------------------------------------------------------------
all:$(LIBRARY)
# test: $(LIBRARY) tst

# ----------------------------------------------------------------------
# Test target
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Sub targets
# ----------------------------------------------------------------------
$(LIBRARY)	: $(OBJECTS)

_global.o	: $(STDINCLUDE)
_hash.o		: _hash.h $(STDINCLUDE)
lwobject.o	: lwobject.h _hash.h $(STDINCLUDE)
c-postore.o	: c-postore.h lwobject.h $(STDINCLUDE)
