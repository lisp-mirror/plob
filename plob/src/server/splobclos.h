/* -------------------------------------------------------------------------
| Module	splobclos.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
 ------------------------------------------------------------------------- */

#include	"plobclos.h"

/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */
extern const char	szInstance []			/* = "instance" */;

extern OBJID		oGlobalClassDescr		/* = NULLOBJID */;
extern OBJID		oGlobalClassSlotDescr		/* = NULLOBJID */;
extern OBJID		oGlobalClassDirSlotDescr	/* = NULLOBJID */;
extern OBJID		oGlobalClassEffSlotDescr	/* = NULLOBJID */;
extern OBJID		oGlobalMethodDescr		/* = NULLOBJID */;

#define			LocateClassDescr( oSelf )	\
((!boundp(oGlobalClassDescr)||				\
  !boundp(oGlobalClassDirSlotDescr)||			\
  !boundp(oGlobalClassEffSlotDescr)||			\
  !boundp(oGlobalMethodDescr))?				\
 (fnLocateClassDescr(oSelf),TRUE):			\
 FALSE)
void			fnLocateClassDescr		( OBJID oSelf );

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
