/* -------------------------------------------------------------------------
| Module	cplobtype.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB client header file:
|		Macros and functions for usage by the PLOB client
 ------------------------------------------------------------------------- */

#include	"plobtype.h"

/* -------------------------------------------------------------------------
| Type tag handling:
 ------------------------------------------------------------------------- */
/* Multi threading restriction: */
extern OBJID		__oTypeTagHeap__	/* = NULLOBJID */;
extern OBJID		__oTypeTagOf__		/* = NULLOBJID */;
extern OBJID		oTypeTagCache		/* = NULLOBJID */;
extern SHTYPETAG	nTypeTagCache		/* = NULLTYPETAG */;

#define	typetagof(oHeap,oSelf)	(__oTypeTagHeap__=(oHeap),		\
				 __oTypeTagOf__=(oSelf),		\
				 (oTypeTagCache!=NULLOBJID&&		\
				  __oTypeTagOf__==oTypeTagCache)?	\
				 nTypeTagCache:				\
				 fnTypeTagOf(__oTypeTagHeap__,__oTypeTagOf__))

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
SHTYPETAG	fnTypeTagOf		( OBJID		oHeap,
					  OBJID		oObjId );


/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
