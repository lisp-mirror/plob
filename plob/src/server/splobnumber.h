/* -------------------------------------------------------------------------
| Module	splobnumber.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/11/07
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
 ------------------------------------------------------------------------- */

#include	"plobnumber.h"

/* -------------------------------------------------------------------------
| Comparing of numeric objects
 ------------------------------------------------------------------------- */
COMPARETAG		fnNumberEqual	( OBJID oSelf,
					  SHTYPETAG nTypeTagSelf,
					  OBJID oCompare );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
