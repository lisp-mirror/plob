/* -------------------------------------------------------------------------
| Module	splobmisc.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
 ------------------------------------------------------------------------- */

#include	"plobmisc.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
extern FLUSHMODE	nFlushModeCache		/* = flushGet */;
#define			GetFlushMode(heap)	\
((nFlushModeCache==flushGet)?fnFlushMode(heap,flushGet):nFlushModeCache)

/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
/* Store an error level and message: */
void		fnStoreError			( ERRLVL nErrorLevel,
						  LPCSTR lpszErrorMsg );
/* Store an error level and message if the passed level is greater
   than the last level encountered: */
BOOL		fnStoreErrorIf			( ERRLVL nErrorLevel,
						  LPCSTR lpszErrorMsg );
/* Dummy callbacks for RPC server: */
BOOL		fnLISPmapClassInfoCallback	( SHTYPETAG nTypeTag,
						  LPCSTR lpszTypeName,
						  int nObjIdSize,
						  int nValueSize );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
