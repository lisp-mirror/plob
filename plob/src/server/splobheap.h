/* -------------------------------------------------------------------------
| Module	splobheap.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
 ------------------------------------------------------------------------- */

#include	"plobheap.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
#define		make_heap(user,machine,description) \
fnMakeHeap(user,machine,description)

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
OBJID		fnMakeHeap		( OBJID	oUser,
					  OBJID	oMachine,
					  OBJID	oDescription );
OBJID		fnHeapUser		( OBJID oHeap );
OBJID		fnHeapMachine		( OBJID oHeap );
OBJID		fnHeapDescription	( OBJID oHeap );

TRACTID		fnTransactionBegin	( OBJID	oHeap,
					  BOOL	bIgnoreError );
TRACTID		fnTransactionCancel	( OBJID	oHeap,
					  BOOL	bIgnoreError );
TRACTID		fnTransactionEnd	( OBJID	oHeap,
					  BOOL	bIgnoreError );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
