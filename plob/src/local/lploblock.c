/* -------------------------------------------------------------------------
| Module	lploblock.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB local source code.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobintern.h"
#include	"lplobmisc.h"
#include	"lplobtype.h"
#include	"lplobnumber.h"
#include	"lplobroot.h"
#include	"lploblock.h"
#include	"lplobbtree.h"
#include	"lplobheap.h"
#include	"lplobsequ.h"
#include	"lplobclos.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientTransactionLockInsert, "c-sh-insert-lock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		  and
		  argument ( SHLOCK, value_in, nLock )
		  and
		  argument ( SHORTOBJID, value_in, oShortToLock )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagToLock )
		  and
		  argument ( FIXNUM, value_in, nIndex ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionLockInsert ( oShortObjIdHeap, oShortObjIdLockBy,
					   nLock, oShortToLock,
					   nTypeTagToLock, nIndex,
					   (FIXNUM *) NULL,
					   (FIXNUM *) NULL ) );
} EndFunction ( fnClientTransactionLockInsert );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientTransactionLockSet, "c-sh-set-lock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		  and
		  argument ( SHLOCK, value_in, nLock )
		  and
		  argument ( SHORTOBJID, value_in, oShortToLock )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagToLock )
		  and
		  argument ( FIXNUM, value_in, nIndex ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionLockSet ( oShortObjIdHeap, oShortObjIdLockBy,
					nLock, oShortToLock, nTypeTagToLock,
					nIndex,
					(FIXNUM *) NULL, (FIXNUM *) NULL ) );
} EndFunction ( fnClientTransactionLockSet );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnShortUnlock, "c-sh-unlock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		  and
		  argument ( SHLOCK, value_in, nLock )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock )
		  and
		  argument ( FIXNUM, value_in, nIndex ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionUnlock ( oShortObjIdHeap, oShortObjIdLockedBy,
			nLock, oShortToUnlock, nIndex, (SHLOCK *) NULL ) );
} EndFunction ( fnShortUnlock );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnShortUnlockAll, "c-sh-unlock-all",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionUnlockAll ( oShortObjIdHeap, oShortObjIdLockedBy,
			   oShortToUnlock ) );
} EndFunction ( fnShortUnlockAll );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnShortUnlockAllAll, "c-sh-unlock-all-all",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionUnlockAllAll ( oShortObjIdHeap, oShortToUnlock ) );
} EndFunction ( fnShortUnlockAllAll );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
