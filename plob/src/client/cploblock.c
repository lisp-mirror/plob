/* -------------------------------------------------------------------------
| Module	cploblock.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB client source code.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobnumber.h"
#include	"cplobroot.h"
#include	"cploblock.h"
#include	"cplobbtree.h"
#include	"cplobheap.h"
#include	"cplobsequ.h"
#include	"cplobclos.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeLockModule		( void )
{
  PROCEDURE	( fnInitializeLockModule );

  RETURN ( VOID );
} /* fnInitializeLockModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeLockModule	( void )
{
  PROCEDURE	( fnDeinitializeLockModule );

  RETURN ( VOID );
} /* fnDeinitializeLockModule */

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
  SHLOCK	nLockOld;
  HPEEK		hPeek = (HPEEK) NULLHPEEK;
  FIXNUM	nObjIdWords = 0;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    nLockOld	= fnCacheLockP ( oShortObjIdLockBy, oShortToLock, nLock );
    if ( (int) nLockOld < 0 ) {
      /* The lock into the client's cache failed, so ask the server to
         lock the object: */
      nLockOld	=
	fnServerTransactionLockInsert ( oShortObjIdHeap, oShortObjIdLockBy,
					(SHLOCK) ( (unsigned int) nLock |
						   (unsigned int)
						   eshLockPeek ),
					oShortToLock, nTypeTagToLock, nIndex,
					(FIXNUM *) &hPeek, &nObjIdWords );
      if ( (int) nLockOld >= 0 ) {
	if ( hPeek != NULLHPEEK ) {
	  fnCacheInsert ( oShortObjIdHeap, oShortObjIdLockBy,
			  hPeek, nObjIdWords );
	}
	fnCacheInsertLock ( oShortObjIdLockBy, oShortToLock, nLock );
      }
    }
  } else {
    nLockOld	=
      fnServerTransactionLockInsert ( oShortObjIdHeap, oShortObjIdLockBy,
				      nLock, oShortToLock, nTypeTagToLock,
				      nIndex,
				      (FIXNUM *) &hPeek, &nObjIdWords );
  }
  RETURN ( nLockOld );
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
  SHLOCK	nLockOld;
  HPEEK		hPeek = (HPEEK) NULLHPEEK;
  FIXNUM	nObjIdWords = 0;

  INITIALIZEPLOB;

  if ( fnCacheGetHeap ( oShortObjIdLockBy ) == NULL ) {
    RETURN ( eshGeneralError );
  }
  nLockOld		=
    fnServerTransactionLockSet ( oShortObjIdHeap, oShortObjIdLockBy,
				 nLock, oShortToLock, nTypeTagToLock,
				 nIndex, (FIXNUM *) &hPeek,
				 &nObjIdWords );
  if ( bGlobalDoCaching && (int) nLockOld >= 0 && hPeek != NULLHPEEK ) {
    fnCacheInsert ( oShortObjIdHeap, oShortObjIdLockBy,
		    hPeek, nObjIdWords );
  }
  RETURN ( nLockOld );
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
  BOOL		bCached;
  SHLOCK	nLockLevel, nLockOld, nVectorLockNow;

  INITIALIZEPLOB;

  if ( fnCacheGetHeap ( oShortObjIdLockedBy ) == NULL ) {
    RETURN ( (SHLOCK) eshGeneralError );
  }
  nLockLevel	= (SHLOCK)
    ( (unsigned int) nLock & (unsigned int) eshLockLevelMask );
  bCached	= (BOOL)
    ( bGlobalDoCaching && nLockLevel >= eshLockLevelVector );
  if ( bCached ) {
    /* The vector lock could be removed completely, so flush the
       object before the unlock will be done: */
    fnCacheFlush  ( oShortObjIdLockedBy, ( nLockLevel > eshLockLevelVector ) ?
		    NULLOBJID : oShortToUnlock );
  }
  nLockOld	=
    fnServerTransactionUnlock ( oShortObjIdHeap, oShortObjIdLockedBy,
				nLock, oShortToUnlock, nIndex,
				&nVectorLockNow );
  if ( bCached && nLockOld >= 0 && ! ( nVectorLockNow & eshLockModeMask ) ) {
    /* The lock was removed completely; remove the object from
       the cache: */
    fnCacheDelete ( oShortObjIdLockedBy, ( nLockLevel > eshLockLevelVector ) ?
		    NULLOBJID : oShortToUnlock );
  }
  RETURN ( nLockOld );
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
  FIXNUM	nReleased;

  INITIALIZEPLOB;

  if ( fnCacheGetHeap ( oShortObjIdLockedBy ) == NULL ) {
    RETURN ( eshGeneralError );
  }
  if ( bGlobalDoCaching ) {
    /* All locks will be removed completely, so flush the object
       before the unlock will be done: */
    fnCacheFlush  ( oShortObjIdLockedBy, oShortToUnlock );
  }
  nReleased	=
    fnServerTransactionUnlockAll ( oShortObjIdHeap, oShortObjIdLockedBy,
				   oShortToUnlock );
  if ( bGlobalDoCaching ) {
    fnCacheDelete ( oShortObjIdLockedBy, oShortToUnlock );
  }
  RETURN ( nReleased );
} EndFunction ( fnShortUnlockAll );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnShortUnlockAllAll, "c-sh-unlock-all-all",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock ) ) )
{
  FIXNUM	nReleased;

  INITIALIZEPLOB;

  if ( fnCacheGetHeap ( oShortObjIdHeap ) == NULL ) {
    RETURN ( eshGeneralError );
  }
  fnCacheGetHeap ( oShortObjIdHeap );
  if ( bGlobalDoCaching ) {
    /* All locks will be removed completely, so flush the object
       before the unlock will be done: */
    fnCacheFlush  ( NULLOBJID, oShortToUnlock );
  }
  nReleased	=
    fnServerTransactionUnlockAllAll ( oShortObjIdHeap, oShortToUnlock );
  if ( bGlobalDoCaching ) {
    fnCacheDelete ( NULLOBJID, oShortToUnlock );
  }
  RETURN ( nReleased );
} EndFunction ( fnShortUnlockAllAll );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
