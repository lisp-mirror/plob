/* -------------------------------------------------------------------------
| Module	cploblock.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB client source code.
|
| Copyright	PLOB! Copyright 1994--2002 Heiko Kirschke.
|		All rights reserved.
|
| Unlimited use, reproduction, modification and distribution of this
| software is permitted.  Any copy or modified version of this
| software must include both the above copyright notice of
| Heiko Kirschke and this paragraph; for each modified version, an
| additional statement must be added telling the year of modification
| and quoting the author of the modification.  Any distribution of
| this software must comply with all applicable German export control
| laws.  This software is made available AS IS, and HEIKO KIRSCHKE
| DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT
| LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
| A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION
| CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM THE
| SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
| CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
| HEIKO KIRSCHKE IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
|
| Please note that these license terms adhere only to the code of
| PLOB!  itself. PLOB! uses POSTORE (Persistent Object Store) as a
| low-level persistent memory; it is provided in binary form within
| PLOB! with the permission of the University of St. Andrews
| (http://www-ppg.dcs.st-andrews.ac.uk/Default.html).  Contact the
| University of St. Andrews for getting their license terms on
| POSTORE.
|
| $Header$
|
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
