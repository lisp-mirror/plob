/* -------------------------------------------------------------------------
| Module	sploblock.c
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Copyright	(C) 1993,1994 Heiko Kirschke
| Date		25.1.94 Derived from c-plob-heap.c
| Description	Multi level locks.
|		The algorithms used here are in part derived from
|		POSTGRES sources version 4.0.1, POSTGRES modules
|		  postgres/src/backend/storage/lock.h
|		  postgres/src/backend/storage/lmgr.h
|		  postgres/src/backend/storage/multilev.h
|		  postgres/src/backend/storage/lmgr/lock.c
|		  postgres/src/backend/storage/lmgr/multi.c
|		The algorithms 'borrowed' from POSTGRES are marked with a
|		comment.
|		Some notes:
|		Detected lock conflicts can't be resolved here because this
|		would mean to set the calling process to sleep; especially
|		there would be no way for the calling process to wake up
|		again and to unlock the objects which caused the conflict.
|		This arises from the fact that PLOB is called from LISP
|		'threads', which are integrated into one UNIX proccess;
|		the solution would be to write an own process handling
|		integrated into LISP ... (or to look if the LISP system
|		into which PLOB is embedded has the possibility to set
|		its 'threads' to sleep and wake them up again).
|		1996/09/24: Actually, that is possible with LispWorks
|		light-weight processes.
|		1996/11/08: Actually, this is no longer possible because
|		of the missing callbacks from the server to the client
|		in the RPC version. Suspending is done by a busy wait.
|
| Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
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
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobtype.h"
#include	"splobnumber.h"
#include	"sploblock.h"
#include	"splobbtree.h"
#include	"splobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
#define	LOGGING	0x00	/* 0 (no), 1 (lock) or 4 (peek) (messages) */

/* -------------------------------------------------------------------------
| Total lock info per locked element resp. sh-vector resp. store
 ------------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lock.h */
typedef enum {
  llNothing	= -1,
  llMin,
  llElement	= llMin,
  llVector,
  llStore,
  llMax		= llStore,
  llNumberOf	= llMax + 1
}	LOCKLEVEL, FAR * LPLOCKLEVEL;

/* -------------------------------------------------------------------------
| Static types
 ------------------------------------------------------------------------- */
/* LOCKMASK is a bit mask whose values correspond to LOCKMODE, i.e.
   (LOCKMODE) 0 == lmRead       ==> (LOCKMASK) 0x01
   (LOCKMODE) 1 == lmWrite      ==> (LOCKMASK) 0x02
   (LOCKMODE) 2 == lmReadIntent ==> (LOCKMASK) 0x04
   ...
   (LOCKMODE) n == lm...        ==> (LOCKMASK) 2^n
*/
typedef int	LOCKMASK, FAR * LPLOCKMASK;

/* -------------------------------------------------------------------------
| Mappings LOCKLEVEL resp. LOCKMODE -> strings, SHLOCK, etc.
 ------------------------------------------------------------------------- */
#define			SZLEVELNOTHING		"nothing"
static const char	szLevelNothing []	= SZLEVELNOTHING;

#define			SZMODENOTHING		"nothing"
static const char	szModeNothing []	= SZMODENOTHING;

static const LPCSTR	ppszLevels [ /* llNumberOf */ ]	= {
  "element",		/* llElement */
  "vector",		/* llVector */
  "store"		/* llStore */
};

static const LPCSTR	ppszModes [ /* lmNumberOf */ ]	= {
  "ro",			/* lmReadOnly */
  "r",			/* lmRead */
  "w",			/* lmWrite */
  "roi",		/* lmReadOnlyIntent */
  "ri",			/* lmReadIntent */
  "wi"			/* lmWriteIntent */
};

static const int	nPriorities [ /* lmNumberOf */ ]	= {
  2,			/* lmReadOnly */
  4,			/* lmRead */
  6,			/* lmWrite */
  1,			/* lmReadOnlyIntent */
  3,			/* lmReadIntent */
  5			/* lmWriteIntent */
};

static const SHLOCK	Level2SHlock [ /* llNumberOf */ ]	= {
  eshLockLevelElement,
  eshLockLevelVector,
  eshLockLevelStore
};

static const SHLOCK	Mode2SHlock [ /* lmNumberOf */ ]	= {
  eshLockModeReadOnly,
  eshLockModeRead,
  eshLockModeWrite,
  eshLockModeReadOnlyIntent,
  eshLockModeReadIntent,
  eshLockModeWriteIntent
};

/* The error codes to return for lock conflicts: */
static const int	Level2Conflict [ /* llNumberOf */ ]	= {
  eshLockConflictElement,
  eshLockConflictVector,
  eshLockConflictStore
};

/* -------------------------------------------------------------------------
| Static variables
 ------------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lmgr/multi.c */
#if defined(__GNUC__) || 1
#define		BITSON( i )	(1<<(i))
#define		BITSOFF( i )	(~BITSON(i))
#else
#error Please read comment following this error message.
/* ... and comment out the above error pragma if appropiate.

   GNU C can handle bit shifts with the shift amount being NOT a constant
   value, e.g. the C code  { int i = foo (), n = 1 << i; } is accepted by
   the GNU compiler and gives the expected result.
   If your compiler does so too, add to the line beginning with
     #if defined(__GNUC__)
   above a compiler specific
     || defined(<your's compiler specific define>)
   Otherwise, just comment out the above error pragma and recompile.
*/
#define		BITSON( i )	BitsOn [ i ]
#define		BITSOFF( i )	BitsOff [ i ]
#endif

/* This is to simplify/speed up some bit arithmetic */
static LOCKMASK	BitsOn		[ lmNumberOf ];
static LOCKMASK	BitsOff		[ lmNumberOf ];

static LOCKMASK	MultiConflicts	[ lmNumberOf ];

/* -------------------------------------------------------------------------
| Static function declarations
 ------------------------------------------------------------------------- */

static BOOL		fnInitLockHeader	( OBJID oObjId,
						  LPLOCKHEADER lpHeader,
						  LPCLASSINFO lpClassInfo );
static BOOL		mfnInitSumLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static int		fnAddModeString		( LPSTR lpszBuffer,
						  size_t nBuffer,
						  int nIndex,
						  LOCKMODE nLockMode,
						  int nLockCount );
static LOCKLEVEL	fnSumLockLevel		( LPSUMLOCK lpSumLock );
static LPSTR		mfnPrintSumLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static BOOL		mfnInitOneLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnPrintOneLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static BOOL		mfnInitQueueEntry	( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSUMLOCK	fnGetSumLock		( OBJID oSelf );
static LPONELOCK	fnGetOneLock		( OBJID oSelf );
static LPQUEUENTRY	fnGetQueueEntry		( OBJID oSelf );
static BOOL		fnSHlock2LevelMode	( SHLOCK nLock,
						  LPLOCKLEVEL pnLockLevel,
						  LPLOCKMODE pnLockMode,
						  LPBOOL lpbUnlock,
						  LPBOOL lpbForce );
static SHLOCK		fnLevelMode2SHlock	( LOCKLEVEL nLockLevel,
						  LOCKMODE nLockMode );
static LPSUMLOCK	fnInsertVectorLock	( OBJID oSelf );
static LPSUMLOCK	fnSearchElementLock	( OBJID oToSearch,
						  int nIndex,
						  LPOBJID FAR * ppoLast,
						  BOOL bInsert );
static LPSUMLOCK	fnSearchVectorLock	( OBJID oToSearch,
						  BOOL bInsert );
static LPSUMLOCK	fnSearchStoreLock	( BOOL bInsert );
static LPSUMLOCK	fnSearchSumLock		( LOCKLEVEL nLockLevel,
						  OBJID oToSearch,
						  int nIndex,
						  LPOBJID FAR * ppoLast,
						  BOOL bInsert );
static LPONELOCK	fnSearchOneLock		( LPSUMLOCK lpSumLock,
						  OBJID oToSearch,
						  LPOBJID FAR * ppoLast,
						  BOOL bInsert );
static SHLOCK		fnGetCurrentSHlock	( LPONELOCK lpOneLock,
						  LOCKLEVEL nLockLevel );
static int		fnIncHolds		( LPLOCKHEADER lpHeader,
						  LOCKMODE nLockMode,
						  int nIncrement );
static void		fnLockGrant		( LPSUMLOCK lpSumLock,
						  LOCKMODE nLockMode );
static int		fnResolveConflicts	( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  LOCKMODE nLockMode );
static int		fnLockAcquire		( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  LOCKMODE nLockMode );
static SHLOCK		fnGetLockOfLevel	( OBJID oLockedByP,
						  LOCKLEVEL nLockLevel,
						  OBJID oLockedP,
						  int nIndexP );
static SHLOCK		fnSetLockByLevelMode	( OBJID oLockBy,
						  LOCKLEVEL nLockLevel,
						  LOCKMODE nLockMode,
						  BOOL bForce,
						  OBJID oToLock,
						  int nIndex,
						  SHLOCK * pnVectorLockNow );
static void		fnDeleteOneLock		( LPONELOCK lpOneLock,
						  LPOBJID lpoLast );
static void		fnDeleteElementLock	( OBJID oToUnlock,
						  LPOBJID lpoLast );
static void		fnDeleteVectorLock	( OBJID oToUnlock );
static void		fnDeleteStoreLock	( void );
static void		fnDeleteSumLock		( LOCKLEVEL nLockLevel,
						  OBJID oToUnlock,
						  LPOBJID lpoLast );
static int		fnLockRelease		( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  OBJID oLockedBy,
						  LOCKMODE nLockMode,
						  OBJID oToUnlock,
						  LOCKLEVEL nLockLevel,
						  LPOBJID lpoSumLast,
						  LPOBJID lpoOneLast );
static int		fnLockReleaseAll	( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  OBJID oLockedBy,
						  OBJID oToUnlock,
						  LOCKLEVEL nLockLevel,
						  LPOBJID lpoSumLast,
						  LPOBJID lpoOneLast );

static int		fnLockWait		( OBJID oSumLock,
						  OBJID oOneLock,
						  LOCKMODE nLockMode );
static int		fnLockWakeup		( OBJID oSumLock );
static LPQUEUENTRY	fnLockEnqueue		( OBJID oSumLock,
						  OBJID oOneLock,
						  LOCKMODE nLockMode );
static void		fnLockDequeue		( OBJID oSumLock,
						  OBJID oOneLock );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeLockModule		( void )
{
  int		i, nBit;

  PROCEDURE	( fnInitializeLockModule );

  /* If this assert fails, the number of LOCKLEVELs has changed but this
     wasn't reflected to array ppszLevels: */
  ASSERT ( length ( ppszLevels ) == llNumberOf );

  /* If this assert fails, the number of LOCKMODEs has changed but this
     wasn't reflected to array ppszModes: */
  ASSERT ( length ( ppszModes ) == lmNumberOf );
  ASSERT ( length ( nPriorities ) == lmNumberOf );

  /* If this assert fails, the number of LOCKLEVELs has changed but this
     wasn't reflected to array Level2SHlock: */
  ASSERT ( length ( Level2SHlock ) == llNumberOf );

  /* If this assert fails, the number of LOCKMODEs has changed but this
     wasn't reflected to array Mode2SHlock: */
  ASSERT ( length ( Mode2SHlock ) == lmNumberOf );

  /* If this assert fails, the number of LOCKLEVELs has changed but this
     wasn't reflected to array Level2Conflict: */
  ASSERT ( length ( Level2Conflict ) == llNumberOf );

  RegisterMethod ( eshSumLockTag, gfnInitializeInstance, mfnInitSumLock );
  RegisterMethod ( eshSumLockTag, gfnPrintObjectDetails, mfnPrintSumLock );

  RegisterMethod ( eshOneLockTag, gfnInitializeInstance, mfnInitOneLock );
  RegisterMethod ( eshOneLockTag, gfnPrintObjectDetails, mfnPrintOneLock );

  RegisterMethod ( eshQueueEntryTag, gfnInitializeInstance,
		   mfnInitQueueEntry );

  for ( i = 0, nBit = 1; i < lmNumberOf; i++, nBit <<= 1 ) {
    BitsOn  [ i ]	= nBit;
    BitsOff [ i ]	= ~nBit;
  }

  /* Fill the MultiConflicts table: */
  memset ( MultiConflicts, 0, sizeof ( MultiConflicts ) );

  /* Conflicts for write locks:
     Almost all reads and writes at any level conflict with a write
     lock. It doesn't conflict with read-only-intent because a
     read-only lock can be placed onto an object being currently
     write-locked: */
  MultiConflicts [ lmWrite ]		=
    BITSON ( lmWrite ) | BITSON ( lmWriteIntent ) |
      BITSON ( lmRead ) | BITSON ( lmReadIntent ) |
	BITSON ( lmReadOnly );

  /* Conflicts for write intent locks.
     1997/12/15 HK: They must conflict with read-only locks, too: */
  MultiConflicts [ lmWriteIntent ]	=
    BITSON ( lmRead ) | BITSON ( lmWrite ) | BITSON ( lmReadOnly );

  /* Conflicts for read locks:
     read locks conflict with write locks at curr and lower levels */
  MultiConflicts [ lmRead ]		=
    BITSON ( lmWrite ) | BITSON ( lmWriteIntent );

  /* Conflicts for read intent locks */
  MultiConflicts [ lmReadIntent ]	=
    BITSON ( lmWrite );

  /* Conflicts for read-only locks: read-only locks conflict with
     write locks at curr and lower levels.
     HK 9.2.94: Read-only locks don't conflict with any lock; this has
     the nice effect that a write-locked object can be marked as
     read-only; the read-only-lock prevents further write locks onto
     the object. */
  MultiConflicts [ lmReadOnly ]		=
    /* BITSON ( lmWrite ) */ 0;

  /* Conflicts for read-only intent locks */
  MultiConflicts [ lmReadOnlyIntent ]	=
    0;

  RETURN ( VOID );
} /* fnInitializeLockModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeLockModule	( void )
{
  PROCEDURE	( fnDeinitializeLockModule );

  RETURN ( VOID );
} /* fnDeinitializeLockModule */

/* -------------------------------------------------------------------------
| Initialization methods
 ------------------------------------------------------------------------- */
static BOOL		fnInitLockHeader	( OBJID oObjId,
						  LPLOCKHEADER lpHeader,
						  LPCLASSINFO lpClassInfo )
{
  register int		i, n;
  register LPOBJID	lpoTotalHold;

  PROCEDURE	 	( fnInitLockHeader );

  lpHeader->oSelf		= oObjId;
  lpHeader->onTotalHold		= o0;
  for ( i = 0, n = length ( lpHeader->oTotalHolds ),
        lpoTotalHold = lpHeader->oTotalHolds; i < n; i++, lpoTotalHold++ ) {
    *lpoTotalHold	= o0;
  }

  RETURN ( TRUE );
} /* fnInitLockHeader */

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitSumLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  register int		i, n;
  register LPOBJID	lpoActiveHold;

  PROCEDURE		( mfnInitSumLock );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  fnInitLockHeader ( oObjId, & ((LPSUMLOCK)lpSHvector)->Header,
		     lpClassInfo );
  ((LPSUMLOCK)lpSHvector)->onLockMask	= o0;
  ((LPSUMLOCK)lpSHvector)->onActiveHold	= o0;
  for ( i = 0, n = length ( ((LPSUMLOCK)lpSHvector)->oActiveHolds ),
        lpoActiveHold = ((LPSUMLOCK)lpSHvector)->oActiveHolds;
        i < n; i++, lpoActiveHold++ ) {
    *lpoActiveHold	= o0;
  }

  RETURN ( TRUE );
} /* mfnInitSumLock */

/* ----------------------------------------------------------------------- */
static int		fnAddModeString		( LPSTR lpszBuffer,
						  size_t nBuffer,
						  int nIndex,
						  LOCKMODE nLockMode,
						  int nLockCount )
{
  int		nLength, i;
  char		szBuffer [ 80 ];

  PROCEDURE	( fnAddModeString );

  nLength	= 0;
  if ( nLockCount > 0 ) {
    i		= 0;
    if ( nIndex > 0 )
      szBuffer [ i++ ]	= '|';
    if ( nLockCount > 1 ) {
      sprintf ( & szBuffer [ i ], "%d", nLockCount );
      i	+= strlen ( & szBuffer [ i ] );
    }
    strcpy ( & szBuffer [ i ], ppszModes [ nLockMode ] );
    strncpy ( & lpszBuffer [ nIndex ], szBuffer, nBuffer );
    nLength	= strlen ( szBuffer );
  }
  RETURN ( nIndex + nLength );
} /* fnAddModeString */

/* ----------------------------------------------------------------------- */
static LOCKLEVEL	fnSumLockLevel		( LPSUMLOCK lpSumLock )
{
  LOCKLEVEL	l;

  PROCEDURE	( fnSumLockLevel );

  l	= llNothing;
  if ( lpSumLock ) {
    if ( boundp ( lpSumLock->onIndex ) ) {
      l	= llElement;
    } else if ( lpSumLock->Header.oSelf == GetRootLock () ) {
      l	= llStore;
    } else {
      l	= llVector;
    }
  }
  RETURN ( l );
} /* fnSumLockLevel */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintSumLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  static const char	szRefArrow []	= "->";

  int			i;
  LOCKMODE		m;
  LOCKLEVEL		l;

  PROCEDURE		( mfnPrintSumLock );

  if ( nBuffer> 16 ) {
    l	= fnSumLockLevel ( (LPSUMLOCK) lpSHvector );
    if ( l == llElement ) {
      sprintf ( lpszBuffer, "%s=%d",
	        ppszLevels [ l ],
	        OBJID2FIXNUM ( ((LPSUMLOCK)lpSHvector)->onIndex ) );
    } else {
      strcpy ( lpszBuffer, ppszLevels [ l ] );
    }
    i			= strlen ( lpszBuffer );
    lpszBuffer [ i++ ]	= ' ';
    nBuffer		-= i;
    lpszBuffer		+= i;
    i			= 0;
    *lpszBuffer		= '\0';
    for ( m = lmMin; m <= lmMax; m = (LOCKMODE) ( (int) m + 1 ) ) {
      i	= fnAddModeString ( lpszBuffer, nBuffer, i, m,
			    OBJID2FIXNUM ( ((LPSUMLOCK)lpSHvector)->
					   Header.oTotalHolds [ m ] ) );
    }
    if ( i + sizeof ( szRefArrow ) < nBuffer && l != llStore ) {
      if ( i > 0 )
	lpszBuffer [ i++ ]	= ' ';
      strcpy ( & lpszBuffer [ i ], szRefArrow );
      i	+= sizeof ( szRefArrow ) - 1;
      fnPrintObject ( ((LPSUMLOCK)lpSHvector)->Header.OfPlob.oLockedBy,
		      & lpszBuffer [ i ], nBuffer - i );
    }
  }
  RETURN ( lpszBuffer );
} /* mfnPrintSumLock */

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitOneLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  register int	i;

  PROCEDURE	( mfnInitOneLock );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  fnInitLockHeader ( oObjId, & ((LPONELOCK)lpSHvector)->Header,
		     lpClassInfo );
  RETURN ( TRUE );
} /* mfnInitOneLock */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintOneLock		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  static const char	szRefArrow []	= "<-";

  int			i;
  LOCKMODE		m;

  PROCEDURE	( mfnPrintOneLock );

  i	= 0;
  for ( m = lmMin; m <= lmMax; m = (LOCKMODE) ( (int) m + 1 ) ) {
    i	= fnAddModeString ( lpszBuffer, nBuffer, i, m,
			    OBJID2FIXNUM ( ((LPONELOCK)lpSHvector)->
					   Header.oTotalHolds [ m ] ) );
  }
  if ( i + sizeof ( szRefArrow ) < nBuffer ) {
    if ( i > 0 )
      lpszBuffer [ i++ ]	= ' ';
    strcpy ( & lpszBuffer [ i ], szRefArrow );
    i	+= sizeof ( szRefArrow ) - 1;
    fnPrintObject ( ((LPONELOCK)lpSHvector)->Header.OfPlob.oLockedBy,
		    & lpszBuffer [ i ], nBuffer - i );
  }
  RETURN ( lpszBuffer );
} /* mfnPrintOneLock */

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitQueueEntry	( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitOneLock );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  ((LPQUEUENTRY)lpSHvector)->oSelf	= oObjId;
  ((LPQUEUENTRY)lpSHvector)->onPriority	= o0;
  ((LPQUEUENTRY)lpSHvector)->onLockMode	= Fixnum2ObjId ( lmNothing );

  RETURN ( TRUE );
} /* mfnInitQueueEntry */

/* -------------------------------------------------------------------------
| Accessors
 ------------------------------------------------------------------------- */
static LPSUMLOCK	fnGetSumLock		( OBJID oSelf )
{
  LPSUMLOCK	lpSumLock;

  PROCEDURE	( fnGetSumLock );

  lpSumLock	= (LPSUMLOCK) NULL;
  if ( boundp ( oSelf ) ) {
    lpSumLock	= (LPSUMLOCK) SH_key_to_address ( oSelf );
    ASSERT ( lpSumLock );
    if ( ! ASSERT_TYPE ( oSelf, lpSumLock, eshSumLockTag ) )
      lpSumLock	= (LPSUMLOCK) NULL;
  }
  RETURN ( lpSumLock );
} /* fnGetSumLock */

/* ----------------------------------------------------------------------- */
#pragma inline ( fnGetOneLock )
static
#if WIN32
__inline
#elif defined(__GNUC__)
__inline__
#endif
LPONELOCK		fnGetOneLock		( OBJID oSelf )
{
  LPONELOCK	lpOneLock;

  PROCEDURE	( fnGetOneLock );

  lpOneLock	= (LPONELOCK) NULL;
  if ( boundp ( oSelf ) ) {
    lpOneLock	= (LPONELOCK) SH_key_to_address ( oSelf );
    ASSERT ( lpOneLock != NULL );
    if ( ! ASSERT_TYPE ( oSelf, lpOneLock, eshOneLockTag ) ) {
      lpOneLock	= (LPONELOCK) NULL;
    }
  }
  RETURN ( lpOneLock );
} /* fnGetOneLock */

/* ----------------------------------------------------------------------- */
static LPQUEUENTRY	fnGetQueueEntry		( OBJID oSelf )
{
  LPQUEUENTRY	lpQueueEntry;

  PROCEDURE	( fnGetQueueEntry );

  lpQueueEntry	= (LPQUEUENTRY) NULL;
  if ( boundp ( oSelf ) ) {
    lpQueueEntry	= (LPQUEUENTRY) SH_key_to_address ( oSelf );
    ASSERT ( lpQueueEntry );
    if ( ! ASSERT_TYPE ( oSelf, lpQueueEntry, eshQueueEntryTag ) )
      lpQueueEntry	= (LPQUEUENTRY) NULL;
  }
  RETURN ( lpQueueEntry );
} /* fnGetQueueEntry */

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
static BOOL		fnSHlock2LevelMode	( SHLOCK nLock,
						  LPLOCKLEVEL pnLockLevel,
						  LPLOCKMODE pnLockMode,
						  LPBOOL lpbUnlock,
						  LPBOOL lpbForce )
{
  LOCKLEVEL		nLockLevel;
  register LOCKLEVEL	l;
  LOCKMODE		nLockMode;
  register LOCKMODE	m;
  BOOL			bUnlock, bForce, bDone;
  int			nItem;

  PROCEDURE	( fnSHlock2LevelMode );

  bDone		= TRUE;

  if ( ! pnLockLevel )
    pnLockLevel	= &nLockLevel;
  if ( ! pnLockMode )
    pnLockMode		= &nLockMode;
  if ( ! lpbUnlock )
    lpbUnlock		= &bUnlock;
  if ( ! lpbForce )
    lpbForce		= &bForce;

  /* 'Parsing' of nLock: */
  nItem			= nLock & eshLockLevelMask;
  if ( nItem == eshLockLevelNothing ) {
    *pnLockLevel	= llNothing;
  } else {
    for ( l = llMin; l <= llMax; l = (LOCKLEVEL) ( (int) l + 1 ) ) {
      if ( Level2SHlock [ l ] == nItem ) {
	*pnLockLevel	= l;
	break;
      }
    }
    if ( l > llMax ) {
      /* Unknown lock level requested: */
      *pnLockLevel	= llVector;
      bDone		= FALSE;
    }
  }

  nItem			= nLock & eshLockModeMask;
  if ( nItem == eshLockModeNothing ) {
    *pnLockMode	= lmNothing;
  } else {
    if ( nItem == ( eshLockModeRead | eshLockModeWrite ) )
      /* Handle read/write like write: */
      nItem	= eshLockModeWrite;
    /* Check only till lmIntent, because no explicit setting of intent-locks
       is allowed: */
    for ( m = lmMin; m < lmIntent; m = (LOCKMODE) ( (int) m + 1 ) ) {
      if ( Mode2SHlock [ m ] == nItem ) {
	*pnLockMode	= m;
	break;
      }
    }
    if ( m >= lmIntent ) {
      /* Unknown lock mode requested: */
      *pnLockMode	= lmNothing;
      bDone		= FALSE;
    }
  }

  *lpbUnlock		= (BOOL) ( ( nLock & eshUnlock ) != 0 );
  *lpbForce		= (BOOL) ( ( nLock & eshLockForce ) != 0 );

  RETURN ( bDone );
} /* fnSHlock2LevelMode */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnLevelMode2SHlock	( LOCKLEVEL nLockLevel,
						  LOCKMODE nLockMode )
{
  SHLOCK	nLock;

  PROCEDURE	( fnLevelMode2SHlock );

  nLock	= ( llMin <= nLockLevel && nLockLevel <= llMax ) ?
    Level2SHlock [ nLockLevel ] : eshLockLevelNothing;
  nLock	= (SHLOCK)
    ( (unsigned int) nLock |
      (unsigned int) ( ( lmMin <= nLockMode && nLockMode <= lmMax ) ?
		       Mode2SHlock [ nLockMode ] : eshLockModeNothing ) );

  RETURN ( nLock );
} /* fnLevelMode2SHlock */

/* ----------------------------------------------------------------------- */
/* Puts a vector lock to oSelf: */
static LPSUMLOCK	fnInsertVectorLock	( OBJID oSelf )
{
  OBJID		oLock;
  LPOBJID	lpSHvector;
  LPSUMLOCK	lpSumLock;

  PROCEDURE	( fnInsertVectorLock );

  oLock			=
    fnCreateObject ( (SHTYPETAG) eshSumLockTag, 0, NULLTYPETAG, 0 );
  ASSERT ( boundp ( oLock ) );
  lpSHvector		= AtomicLock ( oSelf, oLock );
  lpSumLock		= (LPSUMLOCK) AtomicLock ( oLock, oSelf );
  lpSHvector [ eshSHvectorIdxLockedBy ]	= oLock;
  lpSumLock->Header.OfPlob.oLockedBy	= oSelf;
  AtomicUnlock ( oLock, oSelf );
  AtomicUnlock ( oSelf, oLock );

  RETURN ( lpSumLock );
} /* fnInsertVectorLock */
   
/* ----------------------------------------------------------------------- */
static LPSUMLOCK	fnSearchElementLock	( OBJID oToSearch,
						  int nIndex,
						  LPOBJID FAR * ppoLast,
						  BOOL bInsert )
{
  LPSUMLOCK	lpSumLock, lpFree;
  LPOBJID	lpoLast, lpSHvector, lpoFreeLast;
  OBJID		oLock, oNext;

  PROCEDURE	( fnSearchElementLock );

  if ( ppoLast == NULL ) {
    ppoLast	= &lpoLast;
  }

  *ppoLast	= (LPOBJID) NULL;
  lpSHvector	= (LPOBJID) SH_key_to_address ( oToSearch );
  ASSERT ( lpSHvector );

  if ( nIndex < 0 ||
       Cooked2RawIndex ( nIndex ) >= eshSHvectorIdxFirstObjId +
       lpSHvector [ eshSHvectorIdxObjIds ] ) {
    ERROR (( szCantIndex, fnPrintObject ( oToSearch, (LPSTR) NULL, 0 ),
	     nIndex ));
    RETURN ( (LPSUMLOCK) NULL );
  }

  oLock		= lpSHvector [ eshSHvectorIdxLockedBy ];
  /* oLock is now unbound or contains the objid of a vector sum-lock. */
  lpSumLock	= (LPSUMLOCK) NULL;
  lpFree	= (LPSUMLOCK) NULL;

  if ( boundp ( oLock ) ) {
    lpSumLock	= fnGetSumLock ( oLock );
    /* Scan through the lock list for nIndex. The first element of the list
       is always a lock for the sh-vector whose onIndex component must
       be unbound; this is checked by the next ASSERT: */
    ASSERT ( ! boundp ( lpSumLock->onIndex ) );
    *ppoLast	= &lpSumLock->Header.oNext;
    oLock	= lpSumLock->Header.oNext;
    while ( boundp ( oLock ) ) {
      lpSumLock	= fnGetSumLock ( oLock );
      ASSERT ( lpSumLock );
      if ( ObjId2Fixnum ( lpSumLock->onIndex ) == nIndex ) {
	/* Found matching index: */
	RETURN ( lpSumLock );
      }
      if ( lpFree == NULL && ! boundp ( lpSumLock->Header.onTotalHold ) ) {
	/* Found a free lock; use this one instead of allocating a new
	   lock: */
	lpFree		= lpSumLock;
	lpoFreeLast	= *ppoLast;
      }
      *ppoLast	= &lpSumLock->Header.oNext;
      oLock		= lpSumLock->Header.oNext;
    }
  } else if ( bInsert ) {
    /* There is no lock set at all to the vector; so insert at first a
       lock for the vector and afterwards a lock for the element: */
    lpSumLock	= fnInsertVectorLock ( oToSearch );
  }

  if ( bInsert ) {
    if ( lpFree ) {
      /* Re-use the free lock found, but initialize it before use: */
      oLock			= lpFree->Header.oSelf;
      lpSumLock			= lpFree;
      oNext			= lpFree->Header.oNext;
      AtomicLock ( oLock, oLock );
      mfnInitSumLock ( oLock, (LPOBJID) lpSumLock, (LPCLASSINFO) NULL );
      lpSumLock->Header.oNext	= oNext;
      *ppoLast		= lpoFreeLast;
    } else {
      /* Lock for nIndex wasn't found; insert it behind lpSumLock: */
      oNext			=
	fnCreateObject ( (SHTYPETAG) eshSumLockTag, 0, NULLTYPETAG, 0 );
      ASSERT ( boundp ( oNext ) );
      AtomicLock ( lpSumLock->Header.oSelf, lpSumLock->Header.oSelf );
      lpSumLock->Header.oNext	= oNext;
      AtomicUnlock ( lpSumLock->Header.oSelf, lpSumLock->Header.oSelf );
      *ppoLast		= &lpSumLock->Header.oNext;
      lpSumLock		= (LPSUMLOCK) AtomicLock ( oNext, oNext );
    }
    lpSumLock->Header.OfPlob.oLockedBy	= oToSearch;
    lpSumLock->onIndex			= Fixnum2ObjId ( nIndex );
    AtomicUnlock ( lpSumLock->Header.oSelf, lpSumLock->Header.oSelf );
  } else {
    lpSumLock	= (LPSUMLOCK) NULL;
    *ppoLast	= (LPOBJID) NULL;
  }

  RETURN ( lpSumLock );
} /* fnSearchElementLock */

/* ----------------------------------------------------------------------- */
static LPSUMLOCK	fnSearchVectorLock	( OBJID oToSearch,
						  BOOL bInsert )
{
  LPSUMLOCK	lpSumLock;
  LPOBJID	lpSHvector;
  OBJID		oLock;

  PROCEDURE	( fnSearchVectorLock );

  lpSumLock	= (LPSUMLOCK) NULL;
  ASSERT_ObjId_is_valid ( oToSearch );
  lpSHvector	= (LPOBJID) SH_key_to_address ( oToSearch );
  ASSERT ( lpSHvector != NULL );
  oLock		= lpSHvector [ eshSHvectorIdxLockedBy ];
  if ( boundp ( oLock ) ) {
    lpSumLock	= fnGetSumLock ( oLock );
    ASSERT ( lpSumLock != NULL );
    ASSERT ( ! boundp ( lpSumLock->onIndex ) );
  } else if ( bInsert ) {
    lpSumLock	= fnInsertVectorLock ( oToSearch );
    ASSERT ( lpSumLock != NULL );
  }
  RETURN ( lpSumLock );
} /* fnSearchVectorLock */

/* ----------------------------------------------------------------------- */
static LPSUMLOCK	fnSearchStoreLock	( BOOL bInsert )
{
  LPSUMLOCK	lpSumLock;
  OBJID		oLock;

  PROCEDURE	( fnSearchStoreLock );

  lpSumLock	= (LPSUMLOCK) NULL;
  oLock		= GetRootLock ();
  if ( ! boundp ( oLock ) && bInsert ) {
    oLock	=
      fnCreateObject ( (SHTYPETAG) eshSumLockTag, 0, NULLTYPETAG, 0 );
    ASSERT ( boundp ( oLock ) );
    fnSetRootLock ( oLock );
  }
  if ( boundp ( oLock ) && ! lpSumLock ) {
    lpSumLock	= fnGetSumLock ( oLock );
  }
  RETURN ( lpSumLock );
} /* fnSearchStoreLock */

/* ----------------------------------------------------------------------- */
/* Scan for a sum-lock object which matches nLockLevel and oToSearch: */
static LPSUMLOCK	fnSearchSumLock		( LOCKLEVEL nLockLevel,
						  OBJID oToSearch,
						  int nIndex,
						  LPOBJID FAR * ppoLast,
						  BOOL bInsert )
{
  PROCEDURE	( fnSearchSumLock );

  if ( ppoLast != NULL ) {
    *ppoLast	= (LPOBJID) NULL;
  }
  switch ( nLockLevel ) {
  case llElement:
    RETURN ( fnSearchElementLock ( oToSearch, nIndex, ppoLast, bInsert ) );
  case llVector:
    RETURN ( fnSearchVectorLock ( oToSearch, bInsert ) );
  case llStore:
    RETURN ( fnSearchStoreLock ( bInsert ) );
  default:
    break;
  }
  RETURN ( (LPSUMLOCK) NULL );
} /* fnSearchSumLock */

/* ----------------------------------------------------------------------- */
/* Scan the list lpSumLock->oOneLocks for a one-lock object which
   contains the 'private' lock information for oToSearch. A reference
   to the object cell before the found resp. inserted one-lock
   is written to *ppoLast for a maybe later done deletion of the
   found one-lock: */
#pragma inline ( fnSearchOneLock )
static
#if WIN32
__inline
#elif defined(__GNUC__)
__inline__
#endif
LPONELOCK		fnSearchOneLock		( LPSUMLOCK lpSumLock,
						  OBJID oToSearch,
						  LPOBJID FAR * ppoLast,
						  BOOL bInsert )
{
  LPONELOCK	lpOneLock, lpFree;
  LPOBJID	lpoLast, lpoFreeLast;
  OBJID		oOneLock, oNext;

  PROCEDURE	( fnSearchOneLock );

  if ( ! ppoLast )
    ppoLast	= &lpoLast;

  if ( ! lpSumLock ) {
    *ppoLast	= (LPOBJID) NULL;
    RETURN ( (LPONELOCK) NULL );
  }

  lpOneLock	= (LPONELOCK) NULL;
  lpFree	= (LPONELOCK) NULL;
  *ppoLast	= &lpSumLock->oOneLocks;
  oOneLock	= lpSumLock->oOneLocks;

  while ( boundp ( oOneLock ) ) {
    lpOneLock	= fnGetOneLock ( oOneLock );
    ASSERT ( lpOneLock );
    if ( lpOneLock->Header.OfPlob.oLockedBy == oToSearch ) {
      /* Found matching object: */
      RETURN ( lpOneLock );
    }
    if ( lpFree == NULL && ! boundp ( lpOneLock->Header.onTotalHold ) ) {
      /* Found a free lock; use this one instead of allocating a new
	 one-lock: */
      lpFree		= lpOneLock;
      lpoFreeLast	= *ppoLast;
    }
    *ppoLast	= &lpOneLock->Header.oNext;
    oOneLock	= lpOneLock->Header.oNext;
  }

  /* Nothing was found; append a new element to the list: */
  if ( bInsert ) {
    if ( lpFree ) {
      /* Re-use the free one-lock found, but initialize it before use: */
      oOneLock			= lpFree->Header.oSelf;
      lpOneLock			= lpFree;
      oNext			= lpFree->Header.oNext;
      AtomicLock ( oOneLock, oOneLock );
      mfnInitOneLock ( oOneLock, (LPOBJID) lpOneLock, (LPCLASSINFO) NULL );
      lpOneLock->Header.oNext	= oNext;
      *ppoLast			= lpoFreeLast;
    } else {
      /* One-lock for oToSearch wasn't found; insert it in
	 lpSumLock->oOneLocks: */
      oNext			=
	fnCreateObject ( (SHTYPETAG) eshOneLockTag, 0, NULLTYPETAG, 0 );
      ASSERT ( boundp ( oNext ) );
      lpOneLock			= (LPONELOCK) AtomicLock ( oNext, oNext );
      lpOneLock->Header.oNext	= lpSumLock->oOneLocks;
      AtomicLock ( lpSumLock->Header.oSelf, oNext );
      lpSumLock->oOneLocks	= oNext;
      AtomicUnlock ( lpSumLock->Header.oSelf, oNext );
      *ppoLast		= &lpSumLock->oOneLocks;
    }
    lpOneLock->Header.OfPlob.oLockedBy	= oToSearch;
    AtomicUnlock ( lpOneLock->Header.oSelf, lpOneLock->Header.oSelf );
  } else {
    lpOneLock			= (LPONELOCK) NULL;
    *ppoLast			= (LPOBJID) NULL;
  }

  RETURN ( lpOneLock );
} /* fnSearchOneLock */

/* ----------------------------------------------------------------------- */
/* Returns current SHLOCK of an object locked by oLockedBy: */
static SHLOCK		fnGetCurrentSHlock	( LPONELOCK lpOneLock,
						  LOCKLEVEL nLockLevel )
{
  SHLOCK		nLockCurrent;
  register LOCKMODE	m;

  PROCEDURE	( fnGetCurrentSHlock );

  if ( lpOneLock ) {
    /* Sum all lock modes: */
    nLockCurrent	= (SHLOCK) 0;
    for ( m = lmMin; m <= lmMax; m = (LOCKMODE) ( (int) m + 1 ) ) {
      if ( ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ m ] ) > 0 )
	nLockCurrent	= (SHLOCK)
	  ( (unsigned int) nLockCurrent |
	    (unsigned int) fnLevelMode2SHlock ( nLockLevel, m ) );
    }
  } else {
    nLockCurrent	= fnLevelMode2SHlock ( nLockLevel, lmNothing );
  }
  RETURN ( nLockCurrent );
} /* fnGetCurrentSHlock */

/* ----------------------------------------------------------------------- */
static int		fnIncHolds		( LPLOCKHEADER lpHeader,
						  LOCKMODE nLockMode,
						  int nIncrement )
{
  int		nTotalHold;

  PROCEDURE	( fnIncHolds );

  nTotalHold		= ObjId2Fixnum ( lpHeader->onTotalHold ) + nIncrement;
  lpHeader->onTotalHold	= Fixnum2ObjId ( nTotalHold );
  INCOBJID ( lpHeader->oTotalHolds [ nLockMode ], nIncrement );
  RETURN ( nTotalHold );
} /* fnIncHolds */

/* ----------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lmgr/lock.c */
static void		fnLockGrant		( LPSUMLOCK lpSumLock,
						  LOCKMODE nLockMode )
{
  PROCEDURE	( fnLockGrant );

  ASSERT ( lpSumLock );
  INCOBJID ( lpSumLock->onActiveHold, 1 );
  INCOBJID ( lpSumLock->oActiveHolds [ nLockMode ], 1 );
  lpSumLock->onLockMask	=
    Fixnum2ObjId ( ObjId2Fixnum ( lpSumLock->onLockMask ) |
		   BITSON ( nLockMode ) );
  RETURN ( VOID );
} /* fnLockGrant */

/* ----------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lmgr/lock.c; the
   following comment and the comments inside this function are also
   taken from this module:

   Here's what makes this complicated: one transaction's locks don't
   conflict with one another. When many objects hold locks, each has
   to subtract off the other's locks when determining whether or not
   any new lock acquired conflicts with the old ones.

   For example, if I am already holding a lmWriteIntent lock, there
   will not be a conflict with my own lmRead lock. If I don't consider
   the intent lock when checking for conflicts, I find no conflict. */
static int		fnResolveConflicts	( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  LOCKMODE nLockMode )
{
  LOCKMASK		nLockMask;
  register LOCKMODE	m;
  int			nActive, nTotal;

  PROCEDURE		( fnResolveConflicts );

  /* First check for global conflicts: If no locks conflict with mine,
     then I get the lock.

     Checking for conflict: lpSumLock->onLockMask represents the types
     of currently held locks. MultiConflict [ nLockMode ] has a bit
     set for each type of lock that conflicts with mine. Bitwise
     compare tells if there is a conflict. */
  if ( ! ( MultiConflicts [ nLockMode ] &
	   ObjId2Fixnum ( lpSumLock->onLockMask ) ) ) {
    fnIncHolds ( &lpOneLock->Header, nLockMode, 1 );
#if (LOGGING+0) & 0x01
    fprintf ( stderr, "%s(%d): No global conflicts for sumlock %d\n"
	      "\tnLockMode 0x%X, Conflicts 0x%X, sum-lock lock mode 0x%X,"
	      " returns %d holds\n",
	      __szProc__, __LINE__,
	      LONG2SHORTOBJID ( lpSumLock->Header.oSelf ),
	      nLockMode, MultiConflicts [ nLockMode ],
	      ObjId2Fixnum ( lpSumLock->onLockMask ),
	      ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ nLockMode ] ) );
#endif /* #if (LOGGING+0) & 0x01 */
    RETURN ( ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ nLockMode ] ) );
  }

  /* Rats (sic! HK). Something conflicts. But it could still be my own
     lock. We have to construct a conflict mask that does not reflect
     our own locks. */
  nLockMask	= 0;
  for ( m = lmMin; m <= lmMax; m = (LOCKMODE) ( (int) m + 1 ) ) {
    nActive	= ObjId2Fixnum ( lpSumLock->oActiveHolds [ m ] );
    nTotal	= ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ m ] );
    if ( nActive != nTotal ) {
      nLockMask	|= BITSON ( m );
    }
  }

  /* Now check again for conflicts. nLockMask describes the types of
     locks held by other objects. If one of these conflicts with the
     kind of lock that I want, there is a conflict and I have to
     sleep.

     HK: Well, actually a LockConflict return value is used to
     indicate that there is a lock conflict, because there is no easy
     way to make the using LISP 'thread' go to sleep; see also comment
     at the beginning of this file. */
  if ( ! ( MultiConflicts [ nLockMode ] & nLockMask ) ) {
    fnIncHolds ( &lpOneLock->Header, nLockMode, 1 );
#if (LOGGING+0) & 0x01
    fprintf ( stderr, "%s(%d): No local conflicts for sumlock %d\n",
	      __szProc__, __LINE__,
	      LONG2SHORTOBJID ( lpSumLock->Header.oSelf ) );
#endif /* #if (LOGGING+0) & 0x01 */
    RETURN ( ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ nLockMode ] ) );
  }

  /* HK: Reaching here means that there is a conflict; the calling process
     should be set to 'sleep': */
  RETURN ( 0 );
} /* fnResolveConflicts */

/* ----------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lmgr/lock.c */
static int		fnLockAcquire		( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  LOCKMODE nLockMode )
{
  int		nLockCount, nTotal, nActive;

  PROCEDURE	( fnLockAcquire );

  nLockCount	= 0;

  ASSERT ( lpSumLock != NULL );
  ASSERT ( lpOneLock != NULL );

  AtomicLock ( lpSumLock->Header.oSelf, lpOneLock->Header.oSelf );
  AtomicLock ( lpOneLock->Header.oSelf, lpSumLock->Header.oSelf );

  fnIncHolds ( &lpSumLock->Header, nLockMode, 1 );

  nTotal	= ObjId2Fixnum ( lpOneLock->Header.onTotalHold );
  nActive	= ObjId2Fixnum ( lpSumLock->onActiveHold );
  if ( nTotal == nActive ) {
    /* I'm the only one which holds a lock: */
    fnIncHolds ( &lpOneLock->Header, nLockMode, 1 );
    nLockCount	=
      ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ nLockMode ] );
    fnLockGrant ( lpSumLock, nLockMode );
    AtomicUnlock ( lpOneLock->Header.oSelf, lpSumLock->Header.oSelf );
    AtomicUnlock ( lpSumLock->Header.oSelf, lpOneLock->Header.oSelf );
  } else {
    ASSERT ( nTotal <= nActive );
    nLockCount	= fnResolveConflicts ( lpSumLock, lpOneLock, nLockMode );
    if ( nLockCount ) {
      fnLockGrant ( lpSumLock, nLockMode );
      AtomicUnlock ( lpOneLock->Header.oSelf, lpSumLock->Header.oSelf );
      AtomicUnlock ( lpSumLock->Header.oSelf, lpOneLock->Header.oSelf );
    } else {
      /* Wait until someone grants me the lock: */
      AtomicUnlock ( lpOneLock->Header.oSelf, lpSumLock->Header.oSelf );
      AtomicUnlock ( lpSumLock->Header.oSelf, lpOneLock->Header.oSelf );
      nLockCount	= fnLockWait ( lpSumLock->Header.oSelf,
				       lpOneLock->Header.oSelf,
				       nLockMode );
      if ( nLockCount <= 0 ) {
	/* The wait failed: */
	AtomicLock ( lpSumLock->Header.oSelf, lpOneLock->Header.oSelf );
	fnIncHolds ( &lpSumLock->Header, nLockMode, -1 );
	AtomicUnlock ( lpSumLock->Header.oSelf, lpOneLock->Header.oSelf );
      }
    }
  }

#if (LOGGING+0) & 0x01
  if ( nLockCount > 0 ) {
    fprintf ( stderr, "%s(%d): Acquired mode '%s' for sumlock %d\n",
	      __szProc__, __LINE__, ppszModes [ nLockMode ],
	      LONG2SHORTOBJID ( lpSumLock->Header.oSelf ) );
  } else {
    fprintf ( stderr, "%s(%d): Rejected mode '%s' for sumlock %d\n",
	      __szProc__, __LINE__, ppszModes [ nLockMode ],
	      LONG2SHORTOBJID ( lpSumLock->Header.oSelf ) );
  }
#endif /* #if (LOGGING+0) & 0x01 */

  RETURN ( nLockCount );
} /* fnLockAcquire */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnGetLockOfLevel	( OBJID oLockedByP,
						  LOCKLEVEL nLockLevel,
						  OBJID oLockedP,
						  int nIndexP )
{
  LPSUMLOCK	lpSumLock;
  LPONELOCK	lpOneLock;

  PROCEDURE	( fnGetLockOfLevel );

  lpSumLock	= fnSearchSumLock ( nLockLevel, oLockedP, nIndexP,
				    (LPOBJID *) NULL, FALSE );
  lpOneLock	= fnSearchOneLock ( lpSumLock, oLockedByP,
				    (LPOBJID *) NULL, FALSE );
  RETURN ( fnGetCurrentSHlock ( lpOneLock, nLockLevel ) );
} /* fnGetLockOfLevel */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnSetLockByLevelMode	( OBJID oLockBy,
						  LOCKLEVEL nLockLevel,
						  LOCKMODE nLockMode,
						  BOOL bForce,
						  OBJID oToLock,
						  int nIndex,
						  SHLOCK * pnVectorLockNow )
{
  static const char	szTo []		= "\n       to object ";

  struct {
    LPSUMLOCK	lpSumLock;
    LPOBJID	lpoLastSumLock;
    LPONELOCK	lpOneLock;
    LPOBJID	lpoLastOneLock;
    LOCKMODE	nLockMode;
    int		nLockCount;
  }			Locks [ llNumberOf ];
  LOCKMODE		m;
  SHLOCK		nLockOld;
  LPSUMLOCK		lpSumLock;
  LPONELOCK		lpOneLock;
  LPOBJID		lpoLastSumLock, lpoLastOneLock;
  LOCKLEVEL		k, l;
  LPCSTR		lpszWhyNot;

  PROCEDURE		( fnSetLockByLevelMode );

  if ( nLockLevel == llNothing ) {
    if ( pnVectorLockNow != NULL ) {
      *pnVectorLockNow	= fnGetLockOfLevel ( oLockBy, llVector, oToLock, -1 );
    }
    if ( ! bForce && nLockMode != lmNothing ) {
      char	szLockBy [ 512 ];
      CERROR (( "Ignore lock request.",
	        "The locking object %s can't be placed on level '%s'"
	        " with mode '%s' on object %s.",
	        PrintObject ( oLockBy, szLockBy ),
	        szLevelNothing, ppszModes [ nLockMode ],
	        fnPrintObject ( oToLock, (LPSTR) NULL, 0 ) ));
    }
    RETURN ( eshLockModeNothing );
  }

  memset ( &Locks, 0, sizeof ( Locks ) );
  Locks [ nLockLevel ].lpSumLock	=
    fnSearchSumLock ( nLockLevel, oToLock, nIndex,
		      & Locks [ nLockLevel ].lpoLastSumLock,
		      (BOOL) ( nLockMode != lmNothing ) );
  Locks [ nLockLevel ].lpOneLock	=
    fnSearchOneLock ( Locks [ nLockLevel ].lpSumLock, oLockBy,
		      & Locks [ nLockLevel ].lpoLastOneLock,
		      (BOOL) ( nLockMode != lmNothing ) );

  /* Get old lock mode: */
  nLockOld	= fnGetCurrentSHlock ( Locks [ nLockLevel ].lpOneLock,
				       nLockLevel );
  if ( nLockMode == lmNothing ) {
    /* Just return old lock mode for passed level: */
    if ( pnVectorLockNow != NULL ) {
      *pnVectorLockNow	= ( nLockLevel == llVector ) ?
	nLockOld :
	fnGetLockOfLevel ( oLockBy, MAX ( nLockLevel, llVector ),
			   oToLock, -1 );
    }
    RETURN ( nLockOld );
  }

  if ( ! Locks [ nLockLevel ].lpSumLock ) {
    /* Hmpf: Inserting the sum-lock object failed; this indicates a HEAVY
       internal error. */
    char	szLockBy [ 512 ];
    ERROR (( "Setting locking object %s on level '%s' with"
	     " mode '%s'%s%s failed; the lock-object"
	     " could not be inserted.",
	     PrintObject ( oLockBy, szLockBy ),
	     ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
	     ( nLockLevel == llStore ) ?
	     szEmpty : szTo,
	     ( nLockLevel == llStore ) ?
	     szEmpty : fnPrintObject ( oToLock, (LPSTR) NULL, 0 ) ));
    RETURN ( eshLockFailed );
  }

  /* Do the locking; start from highest level llMax going down to
     nLockLevel: */
  Locks [ nLockLevel ].nLockMode	= nLockMode;
  for ( l = llMax; l >= nLockLevel; l = (LOCKLEVEL) ( (int) l - 1 ) ) {
    /* Mode to set on level l: if l > specified lock level, set an intent
       lock plus specified lock mode, otherwise only the specified lock
       mode: */
    m					= 
      ( l > nLockLevel ) ? (LOCKMODE) ( (unsigned int) nLockMode +
					(unsigned int) lmIntent ) :
      nLockMode;
    /* Store found lock mode to LockModes to re-use it in case of failure: */
    Locks [ l ].nLockMode		= m;
    if ( Locks [ l ].lpSumLock ) {
      lpSumLock				= Locks [ l ].lpSumLock;
      lpoLastSumLock			= Locks [ l ].lpoLastSumLock;
      lpOneLock				= Locks [ l ].lpOneLock;
      lpoLastOneLock			= Locks [ l ].lpoLastOneLock;
    } else {
      lpSumLock				=
	fnSearchSumLock ( l, oToLock, nIndex, &lpoLastSumLock, TRUE );
      lpOneLock				=
	fnSearchOneLock ( lpSumLock, oLockBy, &lpoLastOneLock, TRUE );
      Locks [ l ].lpSumLock		= lpSumLock;
      Locks [ l ].lpoLastSumLock	= lpoLastSumLock;
      Locks [ l ].lpOneLock		= lpOneLock;
      Locks [ l ].lpoLastOneLock	= lpoLastOneLock;
      if ( ! lpSumLock ) {
	/* fnSearchSumLock failed; return with failure: */
	nLockOld			= eshLockFailed;
	break;
      }
    }
    /* Now try to set lock mode m to lock lpSumLock: */
    Locks [ l ].nLockCount		=
      fnLockAcquire ( lpSumLock, lpOneLock, m );
    if ( Locks [ l ].nLockCount <= 0 ) {
      /* Return with lock conflict on level l: */
      nLockOld				= (SHLOCK) Level2Conflict [ l ];
      break;
    }
  }

  if ( (int) nLockOld < 0 ) {
    /* A failure here means that the fnLockWait failed; this may have
       been happened by a time-out or so. In the meantime another
       process could have locked the object; so the lpoLast...-pointer
       are maybe invalidated. So the locks have to be re-searched
       again to make sure they are up-to-date; then the already
       acquired locks can be released safely: */
    for ( k = nLockLevel; k <= llMax; k = (LOCKLEVEL) ( (int) k + 1 ) ) {
      if ( Locks [ k ].lpSumLock ) {
	lpSumLock	=
	  fnSearchSumLock ( k, oToLock, nIndex, &lpoLastSumLock, FALSE );
	if ( lpSumLock ) {
	  ASSERT ( boundp ( lpSumLock->Header.onTotalHold ) );
	  lpOneLock	=
	    fnSearchOneLock ( lpSumLock, oLockBy, &lpoLastOneLock, FALSE );
	  if ( lpOneLock ) {
	    ASSERT ( boundp ( lpOneLock->Header.onTotalHold ) );
	    fnLockRelease ( lpSumLock, lpOneLock,
			    oLockBy, Locks [ k ].nLockMode,
			    oToLock, k, lpoLastSumLock, lpoLastOneLock );
	  }
	}
      }
    }
  } else {
    lpszWhyNot	=
      gfnLockAcquired ( oLockBy, oToLock, nLockOld,
			Locks [ nLockLevel ].nLockCount,
			fnGetCurrentSHlock ( Locks [ nLockLevel ].lpOneLock,
					     nLockLevel ) );
    if ( lpszWhyNot != NULL ) {
      char	szLockBy [ 512 ];
      /* oLockBy denied locking object oToLock: */
      for ( k = nLockLevel; k <= llMax; k = (LOCKLEVEL) ( (int) k + 1 ) ) {
	fnLockRelease ( Locks [ k ].lpSumLock,  Locks [ k ].lpOneLock,
			oLockBy, Locks [ k ].nLockMode, oToLock, k,
			Locks [ k ].lpoLastSumLock,
			Locks [ k ].lpoLastOneLock );
      }
      nLockOld		= eshLockDenied;
      CERROR (( "Return with lock denied.",
		"Object %s\n"
		"       denied lock on level '%s' with mode '%s'"
		"%s%s%s%s",
		PrintObject ( oLockBy, szLockBy ),
		ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
		( nLockLevel == llStore ) ?
		szEmpty : szTo,
		( nLockLevel == llStore ) ?
		szEmpty : fnPrintObject ( oToLock, (LPSTR) NULL, 0 ),
		( *lpszWhyNot ) ? ":\n       " : ".",
		lpszWhyNot ));
    }
  }
  if ( pnVectorLockNow != NULL ) {
    *pnVectorLockNow	=
      fnGetLockOfLevel ( oLockBy, MAX ( nLockLevel, llVector ), oToLock, -1 );
  }
  RETURN ( nLockOld );
} /* fnSetLockByLevelMode */

/* ----------------------------------------------------------------------- */
static void		fnDeleteOneLock		( LPONELOCK lpOneLock,
						  LPOBJID lpoLast )
{
  PROCEDURE	( fnDeleteOneLock );

  ASSERT ( lpoLast );
  ASSERT ( *lpoLast == lpOneLock->Header.oSelf );

  *lpoLast	= lpOneLock->Header.oNext;
  makunbound ( lpOneLock->Header.OfPlob.oLockedBy );
  fnDestroyObject ( lpOneLock->Header.oSelf, FALSE );

  RETURN ( VOID );
}  /* fnDeleteOneLock */

/* ----------------------------------------------------------------------- */
static void		fnDeleteElementLock	( OBJID oToUnlock,
						  LPOBJID lpoLast )
{
  LPSUMLOCK	lpNext;

  PROCEDURE	( fnDeleteElementLock );

  ASSERT ( lpoLast );
  ASSERT ( boundp ( *lpoLast ) );

  lpNext	= fnGetSumLock ( *lpoLast );
  ASSERT ( lpNext );
  /* Make sure that we are located on an element lock: */
  ASSERT ( boundp ( lpNext->onIndex ) );
  *lpoLast	= lpNext->Header.oNext;
  makunbound ( lpNext->Header.OfPlob.oLockedBy );
  fnDestroyObject ( lpNext->Header.oSelf, FALSE );

  RETURN ( VOID );
}  /* fnDeleteElementLock */

/* ----------------------------------------------------------------------- */
static void		fnDeleteVectorLock	( OBJID oToUnlock )
{
  LPOBJID	lpSHvector;
  LPSUMLOCK	lpSumLock;

  PROCEDURE	( fnDeleteVectorLock );

  lpSHvector	= (LPOBJID) SH_key_to_address ( oToUnlock );
  lpSumLock	= fnGetSumLock ( lpSHvector [ eshSHvectorIdxLockedBy ] );
  makunbound ( lpSumLock->Header.OfPlob.oLockedBy );
  makunbound ( lpSHvector [ eshSHvectorIdxLockedBy ] );
  fnDestroyObject ( lpSumLock->Header.oSelf, FALSE );
  RETURN ( VOID );
}  /* fnDeleteVectorLock */

/* ----------------------------------------------------------------------- */
/* The store lock exists only once; so there is no need to delete it: */
static void		fnDeleteStoreLock	( void )
{
  PROCEDURE	( fnDeleteStoreLock );
  RETURN ( VOID );
}  /* fnDeleteStoreLock */

/* ----------------------------------------------------------------------- */
static void		fnDeleteSumLock		( LOCKLEVEL nLockLevel,
						  OBJID oToUnlock,
						  LPOBJID lpoLast )
{
  PROCEDURE	( fnDeleteSumLock );

  switch ( nLockLevel ) {
  case llElement:
    fnDeleteElementLock ( oToUnlock, lpoLast );
    break;
  case llVector:
    fnDeleteVectorLock ( oToUnlock );
    break;
  case llStore:
    fnDeleteStoreLock ();
  default:
    break;
  }
  RETURN ( VOID );
}  /* fnDeleteSumLock */

/* ----------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lmgr/lock.c */
static int		fnLockRelease		( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  OBJID oLockedBy,
						  LOCKMODE nLockMode,
						  OBJID oToUnlock,
						  LOCKLEVEL nLockLevel,
						  LPOBJID lpoSumLast,
						  LPOBJID lpoOneLast )
{
  int		nLockCountOld, nIncrement, nActive;
  int		nOneHolds, nSumHolds;

  PROCEDURE	( fnLockRelease );

  if ( ! lpSumLock )
    RETURN ( 0 );

  nLockCountOld	=  0;
  nIncrement	= -1;

  if ( ! lpOneLock ) {

    /* Locking object wasn't found at all; so the object can't be
       locked by it: */
#if (LOGGING+0) & 0x01
    {
      fprintf ( stderr, "%s(%d): No lock found on level '%s', mode '%s'"
	        " for objid %d,\n\tsum-lock %d\n", __szProc__, __LINE__,
	        ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
	        LONG2SHORTOBJID ( oToUnlock ),
	        LONG2SHORTOBJID ( lpSumLock->Header.oSelf ) );
    }
#endif /* #if (LOGGING+0) & 0x01 */
    nIncrement	= 0;

  } else {

    nLockCountOld	=
      ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ nLockMode ] );
    if ( nLockCountOld  == 0 ) {
      /* The object wasn't locked with mode nLockMode, so don't change
	 the statistics (onTotalHold etc.) but check if the one-lock or
	 sum-lock is subject to be deleted: */
#if (LOGGING+0) & 0x01
      {
	fprintf ( stderr,
		  "%s(%d): No mode '%s' found on level '%s' for objid %d,\n"
		  "\tsum-lock %d,\n\tone-lock %d\n",
		  __szProc__, __LINE__, ppszModes [ nLockMode ],
		  ppszLevels [ nLockLevel ],
		  LONG2SHORTOBJID ( oToUnlock ),
		  LONG2SHORTOBJID ( lpSumLock->Header.oSelf ),
		  LONG2SHORTOBJID ( lpOneLock->Header.oSelf ) );
      }
#endif /* #if (LOGGING+0) & 0x01 */
      nIncrement	= 0;
    }

    AtomicLock ( lpOneLock->Header.oSelf, NULLOBJID );
    nOneHolds	= fnIncHolds ( &lpOneLock->Header, nLockMode, nIncrement );
    AtomicUnlock ( lpOneLock->Header.oSelf, NULLOBJID );

    if ( nOneHolds <= 0 ) {
      if ( nOneHolds < 0 ) {
	/* This error message indicates an inconsistent state of a one-lock.
	   This arises e.g. when sub-locks are removed 'the hard way', i.e.
	   locks which have been released without calling fnLockRelease: */
	char	szLockedBy [ 512 ];
	CERROR (( "Delete the one-lock.",
		  "Found one-lock for locked object %s, locking object %s,"
		  " level '%s', mode '%s' with number %d of total locks"
		  " less than 0.",
		  fnPrintObject ( oToUnlock, (LPSTR) NULL, 0 ),
		  PrintObject ( oLockedBy, szLockedBy ),
		  ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
		  nOneHolds ));
      }
      /* This was the last one-lock; free it: */
#if (LOGGING+0) & 0x01
      fprintf ( stderr, "%s(%d): Deleting one-lock %d\n",
	       __szProc__, __LINE__,
		LONG2SHORTOBJID ( lpOneLock->Header.oSelf ) );
#endif /* #if (LOGGING+0) & 0x01 */
      fnDeleteOneLock ( lpOneLock, lpoOneLast );
    }
  }

  ASSERT ( lpSumLock != NULL );
  AtomicLock ( lpSumLock->Header.oSelf, NULLOBJID );
  INCOBJID ( lpSumLock->onActiveHold, nIncrement );
  nActive	=
    ObjId2Fixnum ( lpSumLock->oActiveHolds [ nLockMode ] ) + nIncrement;
  lpSumLock->oActiveHolds [ nLockMode ]	= Fixnum2ObjId ( nActive );
  if ( nActive <= 0 ) {
    if ( nActive < 0 ) {
      /* This error message indicates an inconsistent state of a
	 sum-lock.  This arises e.g. when sub-locks are removed 'the
	 hard way', i.e.  locks which have been released without
	 calling fnLockRelease: */
      char	szLockedBy [ 512 ];
      CERROR (( "Remove mode from sum-lock.",
	        "Found sum-lock for locked object %s, locking object %s,"
	        " level '%s', mode '%s' with number %d of active mode locks"
	        " less than 0.",
	        fnPrintObject ( oToUnlock, (LPSTR) NULL, 0 ),
	        PrintObject ( oLockedBy, szLockedBy ),
	        ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
	        nActive ));
    }
#if (LOGGING+0) & 0x01
    fprintf ( stderr, "%s(%d): Deleted mode '%s' from sum-lock %d\n",
	      __szProc__, __LINE__, ppszModes [ nLockMode ],
	      LONG2SHORTOBJID ( lpSumLock->Header.oSelf ) );
#endif /* #if (LOGGING+0) & 0x01 */
    lpSumLock->onLockMask	=
      Fixnum2ObjId ( ObjId2Fixnum ( lpSumLock->onLockMask ) &
		     BITSOFF ( nLockMode ) );
  }

  nSumHolds	= fnIncHolds ( &lpSumLock->Header, nLockMode, nIncrement );
  AtomicUnlock ( lpSumLock->Header.oSelf, NULLOBJID );

  if ( nSumHolds <= 0 ) {
    if ( nSumHolds < 0 ) {
      /* This error message indicates an inconsistent state of a
	 sum-lock.  This arises e.g. when sub-locks are removed 'the
	 hard way', i.e.  locks which have been released without
	 calling fnLockRelease: */
      char	szLockedBy [ 512 ];
      CERROR (( "Delete the sum-lock.",
	        "Found sum-lock for locked object %s, locking object %s,"
	        " level '%s', mode '%s' with number %d of total locks"
	        " less than 0.",
	        fnPrintObject ( oToUnlock, (LPSTR) NULL, 0 ),
	        PrintObject ( oLockedBy, szLockedBy ),
	        ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
	        nSumHolds ));
    }
    /* This was the last sum-lock; free it: */
#if (LOGGING+0) & 0x01
    fprintf ( stderr, "%s(%d): Deleting sum-lock %d\n",
	      __szProc__, __LINE__,
	      LONG2SHORTOBJID ( lpSumLock->Header.oSelf ) );
#endif /* #if (LOGGING+0) & 0x01 */
    fnDeleteSumLock ( nLockLevel, oToUnlock, lpoSumLast );
  } else if ( nActive <= 0 ) {
    fnLockWakeup ( lpSumLock->Header.oSelf );
  }

  RETURN ( nLockCountOld );
} /* fnLockRelease */

/* ----------------------------------------------------------------------- */
static int		fnLockReleaseAll	( LPSUMLOCK lpSumLock,
						  LPONELOCK lpOneLock,
						  OBJID oLockedBy,
						  OBJID oToUnlock,
						  LOCKLEVEL nLockLevel,
						  LPOBJID lpoSumLast,
						  LPOBJID lpoOneLast )
{
  int		nLockCountOld, nTotalHolds [ lmNumberOf ], nHolds;
  int		nActive, nSumActive;
  LOCKMODE	m;

  PROCEDURE	( fnLockReleaseAll );

  if ( ! lpSumLock )
    RETURN ( 0 );

  nLockCountOld	= 0;
  nSumActive	= 0;
  memset ( nTotalHolds, 0, sizeof ( nTotalHolds ) );

  if ( lpOneLock ) {
    nLockCountOld		=
      ObjId2Fixnum ( lpOneLock->Header.onTotalHold );
    if ( nLockCountOld > 0 ) {
      for ( m = lmMin; m <= lmMax; m = (LOCKMODE) ( (int) m + 1 ) ) {
	nTotalHolds [ m ]	=
	  ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ m ] );
      }
    }
    fnDeleteOneLock ( lpOneLock, lpoOneLast );
  }

  AtomicLock ( lpSumLock->Header.oSelf, NULLOBJID );
  INCOBJID ( lpSumLock->onActiveHold, -nLockCountOld );
  if ( nLockCountOld > 0 ) {
    for ( m = lmMin; m <= lmMax; m = (LOCKMODE) ( (int) m + 1 ) ) {
      nActive				=
	ObjId2Fixnum ( lpSumLock->oActiveHolds [ m ] ) - nTotalHolds [ m ];
      ASSERT ( nActive >= 0 );
      lpSumLock->oActiveHolds [ m ]	= Fixnum2ObjId ( nActive );
      if ( nActive <= 0 ) {
	lpSumLock->onLockMask		=
	  Fixnum2ObjId ( ObjId2Fixnum ( lpSumLock->onLockMask ) &
			 BITSOFF ( m ) );
	nSumActive++;
      }
    }
  }

  nHolds			=
    ObjId2Fixnum ( lpSumLock->Header.onTotalHold ) - nLockCountOld;
  ASSERT ( nHolds >= 0 );
  lpSumLock->Header.onTotalHold	= Fixnum2ObjId ( nHolds );
  if ( nLockCountOld > 0 ) {
    for ( m = lmMin; m <= lmMax; m = (LOCKMODE) ( (int) m + 1 ) ) {
      lpSumLock->Header.oTotalHolds [ m ]	=
	Fixnum2ObjId ( ObjId2Fixnum ( lpSumLock->Header.oTotalHolds [ m ] ) -
		       nTotalHolds [ m ] );
    }
  }
  AtomicUnlock ( lpSumLock->Header.oSelf, NULLOBJID );
  if ( nHolds <= 0 ) {
    /* This was the last sum-lock; free it: */
    fnDeleteSumLock ( nLockLevel, oToUnlock, lpoSumLast );
  } else if ( nSumActive ) {
    fnLockWakeup ( lpSumLock->Header.oSelf );
  }

  RETURN ( nLockCountOld );
} /* fnLockReleaseAll */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
int DLLEXPORT		fnPrintLock		( OBJID oSelf,
						  SHLOCK nLevel,
						  FILE FAR * lpStream )
{
  static const char	szNotLocked []	= "  Not locked.\n";
  static const char	szLockedBy []	= "  Locked by %s\n";
  static const char	szLocking []	= "    Locking object is %s\n";

  LPSUMLOCK	lpSumLock;
  LPONELOCK	lpOneLock;
  LPOBJID	lpSelf;
  OBJID		oLockedBy, oI, oJ;
  int		nLocked;

  PROCEDURE	( fnPrintLock );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  nLevel	= (SHLOCK)
    ( (unsigned int) nLevel & (unsigned int) eshLockLevelMask );

  if ( nLevel == eshLockLevelNothing ) {
    RETURN ( 0 );
  }

  if ( nLevel == eshLockLevelStore ) {
    oSelf	= GetRootLock ();
  }

  switch ( typetagof ( oSelf ) ) {
  case eshSumLockTag:
    lpSumLock	= fnGetSumLock ( oSelf );
    oSelf	= lpSumLock->Header.OfPlob.oLockedBy;
    break;
  case eshOneLockTag:
    CERROR (( "Don't print lock information.",
	      "Called %s with one-lock instance %s.", __szProc__,
	      fnPrintObject ( oSelf, (LPSTR) NULL, 0 ) ));
    RETURN ( 0 );
  }

  nLocked	= 0;
  fprintf ( lpStream, "Object %s:\n",
	    fnPrintObject ( oSelf, (LPSTR) NULL, 0 ) );
  if ( immediatep ( oSelf ) ) {
    fputs ( szNotLocked, lpStream );
  } else {
    lpSelf	= (LPOBJID) SH_key_to_address ( oSelf );
    oLockedBy	= lpSelf [ eshSHvectorIdxLockedBy ];
    if ( boundp ( oLockedBy ) ) {
      if ( nLevel >= eshLockLevelVector ) {
	fprintf ( lpStream, szLockedBy,
		  fnPrintObject ( oLockedBy, (LPSTR) NULL, 0 ) );
      }
      lpSumLock	= fnGetSumLock ( oLockedBy );
      for ( oI = lpSumLock->oOneLocks; boundp ( oI );
	    oI = lpOneLock->Header.oNext ) {
	nLocked++;
	lpOneLock	= fnGetOneLock ( oI );
	if ( nLevel >= eshLockLevelVector ) {
	  fprintf ( lpStream, szLocking,
		    fnPrintObject ( oI, (LPSTR) NULL, 0 ) );
	}
      }
      for ( oI = lpSumLock->Header.oNext; boundp ( oI );
	    oI = lpSumLock->Header.oNext ) {
	nLocked++;
	lpSumLock	= fnGetSumLock ( oI );
	fprintf ( lpStream, szLockedBy,
		  fnPrintObject ( oI, (LPSTR) NULL, 0 ) );
	for ( oJ = lpSumLock->oOneLocks; boundp ( oJ );
	      oJ = lpOneLock->Header.oNext ) {
	  nLocked++;
	  lpOneLock	= fnGetOneLock ( oJ );
	  if ( nLevel >= eshLockLevelElement ) {
	    fprintf ( lpStream, szLocking,
		      fnPrintObject ( oJ, (LPSTR) NULL, 0 ) );
	  }
	}
      }
    } else {
      fputs ( szNotLocked, lpStream );
    }
  }
  RETURN ( nLocked );
} /* fnPrintLock */

/* ----------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lmgr/multi.c */
SHLOCK DLLEXPORT	fnUnlock		( OBJID oLockedBy,
						  SHLOCK nLock,
						  OBJID oToUnlock,
						  int nIndex,
						  SHLOCK * pnVectorLockNow )
{
  static const char	szFrom []		= " from object ";

  struct {
    LPSUMLOCK	lpSumLock;
    LPOBJID	lpoLastSumLock;
    LPONELOCK	lpOneLock;
    LPOBJID	lpoLastOneLock;
    int		nLockCountOld;
  }			Unlocks [ llNumberOf ];
  LOCKLEVEL		nLockLevel;
  LOCKMODE		nLockMode, m;
  BOOL			bForce;
  SHLOCK		nLockOld;
  LPSUMLOCK		lpSumLock;
  LPONELOCK		lpOneLock;
  LPOBJID		lpoLastSumLock, lpoLastOneLock;
  LOCKLEVEL		l;
  int			nFailures;

  PROCEDURE		( fnUnlock );

  if ( ! fnSHlock2LevelMode ( nLock, &nLockLevel, &nLockMode,
			      (LPBOOL) NULL, &bForce ) ) {
    char	szLockedBy [ 512 ];
    ERROR (( "Removing locking object %s from object %s failed;"
	     " illegal unlock-level or unlock-mode requested.",
	     PrintObject ( oLockedBy, szLockedBy ),
	     fnPrintObject ( oToUnlock, (LPSTR) NULL, 0 ) ));
    RETURN ( eshLockFailed );
  }

  if ( nLockLevel == llNothing ) {
    if ( pnVectorLockNow != NULL ) {
      *pnVectorLockNow	=
	fnGetLockOfLevel ( oLockedBy, llVector, oToUnlock, -1 );
    }
    if ( ! bForce && nLockMode != lmNothing ) {
      char	szLockedBy [ 512 ];
      CERROR (( "Ignore unlock request.",
	        "The locking object %s can't be deleted on level '%s'"
	        " with mode '%s' from object %s.",
	        PrintObject ( oLockedBy, szLockedBy ),
	        szLevelNothing, ppszModes [ nLockMode ],
	        fnPrintObject ( oToUnlock, (LPSTR) NULL, 0 ) ));
    }
    RETURN ( eshLockModeNothing );
  }

  memset ( Unlocks, 0, sizeof ( Unlocks ) );
  Unlocks [ nLockLevel ].lpSumLock	=
    fnSearchSumLock ( nLockLevel, oToUnlock, nIndex,
		      & Unlocks [ nLockLevel ].lpoLastSumLock, FALSE );
  Unlocks [ nLockLevel ].lpOneLock	=
    fnSearchOneLock ( Unlocks [ nLockLevel ].lpSumLock, oLockedBy,
		      & Unlocks [ nLockLevel ].lpoLastOneLock, FALSE );
  /* Get old lock mode: */
  nLockOld	= fnGetCurrentSHlock ( Unlocks [ nLockLevel ].lpOneLock,
				       nLockLevel );
  if ( nLockMode == lmNothing ) {
    /* Just return old lock mode for passed level: */
    if ( pnVectorLockNow != NULL ) {
      *pnVectorLockNow	= ( nLockLevel == llVector ) ?
	nLockOld :
	fnGetLockOfLevel ( oLockedBy, MAX ( nLockLevel, llVector ),
			   oToUnlock, -1 );
    }
    RETURN ( nLockOld );
  }

  if ( ! Unlocks [ nLockLevel ].lpSumLock ) {
    /* Object is not locked at all: */
    if ( pnVectorLockNow != NULL ) {
      *pnVectorLockNow	=
	fnGetLockOfLevel ( oLockedBy, MAX ( nLockLevel, llVector ),
			   oToUnlock, -1 );
    }
    if ( ! bForce ) {
      char	szLockedBy [ 512 ];
      WARN (( "Removing locking object %s on level '%s' with"
	      " mode '%s'%s%s failed because it is not"
	      " locked by anyone.",
	      PrintObject ( oLockedBy, szLockedBy ),
	      ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
	      ( nLockLevel == llStore ) ?
	      szEmpty : szFrom,
	      ( nLockLevel == llStore ) ?
	      szEmpty : fnPrintObject ( oToUnlock, (LPSTR) NULL, 0 ) ));
    }
    RETURN ( nLockOld );
  }

  /* Do the unlocking; start from level nLockLevel going up to highest level
     llMax: */
  nFailures	= 0;
  for ( l = nLockLevel; l <= llMax; l = (LOCKLEVEL) ( (int) l + 1 ) ) {
    /* Mode to unset on level l: if l > specified lock level, unset an
       intent lock plus specified lock mode, otherwise only the specified
       lock mode: */
    m					= 
      ( l > nLockLevel ) ? (LOCKMODE) ( (unsigned int) nLockMode +
					(unsigned int) lmIntent ) :
      nLockMode;
    if ( Unlocks [ l ].lpSumLock ) {
      lpSumLock				= Unlocks [ l ].lpSumLock;
      lpoLastSumLock			= Unlocks [ l ].lpoLastSumLock;
      lpOneLock				= Unlocks [ l ].lpOneLock;
      lpoLastOneLock			= Unlocks [ l ].lpoLastOneLock;
    } else {
      lpSumLock				=
	fnSearchSumLock ( l, oToUnlock, nIndex, &lpoLastSumLock, FALSE );
      lpOneLock				=
	fnSearchOneLock ( lpSumLock, oLockedBy, &lpoLastOneLock, FALSE );
      Unlocks [ l ].lpSumLock		= lpSumLock;
      Unlocks [ l ].lpoLastSumLock	= lpoLastSumLock;
      Unlocks [ l ].lpOneLock		= lpOneLock;
      Unlocks [ l ].lpoLastOneLock	= lpoLastOneLock;
    }
    Unlocks [ l ].nLockCountOld	=
      fnLockRelease ( lpSumLock, lpOneLock, oLockedBy, m, oToUnlock, l,
		      lpoLastSumLock, lpoLastOneLock );
    if ( ! Unlocks [ l ].nLockCountOld ) {
      nFailures++;
      /* Stop unlocking, because the unlock on 'higher' levels might work
         and leads to inconsistent values in the lock statistics
	 (onTotalHold, oTotalHolds etc.). Example:
	  - unlock on level 'element' fails because the object is not
	    locked with the index and lock mode to be deleted;
	    BUT
          - there is a corresponding 'intent' lock on level 'vector' because
	    e.g. there is another element lock by the same locking object;
	    THIS
	  - would delete the intent-lock on level 'vector', although there
	    was an error at deleting the lock on the lower 'element' level.
	 Therefore, the unlock must terminate when encountering an error. */
      break;
    }
  }
  if ( nFailures == 0 ) {
    gfnLockReleased ( oLockedBy, oToUnlock, nLockOld,
		      Unlocks [ nLockLevel ].nLockCountOld,
		      fnGetCurrentSHlock ( ( boundp ( Unlocks [ nLockLevel ].
						      lpOneLock->Header.
						      onTotalHold ) ) ?
					   Unlocks [ nLockLevel ].lpOneLock :
					   (LPONELOCK) NULL,
					   nLockLevel ) );
  } else if ( ! bForce ) {
    char	szLockedBy [ 512 ];
    WARN (("Removing locking object %s on level '%s' with"
	   " mode '%s'%s%s failed.",
	   PrintObject ( oLockedBy, szLockedBy ),
	   ppszLevels [ nLockLevel ], ppszModes [ nLockMode ],
	   ( nLockLevel == llStore ) ?
	   szEmpty : szFrom,
	   ( nLockLevel == llStore ) ?
	   szEmpty : fnPrintObject ( oToUnlock, (LPSTR) NULL, 0 ) ));
  }
  if ( pnVectorLockNow != NULL ) {
    *pnVectorLockNow	=
      fnGetLockOfLevel ( oLockedBy, MAX ( nLockLevel, llVector ),
			 oToUnlock, -1 );
  }
  RETURN ( nLockOld );
} /* fnUnlock */

/* ----------------------------------------------------------------------- */
SHLOCK DLLEXPORT	fnLockGet		( OBJID oLockedByP,
						  SHLOCK nLevelP,
						  OBJID oLockedP,
						  int nIndexP )
{
  LOCKLEVEL	nLockLevel;

  PROCEDURE	( fnLockGet );

  if ( ! fnSHlock2LevelMode ( (SHLOCK)
			      ( (unsigned int) nLevelP &
				(unsigned int) eshLockLevelMask ),
			      &nLockLevel,
			      (LPLOCKMODE) NULL, (LPBOOL) NULL,
			      (LPBOOL) NULL ) ) {
    char	szLockedBy [ 512 ];
    ERROR (( "Getting lock mode of locking object %s to locked"
	     " object %s failed; illegal lock-level requested.",
	     PrintObject ( oLockedByP, szLockedBy ),
	     fnPrintObject ( oLockedP, (LPSTR) NULL, 0 ) ));
    RETURN ( eshLockFailed );
  }

  RETURN ( fnGetLockOfLevel ( oLockedByP, nLockLevel, oLockedP, nIndexP ) );
} /* fnLockGet */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnLockInsertOnObject	( OBJID oLockBy,
						  SHLOCK nLock,
						  OBJID oToLock,
						  int nIndex,
						  SHLOCK * pnVectorLockNow )
{
  LOCKLEVEL	nLockLevel, l;
  LOCKMODE	nLockMode;
  BOOL		bForce;
  SHLOCK	nSHlockMode, nSHlockIntent, nSHlockVector, nSHlockCurrent;
  SHLOCK	nLockOld;

  PROCEDURE	( fnLockInsertOnObject );

  nLock	= (SHLOCK) ( (unsigned int) nLock & ~ (unsigned int) eshUnlock );

  if ( ! fnSHlock2LevelMode ( nLock, &nLockLevel, &nLockMode,
			      (LPBOOL) NULL, &bForce ) ) {
    char	szLockBy [ 512 ];
    if ( pnVectorLockNow != NULL ) {
      *pnVectorLockNow	= eshUnknownLockMode;
    }
    ERROR (( "Inserting locking object %s into object %s failed;"
	     " illegal lock-level or lock-mode requested.",
	     PrintObject ( oLockBy, szLockBy ),
	     fnPrintObject ( oToLock, (LPSTR) NULL, 0 ) ));
    RETURN ( eshLockFailed );
  }

  if ( nLockLevel == llNothing ) {
    if ( pnVectorLockNow != NULL ) {
      *pnVectorLockNow	= fnGetLockOfLevel ( oLockBy, llVector, oToLock, -1 );
    }
    if ( ! bForce && nLockMode != lmNothing ) {
      char	szLockBy [ 512 ];
      CERROR (( "Ignore insert lock request.",
	        "The locking object %s can't be inserted on level '%s'"
	        " with mode '%s' into object %s.",
	        PrintObject ( oLockBy, szLockBy ),
	        szLevelNothing, ppszModes [ nLockMode ],
	        fnPrintObject ( oToLock, (LPSTR) NULL, 0 ) ));
    }
    RETURN ( eshLockModeNothing );
  }

  nSHlockMode	= Mode2SHlock [ nLockMode ];
  nSHlockIntent	= Mode2SHlock [ nLockMode + lmIntent ];

  /* Now check if there is already a lock on current or a higher level;
     check at first the 'vector' level, because this is the level with
     the highest probability (and testing this level doesn't cost very
     much): */
  if ( nLockLevel <= llVector ) {
    nSHlockVector	= fnGetLockOfLevel ( oLockBy, llVector, oToLock, -1 );
    if ( nSHlockVector & nSHlockMode ) {
      /* Bingo: Object is locked on level eshLockLevelVector (and
         therefore, on all levels below llVector): */
      if ( pnVectorLockNow != NULL ) {
	*pnVectorLockNow	= nSHlockVector;
      }
      RETURN ( nSHlockVector );
    }
  } else if ( pnVectorLockNow != NULL ) {
    *pnVectorLockNow	= eshUnknownLockMode;
  }

  /* Check all levels. Start with highest level going down to
     nLockLevel: */
  for ( l = llStore; l >= nLockLevel; l = (LOCKLEVEL) ( (int) l - 1 ) ) {
    nSHlockCurrent	= ( l == llVector ) ?
      nSHlockVector : fnGetLockOfLevel ( oLockBy, l, oToLock, nIndex );
    if ( nSHlockCurrent & nSHlockMode ) {
      /* Bingo: object is locked on level l: */
      RETURN ( nSHlockCurrent );
    }
    if ( ! ( nSHlockCurrent & nSHlockIntent ) ) {
      /* Bingo: object can't have been locked on level l or a
	 sub-level because there is no intent lock: */
      nLockOld	= fnSetLockByLevelMode ( oLockBy, nLockLevel, nLockMode,
					 bForce, oToLock, nIndex,
					 pnVectorLockNow );
      RETURN ( nLockOld );
    }
  }

  /* Reaching here means that no lock was found; so insert it: */
  nLockOld	=
    fnSetLockByLevelMode ( oLockBy, nLockLevel, nLockMode, bForce,
			   oToLock, nIndex, pnVectorLockNow );
  RETURN ( nLockOld );
} /* fnLockInsertOnObject */

/* ----------------------------------------------------------------------- */
/* Insert the lock into oLockBy and into all of its dependent objects: */
static SHLOCK		fnLockInsertOnDependents( OBJID oLockBy,
						  SHLOCK nLock,
						  OBJID oToLock,
						  int nIndex,
						  SHLOCK * pnVectorLockNow,
						  PHPEEK phPeek )
{
  typedef struct {
    OBJID	oToLock;
    SHLOCK	nLockOld;
    SHLOCK	nLockNow;
  }		* PLOCKS;

  SHLOCK	nLockOld, nVectorLockNow, nLockNew;
  LPOBJID	poToLock = (LPOBJID) NULL;
  int		i, i0, j, n, nDependentMask = 0, nDependents = 0;
  PLOCKS	pLocks = (PLOCKS) NULL;

  PROCEDURE	( fnLockInsertOnDependents );

  nLockOld	= fnLockInsertOnObject ( oLockBy, nLock, oToLock,
					 nIndex, &nVectorLockNow );
  if ( pnVectorLockNow != NULL ) {
    *pnVectorLockNow	= nVectorLockNow;
  }
  if ( (int) nLockOld < 0 || phPeek == NULL ) {
    RETURN ( nLockOld );
  }

  switch ( nLock & eshLockModeMask ) {
  case eshLockModeRead:
    nDependentMask	= flagDependentRead;
    break;
  case eshLockModeWrite:
    nDependentMask	= flagDependentWrite;
    break;
  default:
    RETURN ( nLockOld );
  }

  /* A lock has been inserted successfully.  Iterate over all
     dependent objects and try to lock them too: */
  poToLock	= ( ObjId_is_valid ( oToLock ) ) ?
    SH_key_to_address ( oToLock ) : (LPOBJID) NULL;
  switch ( nLock & eshLockLevelMask ) {
  case eshLockLevelElement:
    ASSERT ( poToLock != NULL );
    /* If the following assert fails, the nIndex passed is invalid: */
    ASSERT ( nIndex >= 0 );
    i0		= eshSHvectorIdxFirstData + nIndex;
    /* If the following assert fails, the nIndex passed is invalid: */
    ASSERT ( i0 < eshSHvectorIdxFirstObjId +
	     poToLock [ eshSHvectorIdxObjIds ] );
    n		= i0 + 1;
    nLockNew	= (SHLOCK) 
      ( (unsigned int) eshLockLevelVector |
	( (unsigned int) nLock & (unsigned int) eshLockModeMask ) );
    break;
  case eshLockLevelVector:
    ASSERT ( poToLock != NULL );
    i0		= eshSHvectorIdxFirstData;
    n		= eshSHvectorIdxFirstObjId + poToLock [ eshSHvectorIdxObjIds ];
    nLockNew	= nLock;
    switch ( nVectorLockNow & eshLockLevelMask ) {
    case eshLockLevelElement:
      RETURN ( nLockOld );
    case eshLockLevelVector:
      if ( ( nLockOld & nLock & eshLockModeMask ) ==
	   ( nVectorLockNow & nLock & eshLockModeMask ) ) {
	/* Object was already locked: */
	RETURN ( nLockOld );
      }
      break;
    default:
      nVectorLockNow	= (SHLOCK)
        ( (unsigned int) eshLockLevelStore |
	  ( (unsigned int) nLock & (unsigned int) eshLockModeMask ) );
      break;
    }
    if ( *phPeek == NULLHPEEK ) {
      *phPeek	= fnPeekHandleCreate ( oToLock, nVectorLockNow );
    }
    if ( ! fnPeekHandleInsert ( *phPeek, oToLock ) ) {
      RETURN ( nLockOld );
    }
    break;
  case eshLockLevelStore:
    if ( poToLock == NULL ) {
      RETURN ( nLockOld );
    }
    i0	= eshSHvectorIdxFirstData;
    n	= eshSHvectorIdxFirstObjId + poToLock [ eshSHvectorIdxObjIds ];
    nLockNew	= nLock;
    if ( *phPeek == NULLHPEEK ) {
      *phPeek	= fnPeekHandleCreate ( oToLock, nVectorLockNow );
    }
    if ( ! fnPeekHandleInsert ( *phPeek, oToLock ) ) {
      RETURN ( nLockOld );
    }
    break;
  default:
    RETURN ( nLockOld );
  }

  /* Count the dependent objects: */
  ASSERT ( poToLock != NULL );
  for ( i = i0; i < n; i++ ) {
    if ( boundp ( poToLock [ i ] ) && ! immediatep ( poToLock [ i ] ) &&
	 ! ObjId_is_valid ( poToLock [ i ] ) ) {
      char	szElement [ 256 ], szToLock [ 256 ], szLockBy [ 256 ];
      ERROR (( "Encountered invalid\n"
	       "       objid %d (%s),\n"
	       "       allowed range %d .. %d\n"
	       "       in oToLock %s,\n"
	       "       at slot location %d,\n"
	       "       oLockBy %s",
	       LONG2SHORTOBJID ( poToLock [ i ] ),
	       PrintObject ( poToLock [ i ], szElement ),
	       LONG2SHORTOBJID ( oGlobalMinObjId ),
	       LONG2SHORTOBJID ( oGlobalMaxObjId ),
	       PrintObject ( oToLock, szToLock ),
	       i,
	       PrintObject ( oLockBy, szLockBy ) ));
      RETURN ( eshGeneralError );
    }
    if ( dependentp ( poToLock [ i ] ) & nDependentMask ) {
      /* 1996/11/07 HK: Debug: */
#if (LOGGING+0) & 0x04
      if ( GetFlagWord() & 0x04 ) {
	fprintf ( stderr, "%s(%d): %s: Handle %d, about to insert lock 0x%X"
		  " on objid %d\n",
		  __szFile__, __LINE__, __szProc__,
		  ( phPeek ) ? *phPeek : -1, nLockNew,
		  LONG2SHORTOBJID ( poToLock [ i ] ) );
      }
#endif /* #if (LOGGING+0) & 0x04 */
      nDependents++;
    }
  }

  if ( nDependents > 0 ) {
    pLocks	= (PLOCKS) Malloc ( nDependents * sizeof ( *pLocks ) );
    ASSERT ( pLocks != NULL );
    for ( i = i0, j = 0; i < n; i++ ) {
      if ( dependentp ( poToLock [ i ] ) & nDependentMask ) {
	pLocks [ j ].oToLock	= poToLock [ i ];
	pLocks [ j ].nLockOld	=
	  fnLockInsertOnDependents ( oLockBy, nLockNew,
				     pLocks [ j ].oToLock, -1, 
				     & pLocks [ j ].nLockNow, phPeek );
	if ( pLocks [ j ].nLockOld >= 0 ) {
	  j++;
	} else {
	  /* Locking the dependent object failed, so release all
	     other locks inserted here: */
	  for ( i = 0; i < j; i++ ) {
	    if ( pLocks [ i ].nLockOld != pLocks [ i ].nLockNow ) {
	      fnUnlock ( oLockBy, nLock, pLocks [ i ].oToLock,
			 -1, (SHLOCK *) NULL );
	    }
	  }
	  nLockOld	= pLocks [ j ].nLockOld;
	  break;
	}
      }
    }
    Free ( pLocks );
    pLocks	= (PLOCKS) NULL;
  }

  if ( (int) nLockOld < 0 && *phPeek != NULLHPEEK ) {
    *phPeek	= fnPeekHandleDestroy ( *phPeek );
  }

  RETURN ( nLockOld );
} /* fnLockInsertOnDependents */

/* ----------------------------------------------------------------------- */
/* Insert the lock into oLockBy and into all of its dependent objects: */
SHLOCK DLLEXPORT	fnLockInsert		( OBJID oLockBy,
						  SHLOCK nLock,
						  OBJID oToLock,
						  int nIndex,
						  PHPEEK phPeek )
{
  SHLOCK	nLockOld;

  PROCEDURE	( fnLockInsert );

  if ( phPeek != NULL ) {
    *phPeek	= NULLHPEEK;
  }
  nLockOld	= fnLockInsertOnDependents ( oLockBy, nLock, oToLock,
					     nIndex, (SHLOCK *) NULL, phPeek );
  /* 1996/11/07 HK: Debug: */
#if (LOGGING+0) & 0x04
  if ( ( GetFlagWord() & 0x04 ) ) {
    if ( phPeek != NULL && *phPeek != NULLHPEEK ) {
      PPEEKHANDLE	pPeekHandle;
      pPeekHandle	= fnPeekHandlePtr ( *phPeek );
      ASSERT ( pPeekHandle != NULL );
      fprintf ( stderr, "%s(%d): %s: Objid %d has %d objects,"
		" %d words ObjIds, %d words values\n",
		__szFile__, __LINE__, __szProc__,
		LONG2SHORTOBJID ( oToLock ),
		fnHashOccupied ( &pPeekHandle->ObjIds ),
		pPeekHandle->nObjIdWords, pPeekHandle->nValueWords );
    } else {
      fprintf ( stderr, "%s(%d): %s: Nothing to peek for objid %d\n",
		__szFile__, __LINE__, __szProc__,
		LONG2SHORTOBJID ( oToLock ) );
    }
  }
#endif /* #if (LOGGING+0) & 0x04 */
  RETURN ( nLockOld );
} /* fnLockInsert */

/* ----------------------------------------------------------------------- */
/* Partly from: postgres/src/backend/storage/lmgr/multi.c */
/* 1996/11/07 HK: Dependent locking not supported for lock setting,
   guess it won't be necessary in the near future: */
SHLOCK DLLEXPORT	fnLockSet		( OBJID oLockBy,
						  SHLOCK nLock,
						  OBJID oToLock,
						  int nIndex,
						  PHPEEK phPeek )
{
  LOCKLEVEL		nLockLevel;
  LOCKMODE		nLockMode;
  BOOL			bUnlock, bForce;

  PROCEDURE		( fnLockSet );

  if ( phPeek != NULL ) {
    *phPeek	= NULLHPEEK;
  }
  if ( ! fnSHlock2LevelMode ( nLock, &nLockLevel, &nLockMode,
			      &bUnlock, &bForce ) ) {
    char	szLockBy [ 512 ];
    ERROR (( "Setting locking object %s to object %s failed;"
	     " illegal lock-level or lock-mode requested.",
	     PrintObject ( oLockBy, szLockBy ),
	     fnPrintObject ( oToLock, (LPSTR) NULL, 0 ) ));
    RETURN ( eshLockFailed );
  }

  if ( bUnlock ) {
    RETURN ( fnUnlock ( oLockBy, nLock, oToLock, nIndex, (SHLOCK *) NULL ) );
  }

  RETURN ( fnSetLockByLevelMode ( oLockBy, nLockLevel, nLockMode, bForce,
				  oToLock, nIndex, (SHLOCK *) NULL ) );
} /* fnLockSet */

/* ----------------------------------------------------------------------- */
SHLOCK DLLEXPORT	fnSetLockRetry		( OBJID oLockBy,
						  SHLOCK nLock,
						  OBJID oToLock,
						  int nIndex,
						  PHPEEK phPeek )
{
  static const char	szBy []	= " by ";

  SHLOCK		nLockOld;
  char			szLockBy [ 512 ];

  PROCEDURE	( fnSetLockRetry );

  if ( ! boundp ( oLockBy ) )
    nLock	= eshLockModeNothing;

  while ( ( nLockOld = fnLockSet ( oLockBy, nLock, oToLock,
				   nIndex, phPeek ) ) ==
	  eshLockFailed ) {
    if ( oLockBy == NULLOBJID ) {
      szLockBy [ 0 ]	= '\0';
    } else {
      strcpy ( szLockBy, szBy );
      fnPrintObject ( oLockBy, & szLockBy [ sizeof ( szBy ) - 1 ],
		      sizeof ( szLockBy ) - sizeof ( szBy ) + 1 );
    }
    CERROR (( "Retry object locking.",
	      "Locking object %s%s failed.",
	      fnPrintObject ( oToLock, (LPSTR) NULL, 0 ), szLockBy ));
  }
  RETURN ( nLockOld );
} /* fnSetLockRetry */

/* ----------------------------------------------------------------------- */
/* Removes all oLockedBy locking objects from the locked object oToUnlock: */
int DLLEXPORT		fnUnlockAll		( OBJID oLockedBy,
						  OBJID oToUnlock )
{
  int		nReleased = 0;
  LOCKLEVEL	l;
  LPOBJID	lpSHvector, lpoLastSumLock, lpoLastOneLock;
  OBJID		oS, oNext;
  LPSUMLOCK	lpSumLockVector, lpS;
  LPONELOCK	lpOneLock;

  PROCEDURE	( fnUnlockAll );

  for ( l = llMin; l <= llMax; l = (LOCKLEVEL) ( (int) l + 1 ) ) {
    switch ( l ) {

    case llElement:
      if ( boundp ( oToUnlock ) ) {
	ASSERT_ObjId_is_valid ( oToUnlock );
	lpSHvector	= SH_key_to_address ( oToUnlock );
	ASSERT ( lpSHvector != NULL );
	lpSumLockVector	=
	  fnGetSumLock ( lpSHvector [ eshSHvectorIdxLockedBy ] );
	if ( lpSumLockVector == NULL ) {
	  break;
	}
	lpoLastSumLock	= &lpSumLockVector->Header.oNext;
	oS		= lpSumLockVector->Header.oNext;
	while ( boundp ( oS ) ) {
	  lpS		= fnGetSumLock ( oS );
	  lpOneLock	=
	    fnSearchOneLock ( lpS, oLockedBy, &lpoLastOneLock, FALSE );
	  oNext		= lpS->Header.oNext;
	  nReleased	+=
	    fnLockReleaseAll ( lpS, lpOneLock, oLockedBy, oToUnlock, l,
			       lpoLastSumLock, lpoLastOneLock );
	  oS		= oNext;
	  if ( lpS->Header.oNext == oNext ) {
	    /* No change in oNext-field ==> the sum-lock oS wasn't deleted: */
	    lpoLastSumLock	= &lpS->Header.oNext;
	  }
	}
      }
      break;

    case llVector:
      if ( boundp ( oToUnlock ) ) {
	ASSERT_ObjId_is_valid ( oToUnlock );
      } else {
	break;
      }

    default:
      lpS		=
	fnSearchSumLock ( l, oToUnlock, -1, &lpoLastSumLock, FALSE );
      lpOneLock		=
	fnSearchOneLock ( lpS, oLockedBy, &lpoLastOneLock, FALSE );
      nReleased		+=
	fnLockReleaseAll ( lpS, lpOneLock, oLockedBy, oToUnlock, l,
			   lpoLastSumLock, lpoLastOneLock );
      break;
    }
  }
  RETURN ( nReleased );
} /* fnUnlockAll */

/* ----------------------------------------------------------------------- */
/* Removes all locking objects from the locked object oToUnlock: */
int DLLEXPORT		fnUnlockAllAll		( OBJID oToUnlock )
{
  int		nReleased = 0;
  LPOBJID	lpSHvector;
  OBJID		oSumLock;
  LPSUMLOCK	lpSumLock;
  LPONELOCK	lpOneLock;
  int		i, n;

  PROCEDURE	( fnUnlockAllAll );

  if ( ! boundp ( oToUnlock ) ) {
    /* 1996/11/19 HK: Remove all root locks for passing an unbound
       object for oToUnlock. I'm not quite sure if this is a so good
       idea ... */
    nReleased	= ( boundp ( GetRootLock () ) ) ? 1 : 0;
    fnSetRootLock ( unbound );
    RETURN ( nReleased );
  }

  ASSERT_ObjId_is_valid ( oToUnlock );
  lpSHvector	= (LPOBJID) SH_key_to_address ( oToUnlock );
  ASSERT ( lpSHvector != NULL );
  oSumLock	= lpSHvector [ eshSHvectorIdxLockedBy ];
  while ( ( lpSumLock = fnGetSumLock ( oSumLock ) ) ) {
    lpOneLock	= fnGetOneLock ( lpSumLock->oOneLocks );
    if ( lpOneLock ) {
      nReleased	+=
	fnUnlockAll ( lpOneLock->Header.OfPlob.oLockedBy, oToUnlock );
      oSumLock	= lpSHvector [ eshSHvectorIdxLockedBy ];
    } else {
      oSumLock	= lpSumLock->Header.oNext;
    }
  }
  if ( nReleased > 0 ) {
    /* Unlock the dependent objects too: */
    n	= eshSHvectorIdxFirstObjId + lpSHvector [ eshSHvectorIdxObjIds ];
    for ( i = eshSHvectorIdxFirstData; i < n; i++ ) {
      if ( dependentp ( lpSHvector [ i ] ) ) {
	nReleased	+= fnUnlockAllAll ( lpSHvector [ i ] );
      }
    }
  }
  RETURN ( nReleased );
} /* fnUnlockAllAll */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		fnMakeReadOnly		( OBJID oSelf,
						  READONLYMODE nReadOnlyP )
{
  SHLOCK	nLock, nLockOld;

  PROCEDURE	( fnMakeReadOnly );

  INITIALIZEPLOB;

  if ( immediatep ( oSelf ) )
    /* An immediate object is always considered read-only: */
    RETURN ( TRUE );
  ASSERT_ObjId_is_valid ( oSelf );

  nLock		= (SHLOCK)
    ( (unsigned int) eshLockLevelVector | (unsigned int) eshLockModeReadOnly );
  switch ( nReadOnlyP ) {
  case eshReadOnly:
    /* Make an object read-only by placing a read-only lock onto itself: */
    nLockOld	= fnLockInsertOnObject ( oSelf, nLock, oSelf, -1,
					 (SHLOCK *) NULL );
    break;
  case eshReadWrite:
    nLockOld	= fnUnlock ( oSelf, nLock, oSelf, -1, (SHLOCK *) NULL );
    break;
  default:
    ERROR (( "Illegal value %d for parameter nReadOnlyP.",
	     nReadOnlyP ));
  case eshReadOnlyP:
    nLockOld	= eshUnlockFailed;
    break;
  }

  if ( (int) nLockOld < 0 ) {
    nLockOld	= fnLockGet ( oSelf, nLock, oSelf, -1 );
  }
  RETURN ( (BOOL) ( ( nLockOld & eshLockModeReadOnly ) != 0 ) );
} /* fnMakeReadOnly */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientLockPrint, "c-sh-lock-print",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortSelf )
		  and
		  argument ( SHLOCK, value_in, nLevel )
		  and
		  argument ( NUMERICSTDSTREAM, value_in, nStdStream ) ) )
{
  FIXNUM	nPrinted;
  OBJID		oSelf;
  FILE		FAR * lpStream;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oSelf		= Short2LongObjId ( oShortSelf );
  lpStream	= ( nStdStream == eshStdOut ) ? stdout : stderr;
  nPrinted	= fnPrintLock ( oSelf, nLevel, lpStream );

  UnstoreSession ();
  RETURN ( nPrinted );
} EndFunction ( fnClientLockPrint );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientTransactionLockGet, "c-sh-get-lock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockedByP )
		  and
		  argument ( SHLOCK, value_in, nLevelP )
		  and
		  argument ( SHORTOBJID, value_in, oShortLockedP )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagLockedP )
		  and
		  argument ( FIXNUM, value_in, nIndexP ) ) )
{
  SHLOCK	nLockOld;
  OBJID		oLockedByP, oLockedP;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oLockedByP	= ( oShortObjIdLockedByP != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdLockedByP ) : NULLOBJID;
  oLockedP	= Short2LongObjId ( oShortLockedP );

  if ( nTypeTagLockedP != NULLTYPETAG &&
       nTypeTagLockedP != typetagof ( oLockedP ) ) {
    UNEXPECTED_TYPE_TAG ( oLockedP, -1, nTypeTagLockedP );
    UnstoreSession ();
    RETURN ( eshLockFailed );
  }

  nLockOld	= fnLockGet ( oLockedByP, nLevelP, oLockedP, nIndexP );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnClientTransactionLockGet );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerTransactionLockInsert, "c-sh-insert-lock",
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
		  argument ( FIXNUM, value_in, nIndex )
		  and	/* Additional output for client caching: */
		  argument ( FIXNUM, value_out, phPeek )
		  and
		  argument ( FIXNUM, value_out, pnObjIdWords ) ) )
{
  OBJID		oLockBy, oToLock;
  SHLOCK	nLockOld;
  PPEEKHANDLE	pPeekHandle;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  if ( phPeek != NULL ) {
    *phPeek	= NULLHPEEK;
  }
  if ( pnObjIdWords != NULL ) {
    *pnObjIdWords	= 0;
  }
  if ( ! ( nLock & eshLockPeek ) ) {
    phPeek	= (FIXNUM *) NULL;
  }

  oLockBy	= ( oShortObjIdLockBy != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdLockBy ) : NULLOBJID;

  if ( ( nLock & eshLockLevelMask ) <= eshLockLevelVector ) {
    oToLock	= Short2LongObjId ( oShortToLock );
    if ( nTypeTagToLock != NULLTYPETAG &&
	 nTypeTagToLock != typetagof ( oToLock ) ) {
      UNEXPECTED_TYPE_TAG ( oToLock, -1, nTypeTagToLock );
      UnstoreSession ();
      RETURN ( eshLockFailed );
    }
  } else {
    oToLock	= NULLOBJID;
  }

  nLockOld	=
    fnLockInsert ( oLockBy, nLock, oToLock, nIndex, (PHPEEK) phPeek );
  if ( phPeek != NULL && *phPeek != NULLHPEEK ) {
    pPeekHandle	= fnPeekHandlePtr ( *phPeek );
    ASSERT ( pPeekHandle != NULL );
    if ( pnObjIdWords != NULL ) {
      *pnObjIdWords	= pPeekHandle->nObjIdWords;
    }
  }

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerTransactionLockInsert );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerTransactionLockSet, "c-sh-set-lock",
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
		  argument ( FIXNUM, value_in, nIndex )
		  and	/* Additional output for client caching: */
		  argument ( FIXNUM, value_out, phPeek )
		  and
		  argument ( FIXNUM, value_out, pnObjIdWords ) ) )
{
  OBJID		oLockBy, oToLock;
  SHLOCK	nLockOld;
  PPEEKHANDLE	pPeekHandle;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  if ( phPeek != NULL ) {
    *phPeek	= NULLHPEEK;
  }
  if ( pnObjIdWords != NULL ) {
    *pnObjIdWords	= 0;
  }
  if ( ! ( nLock & eshLockPeek ) ) {
    phPeek	= (FIXNUM *) NULL;
  }

  oLockBy	= ( oShortObjIdLockBy != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdLockBy ) : NULLOBJID;
  oToLock	= Short2LongObjId ( oShortToLock );

  if ( nTypeTagToLock != NULLTYPETAG &&
       nTypeTagToLock != typetagof ( oToLock ) ) {
    UNEXPECTED_TYPE_TAG ( oToLock, -1, nTypeTagToLock );
    UnstoreSession ();
    RETURN ( eshLockFailed );
  }

  nLockOld	=
    fnLockSet ( oLockBy, nLock, oToLock, nIndex, (PHPEEK) phPeek );
  if ( phPeek != NULL && *phPeek != NULLHPEEK ) {
    pPeekHandle	= fnPeekHandlePtr ( *phPeek );
    ASSERT ( pPeekHandle != NULL );
    if ( pnObjIdWords != NULL ) {
      *pnObjIdWords	= pPeekHandle->nObjIdWords;
    }
  }

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerTransactionLockSet );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerTransactionUnlock, "c-sh-unlock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		  and
		  argument ( SHLOCK, value_in, nLock )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and	/* Additional output for client caching: */
		  argument ( SHLOCK, value_out,
			     pnVectorLockNow ) ) )
{
  SHLOCK	nLockOld;
  OBJID		oLockedBy, oToUnlock;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oLockedBy	= ( oShortObjIdLockedBy != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdLockedBy ) : NULLOBJID;
  oToUnlock	= Short2LongObjId ( oShortToUnlock );
  nLockOld	= fnUnlock ( oLockedBy, nLock, oToUnlock, nIndex,
			     pnVectorLockNow );
  
  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerTransactionUnlock );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnServerTransactionUnlockAll, "c-sh-unlock-all",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock ) ) )
{
  SHLOCK	nLockOld;
  OBJID		oLockedBy, oToUnlock;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oLockedBy	= Short2LongObjId ( oShortObjIdLockedBy );
  oToUnlock	= Short2LongObjId ( oShortToUnlock );
  nLockOld	= fnUnlockAll ( oLockedBy, oToUnlock );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerTransactionUnlockAll );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnServerTransactionUnlockAllAll, "c-sh-unlock-all-all",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock ) ) )
{
  FIXNUM	nUnlocked;
  OBJID		oToUnlock;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oToUnlock	= ( boundp ( SHORT2LONGOBJID ( oShortToUnlock ) ) ) ?
    Short2LongObjId ( oShortToUnlock ) : NULLOBJID;
  nUnlocked	= fnUnlockAllAll ( oToUnlock );
  
  UnstoreSession ();
  RETURN ( nUnlocked );
} EndFunction ( fnServerTransactionUnlockAllAll );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
	        fnShortMakeReadOnly, "c-sh-read-only",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortSelf )
		  and
		  argument ( READONLYMODE, value_in, nReadOnlyP ) ) )
{
  BOOL	bDone;
  OBJID	oSelf;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }
  ASSERT ( StableHeap_is_open );

  oSelf	= Short2LongObjId ( oShortSelf );
  bDone	= fnMakeReadOnly ( oSelf, nReadOnlyP );

  UnstoreSession ();
  RETURN ( bDone );
} EndFunction ( fnShortMakeReadOnly );

/* ----------------------------------------------------------------------- */
static int		fnLockWait		( OBJID oSumLock,
						  OBJID oOneLock,
						  LOCKMODE nLockMode )
{
  LPONELOCK	lpOneLock;
  LPSUMLOCK	lpSumLock;
  int		nLockCount;
  char		szObject [ 256 ], szReason [ 640 ];

  PROCEDURE	( fnLockWait );

  nLockCount	= 0;
  /* Insert lock request into wait queue: */
  fnLockEnqueue ( oSumLock, oOneLock, nLockMode );
  /* Call caller-suspend to suspend the current process: */
  lpOneLock	= fnGetOneLock ( oOneLock );
  lpSumLock	= fnGetSumLock ( oSumLock );
#if (LOGGING+0) & 0x01
  fprintf ( stderr, "%s(%d): Setting objid %d to sleep\n",
	    __szProc__, __LINE__,
	    LONG2SHORTOBJID ( lpOneLock->Header.OfPlob.oLockedBy ) );
#endif /* #if (LOGGING+0) & 0x01 */
  PrintObject ( lpSumLock->Header.OfPlob.oLockedBy, szObject );
  sprintf ( szReason,
	    "Waiting for lock level %s, mode %s on object\n"
	    "       %.256s by\n"
	    "       %.256s.",
	    ppszLevels [ fnSumLockLevel ( lpSumLock ) ],
	    ppszModes [ nLockMode ], szObject,
	    fnPrintObject ( lpOneLock->Header.OfPlob.oLockedBy,
			    (LPSTR) NULL, 0 ) );
  if ( fnPLOBsuspendCallback ( lpOneLock->Header.OfPlob.oLockedBy,
			       lpSumLock->Header.OfPlob.oLockedBy,
			       szReason ) ) {
    /* If the following ASSERT fails, someone has deleted the SumLock
       we have been waiting for: */
    ASSERT ( boundp ( lpSumLock->Header.onTotalHold ) );
    /* If the following ASSERT fails, someone has deleted the OneLock
       we have been waiting for: */
    ASSERT ( boundp ( lpOneLock->Header.onTotalHold ) );
    /* When the suspend was successfull, nLockCount is now > 0
       (because it was set by someone who woke me up): */
    nLockCount	=
      ObjId2Fixnum ( lpOneLock->Header.oTotalHolds [ nLockMode ] );
  } else {
    nLockCount	= 0;
  }
  if ( nLockCount <= 0 ) {
#if (LOGGING+0) & 0x01
    fprintf ( stderr, "%s(%d): Setting objid %d to sleep failed.\n",
	      __szProc__, __LINE__,
	      LONG2SHORTOBJID ( lpOneLock->Header.OfPlob.oLockedBy ) );
#endif /* #if (LOGGING+0) & 0x01 */
    /* The suspend failed; de-queue the lock: */
    fnLockDequeue ( oSumLock, oOneLock );
  }
  RETURN ( nLockCount );
} /* fnLockWait */

/* ----------------------------------------------------------------------- */
static int		fnLockWakeup		( OBJID oSumLock )
{
  int		nWakedup;
  OBJID		oFirst, oQueue;
  LPSUMLOCK	lpSumLock;
  LPONELOCK	lpOneLock;
  LOCKMODE	nLockMode;
  LOCKLEVEL	nLockLevel;
  LPQUEUENTRY	lpQueue;
  BTREERESULT	nUpdated, nDeleted;
  char		szObject [ 256 ], szReason [ 640 ];

  PROCEDURE	( fnLockWakeup );

  nWakedup	= 0;
  makunbound ( oFirst );
  BTreeSearchByObjId ( NULLOBJID, GetRootLockQueue (),
		       oSumLock, NULL, &oFirst );
  if ( boundp ( oFirst ) ) {
    /* Traverse the oNext-pointer for processes to wake up: */
    lpSumLock	= fnGetSumLock ( oSumLock );
    /* If one of the following ASSERT fails, someone has deleted
       the SumLock we have been waiting for: */
    ASSERT ( lpSumLock );
    ASSERT ( boundp ( lpSumLock->Header.onTotalHold ) );
    nLockLevel	= fnSumLockLevel ( lpSumLock );
    oQueue	= oFirst;
    lpQueue	= fnGetQueueEntry ( oQueue );
    AtomicLock ( GetRootLockQueue (), GetRootLockQueue () );
    while ( lpQueue ) {
      lpOneLock	= fnGetOneLock ( lpQueue->oOneLock );
      nLockMode	= (LOCKMODE) ObjId2Fixnum ( lpQueue->onLockMode );
      if ( fnResolveConflicts ( lpSumLock, lpOneLock, nLockMode ) ) {
	/* Remove the entry from the queue: */
	oQueue	= lpQueue->oNext;
	fnDestroyObject ( lpQueue->oSelf, FALSE );
	/* Grant the process the lock ... */
	fnLockGrant ( lpSumLock, nLockMode );
	/* ... and wake it up: */
#if (LOGGING+0) & 0x01
	fprintf ( stderr, "%s(%d): Waking up objid %d\n",
		  __szProc__, __LINE__,
		  LONG2SHORTOBJID ( lpOneLock->Header.OfPlob.oLockedBy ) );
#endif /* #if (LOGGING+0) & 0x01 */
	PrintObject ( lpSumLock->Header.OfPlob.oLockedBy, szObject );
	sprintf ( szReason,
		  "Received lock level %s, mode %s on object\n"
		  "       %.256s by\n"
		  "       %.256s.",
		  ppszLevels [ fnSumLockLevel ( lpSumLock ) ],
		  ppszModes [ nLockMode ], szObject,
		  fnPrintObject ( lpOneLock->Header.OfPlob.oLockedBy,
				  (LPSTR) NULL, 0 ) );
	if ( fnPLOBwakeupCallback ( lpOneLock->Header.OfPlob.oLockedBy,
				    lpSumLock->Header.OfPlob.oLockedBy,
				    szReason ) ) {
	  nWakedup++;
	}
	lpQueue	= fnGetQueueEntry ( oQueue );
      } else {
	break;
      }
    }
    if ( oQueue != oFirst ) {
      if ( boundp ( oQueue ) ) {
	nUpdated	=
	  fnBTreeInsertByObjId ( NULLOBJID, GetRootLockQueue (),
				 oSumLock, oQueue );
	/* If this assert fails, the queue entry wasn't updated as
	   expected: */
	ASSERT ( nUpdated == btreeUpdated );
      } else {
	nDeleted	=
	  fnBTreeDeleteByObjId ( NULLOBJID, GetRootLockQueue (), oSumLock );
	/* If this assert fails, the queue entry wasn't deleted as
	   expected: */
	ASSERT ( nDeleted == btreeDeleted );
      }
    }
    AtomicUnlock ( GetRootLockQueue (), GetRootLockQueue () );
  }

  RETURN ( nWakedup );
} /* fnLockWakeup */

/* ----------------------------------------------------------------------- */
static LPQUEUENTRY	fnLockEnqueue		( OBJID oSumLock,
						  OBJID oOneLock,
						  LOCKMODE nLockMode )
{
  int		nPriority;
  OBJID		oNew, oQueue;
  LPQUEUENTRY	lpNew, lpQueue, lpLast;

  PROCEDURE	( fnLockEnqueue );

  oNew			=
    fnCreateObject ( (SHTYPETAG) eshQueueEntryTag, 0, NULLTYPETAG, 0 );
  lpNew			= fnGetQueueEntry ( oNew );
  nPriority		= nPriorities [ nLockMode ];
  lpNew->onPriority	= Fixnum2ObjId ( nPriority );
  lpNew->onLockMode	= Fixnum2ObjId ( nLockMode );
  lpNew->oOneLock	= oOneLock;
  makunbound ( oQueue );
  BTreeSearchByObjId ( NULLOBJID, GetRootLockQueue (),
		       oSumLock, NULL, &oQueue );
  if ( boundp ( oQueue ) ) {
    /* Traverse the oNext-pointer until a queue entry with its priority <=
       nPriority is found: */
    lpLast	= (LPQUEUENTRY) NULL;
    lpQueue	= fnGetQueueEntry ( oQueue );
    AtomicLock ( GetRootLockQueue (), GetRootLockQueue () );
    while ( lpQueue && ObjId2Fixnum ( lpQueue->onPriority ) > nPriority ) {
      lpLast	= lpQueue;
      lpQueue	= fnGetQueueEntry ( lpQueue->oNext );
    }
    /* lpQueue now points to the queue entry whose priority is <= nPriority.
       Put the queue entry into the chain: */
    if ( lpQueue )
      lpNew->oNext	= lpQueue->oSelf;
    if ( lpLast ) {
      lpLast->oNext	= oNew;
    } else {
      fnBTreeInsertByObjId ( NULLOBJID, GetRootLockQueue (), oSumLock, oNew );
    }
    AtomicUnlock ( GetRootLockQueue (), GetRootLockQueue () );
  } else {
    /* This is the first object to insert: */
    fnBTreeInsertByObjId ( NULLOBJID, GetRootLockQueue (), oSumLock, oNew );
  }

  RETURN ( lpNew );
} /* fnLockEnqueue */

/* ----------------------------------------------------------------------- */
static void		fnLockDequeue		( OBJID oSumLock,
						  OBJID oOneLock )
{
  OBJID		oQueue;
  LPQUEUENTRY	lpQueue, lpLast;

  PROCEDURE	( fnLockDequeue );

  makunbound ( oQueue );
  BTreeSearchByObjId ( NULLOBJID, GetRootLockQueue (),
		       oSumLock, NULL, &oQueue );
  if ( boundp ( oQueue ) ) {
    /* Traverse the oNext-pointer until a queue entry with
       its oOnelock == oOneLock is found: */
    lpLast	= (LPQUEUENTRY) NULL;
    lpQueue	= fnGetQueueEntry ( oQueue );
    AtomicLock ( GetRootLockQueue (), GetRootLockQueue () );
    while ( lpQueue && lpQueue->oOneLock != oOneLock ) {
      lpLast	= lpQueue;
      lpQueue	= fnGetQueueEntry ( lpQueue->oNext );
    }
    /* Remove the queue entry from the chain: */
    if ( lpQueue ) {
      /* Found the entry: */
      if ( lpLast ) {
	/* There is at least one entry before the current entry: */
	lpLast->oNext	= lpQueue->oNext;
      } else if ( boundp ( lpQueue->oNext ) ) {
	/* This is the first entry: */
	fnBTreeInsertByObjId ( NULLOBJID, GetRootLockQueue (),
			       oSumLock, lpQueue->oNext );
      } else {
	/* This is the first and only entry: */
	fnBTreeDeleteByObjId ( NULLOBJID, GetRootLockQueue (), oSumLock );
      }
      fnDestroyObject ( lpQueue->oSelf, FALSE );
    }
    AtomicUnlock ( GetRootLockQueue (), GetRootLockQueue () );
  }

  RETURN ( VOID );
} /* fnLockDequeue */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
