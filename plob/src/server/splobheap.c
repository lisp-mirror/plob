/* -------------------------------------------------------------------------
| Module	splobheap.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94 Derived from c-plob.c
| Description	Transactions
|		After every write to a transaction file there should be
|		a fflush () to force the transaction files to be always
|		actual on disk and not only in the transaction files
|		stream's internal buffers. The point at which flushing
|		is done is determined by the flush mode returned from
|		GetFlushMode ().
|
| Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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

#include	<errno.h>
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
#include	"trmalloc.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobtype.h"
#include	"splobnumber.h"
#include	"splobbtree.h"
#include	"splobroot.h"
#include	"sploblock.h"
#include	"splobheap.h"
#include	"splobstruct.h"
#include	"splobclos.h"

/* HK 18.4.94: test: */
#include	"splobsequ.h"

#define		RPCNOTYPES
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
#if 0
#define	LOGGING
#endif

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| The POSTORE long objids start at nGlobalMaxObjId going down to
| nGlobalMinObjId; a 'fixed-for-lifetime'-effect for long objids
| can be obtained by setting oFixedObjId = oGlobalMaxObjId - oObjId
| resp. oObjId = oGlobalMaxObjId - oFixedObjId
 ------------------------------------------------------------------------- */

static OBJID		__oVar2Fix__	= NULLOBJID;

/* Change a long objid to an objid with its extent fixed for lifetime: */
#define	Var2FixObjId( oObjId )		\
(__oVar2Fix__=(oObjId),			\
 (immediatep(__oVar2Fix__))?		\
 __oVar2Fix__:				\
 (oGlobalMaxObjId-(__oVar2Fix__)))

/* Change an objid with its extent fixed for lifetime to a long objid: */
#define	Fix2VarObjId( oFixObjId )	\
(__oVar2Fix__=(oFixObjId),		\
 (immediatep(__oVar2Fix__))?		\
 __oVar2Fix__:				\
 (oGlobalMaxObjId-(__oVar2Fix__)))

/* -------------------------------------------------------------------------
| Module initialization
 ------------------------------------------------------------------------- */
static time_t	timeGlobalStart		= 0;

enum {
  /* Stream position tag indicating that the sh-vector is not yet saved: */
  nPosNotSavedTag	= -1
};

/* This is the structure stored into the PLOBHEAP's hash table
   TractIdxTable. It contains informations about all objects locked in
   a transaction: */
typedef struct {
  OBJID		oFix;		/* Objid in 'fix' format */
  int		nTractId;	/* Current transaction id */
  SHLOCK	nLock;		/* Lock word of object oFix */
  long		nPosIndex;	/* Position of record in index stream */
  long		nPosHeap;	/* Position of sh-vector in heap stream */
}	TRACTIDX, FAR * LPTRACTIDX;

#define	FGETPOS( lpFilePos )		((lpFilePos)->nPos)
#define	FSETPOS( lpFilePos, nSet )	((lpFilePos)->nPos=(nSet))
#define	FINCPOS( lpFilePos, nInc )	((lpFilePos)->nPos+=(nInc))

/* -------------------------------------------------------------------------
| Static types, variables
 ------------------------------------------------------------------------- */
typedef enum {
  indexFile,
  heapFile
}	FILEKIND, FAR * LPFILEKIND;

/* Transaction log file name. The first "%s" is replaced by the directory
   in which the stable heap resides; the second "%s" is replaced by the
   FILEKIND ( "idx" for index files, "log" for heap files); the "%X" is
   replaced by the transaction log id: */
static CONST char	szTractFileFormat []	= "%s/tr%s.%X";

/* -------------------------------------------------------------------------
| Error message formats
 ------------------------------------------------------------------------- */
static CONST char	szNoTransaction []	=
"There is no active transaction\n"
"       on %s.";

static CONST char	szOpenIndexFailed []	=
"Opening of transaction index file\n"
"       for %s\n"
"       failed. errno is %d.";

static CONST char	szUnexpectedKind []	=
"Unexpected file kind %d.";

/* -------------------------------------------------------------------------
| Static function declarations
 ------------------------------------------------------------------------- */
static BOOL		mfnHeapInitializeInstance( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnHeapPrintObjectDetails( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static LPCSTR		mfnHeapLockAcquired	( OBJID oSelf,
						  OBJID oLocked,
						  SHLOCK nLockOld,
						  int nLockCount,
						  SHLOCK nLockNew );
static void		mfnHeapLockReleased	( OBJID oSelf,
						  OBJID oLocked,
						  SHLOCK nLockOld,
						  int nLockCount,
						  SHLOCK nLockNew );
static BOOL		mfnHeapObjectStateChanged( OBJID oSelf,
						  OBJID oLocked );
static BOOL		mfnHeapDestroy		( OBJID oSelf,
						  BOOL bKill );
static void		mfnHeapFlush		( OBJID oSelf );
static LPHASHTABLE	fnRestoreIdxTable	( LPPLOBHEAP lpHeap,
						  int nTractId,
						  BOOL bIgnoreError );
static LPPLOBHEAP	fnGetPlobHeap		( OBJID oSelf,
						  BOOL bIgnoreError );
static int		fnRead			( LPVOID lpItem,
						  size_t nSizeOfItem,
						  size_t nNumberOfItems,
						  LPFILEPOS lpFilePos );
static size_t		fnWrite			( LPVOID lpItem,
						  size_t nSizeOfItem,
						  size_t nNumberOfItems,
						  LPFILEPOS lpFilePos );
static long		fnSeek			( LPFILEPOS lpFilePos,
						  long nPos,
						  int nMode );
static void		fnFlush			( LPPLOBHEAP lpHeap );
static LPSTR		fnTractFilename		( LPPLOBHEAP lpHeap,
						  FILEKIND nKind,
						  LPSTR lpszBuf,
						  size_t nBuf );
static LPFILE		fnTractOpen		( LPPLOBHEAP lpHeap,
						  FILEKIND nKind,
						  LPCSTR lpszMode );
static void		fnTractClose		( LPPLOBHEAP lpHeap,
						  BOOL bFlush );
static void		fnClose			( LPFILEPOS	pFilePos );
static void		fnTractUnlink		( LPPLOBHEAP lpHeap );
static LPTRACTIDX	fnSaveTractIdx		( LPPLOBHEAP lpHeap,
						  LPTRACTIDX lpTractIdx );
static LPTRACTIDX	fnSaveVectorLock	( LPPLOBHEAP lpHeap,
						  OBJID oLocked,
						  SHLOCK nLockNew );
static BOOL		fnSaveLock		( LPPLOBHEAP lpHeap,
						  OBJID oLocked,
						  SHLOCK nLock );
static BOOL		fnUnsaveLock		( LPPLOBHEAP lpHeap,
						  OBJID oLocked,
						  SHLOCK nLock );
static BOOL		fnSaveObject		( LPPLOBHEAP lpHeap,
						  OBJID oToSave,
						  LPOBJID lpSHvector );
static BOOL		fnTractRollback		( LPPLOBHEAP lpHeap,
						  int nTractId );
static HASHENUM		fnTractUnlock		( LPHASHTABLE lpTractIdxTable,
						  HASHKEY nObjIdFix,
						  LPVOID lpTractIdx,
						  size_t nData,
						  LPVOID lpUserData );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeHeapModule		( void )
{
  PROCEDURE	( fnInitializeHeapModule );

  time ( &timeGlobalStart );

  /* Make sure that the struct's components offsets match the sh-vector
     indices. If one of the following ASSERTs fails, the eshHeapIdx...-
     constants have been modified without reflecting these modifications
     in the corresponding structs PLOBHEAP (or vice versa): */
  ASSERT ( Offset_matches_Index ( PLOBHEAP, oSelf,
				  Cooked2RawIndex ( eshHeapIdxSelf ) ) );
  ASSERT ( Offset_matches_Index ( PLOBHEAP, oTractId,
				  Cooked2RawIndex ( eshHeapIdxTractId ) ) );
  ASSERT ( Offset_matches_Index ( PLOBHEAP, oTractLogId,
				  Cooked2RawIndex ( eshHeapIdxTractLogId ) ) );

  RegisterMethod ( eshHeapTag, gfnInitializeInstance,
		   mfnHeapInitializeInstance );
  RegisterMethod ( eshHeapTag, gfnPrintObjectDetails,
		   mfnHeapPrintObjectDetails );
  RegisterMethod ( eshHeapTag, gfnLockAcquired, mfnHeapLockAcquired );
  RegisterMethod ( eshHeapTag, gfnLockReleased, mfnHeapLockReleased );
  RegisterMethod ( eshHeapTag, gfnObjectStateChanged,
		   mfnHeapObjectStateChanged );
  RegisterMethod ( eshHeapTag, gfnDestroy, mfnHeapDestroy );
  RegisterMethod ( eshHeapTag, gfnFlush, mfnHeapFlush );

  RETURN ( VOID );
} /* fnInitializeHeapModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeHeapModule	( void )
{
  PROCEDURE	( fnDeinitializeHeapModule );

  RETURN ( VOID );
} /* fnDeinitializeHeapModule */

/* -------------------------------------------------------------------------
| Methods
 ------------------------------------------------------------------------- */
static BOOL		mfnHeapInitializeInstance( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnHeapInitializeInstance );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  ((LPPLOBHEAP)lpSHvector)->oSelf	= oObjId;
  ((LPPLOBHEAP)lpSHvector)->oTractLogId	=
      FIXNUM2OBJID ( LONG2SHORTOBJID ( Var2FixObjId ( oObjId ) ) );
  ((LPPLOBHEAP)lpSHvector)->oIdxWritten	= o0;
  RETURN ( (BOOL) TRUE );
} /* fnHeapInitializeInstance */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnHeapPrintObjectDetails( OBJID oObjId,
						   LPOBJID lpSHvector,
						   LPCLASSINFO lpClassInfo,
						   LPSTR lpszBuffer,
						   size_t nBuffer )
{
  int		i	= 0;

  PROCEDURE	( mfnHeapPrintObjectDetails );

  sprintf ( lpszBuffer, "%s@%s",
	    gfnNameOf ( ((LPPLOBHEAP)lpSHvector)->oUser, (LPINT) NULL ),
	    gfnNameOf ( ((LPPLOBHEAP)lpSHvector)->oMachine, (LPINT) NULL ) );
  i				= strlen ( lpszBuffer );
  if ( boundp ( ((LPPLOBHEAP)lpSHvector)->oDescription ) ) {
    if ( i > 0 ) {
      lpszBuffer [ i++ ]	= ' ';
    }
    gfnPrintObjectDetails ( ((LPPLOBHEAP)lpSHvector)->oDescription,
			    & lpszBuffer [ i ], nBuffer - i );
    i				+= strlen ( & lpszBuffer [ i ] );
  }
  if ( boundp ( ((LPPLOBHEAP)lpSHvector)->oTractId ) ) {
    if ( i > 0 ) {
      lpszBuffer [ i++ ]	= ' ';
    }
    sprintf ( & lpszBuffer [ i ], "transaction=%d",
	      OBJID2FIXNUM ( ((LPPLOBHEAP)lpSHvector)->oTractId ) );
  }
  RETURN ( lpszBuffer );
} /* mfnHeapPrintObjectDetails */

/* ----------------------------------------------------------------------- */
static LPCSTR		mfnHeapLockAcquired	( OBJID oSelf,
						  OBJID oLocked,
						  SHLOCK nLockOld,
						  int nLockCount,
						  SHLOCK nLockNew )
{
  static const char	szNoTransaction []			=
    "There is no active transaction on the heap.";
  static const char	szSaveFailed []				=
    "Saving the locked object's state to the transaction log failed.";
  static const char	szLockButNoTransaction []		=
    "Hmmmm: Object is locked, but there is no active transaction.";

  LPPLOBHEAP	lpHeap;

  PROCEDURE	( mfnHeapLockAcquired );

  lpHeap	= fnGetPlobHeap ( oSelf, (BOOL) FALSE );
  if ( nLockCount == 1 ) {
    /* Object was locked the first time: */
    if ( ! boundp ( lpHeap->oTractId ) ) {
      RETURN ( szNoTransaction );
    }
    RETURN ( ( fnSaveLock ( lpHeap, oLocked, nLockNew ) ) ?
	     (LPCSTR) NULL : szSaveFailed );
  } else if ( ! boundp ( lpHeap->oTractId ) ) {
    char	szHeap [ 128 ];
    fnFlush ( lpHeap );
    CERROR (( "Remove all locks from object.",
	      "Object %s\n"
	      "       is locked by %s\n"
	      "       although there is no active transaction.",
	      fnPrintObject ( oLocked, (LPSTR) NULL, 0 ),
	      PrintObject ( oSelf, szHeap ) ));
    fnUnlockAll ( oSelf, oLocked );
    RETURN ( szLockButNoTransaction );
  }
  RETURN ( (LPCSTR) NULL );
} /* mfnHeapLockAcquired */

/* ----------------------------------------------------------------------- */
static void		mfnHeapLockReleased	( OBJID oSelf,
						  OBJID oLocked,
						  SHLOCK nLockOld,
						  int nLockCount,
						  SHLOCK nLockNew )
{
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( mfnHeapLockReleased );

  if ( nLockCount == 1 ) {
    lpHeap	= fnGetPlobHeap ( oSelf, (BOOL) FALSE );
    /* Before a call to unlock there should have been a call to
       lock; so it should be sure that oSelf is always in an active
       transaction: */
    if ( ! boundp ( lpHeap->oTractId ) ) {
      fnFlush ( lpHeap );
      ERROR (( szNoTransaction, fnPrintObject ( oSelf, (LPSTR) NULL, 0 ) ));
      RETURN ( VOID );
    }
    fnUnsaveLock ( lpHeap, oLocked, nLockNew );
  }
  RETURN ( VOID );
} /* mfnHeapLockReleased */

/* ----------------------------------------------------------------------- */
static BOOL		mfnHeapObjectStateChanged( OBJID oSelf,
						  OBJID oLocked )
{
  LPPLOBHEAP	lpHeap;
  LPOBJID	lpSHvector;

  PROCEDURE	( mfnHeapObjectStateChanged );

  lpHeap	= fnGetPlobHeap ( oSelf, (BOOL) FALSE );

  /* Make sure that oSelf is in an active transaction: */
  if ( ! boundp ( lpHeap->oTractId ) ) {
    fnFlush ( lpHeap );
    ERROR (( szNoTransaction, fnPrintObject ( oSelf, (LPSTR) NULL, 0 ) ));
    RETURN ( (BOOL) FALSE );
  }
  lpSHvector	= AtomicLock ( oLocked, oSelf );
  SETFLAGBIT ( lpSHvector [ eshSHvectorIdxTypeTag ], flagDirty, (BOOL) TRUE );
  AtomicUnlock ( oLocked, oSelf );
  RETURN ( fnSaveObject ( lpHeap, oLocked, lpSHvector ) );
} /* mfnHeapObjectStateChanged */

/* ----------------------------------------------------------------------- */
static BOOL		mfnHeapDestroy		( OBJID oSelf,
						  BOOL bKill )
{
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( mfnHeapDestroy );

  lpHeap	= fnGetPlobHeap ( oSelf, (BOOL) FALSE );
  if ( boundp ( lpHeap->oTractId ) ) {
    if ( bKill ) {
      /* Before killing a heap with an active transaction,
         do a rollback: */
      fnTractRollback ( lpHeap, ObjId2Fixnum ( lpHeap->oTractId ) );
      fnHashDestroy ( &lpHeap->TractIdxTable );
    } else {
      fnFlush ( lpHeap );
      CERROR (( "Ignore request to destroy the heap object.",
		"Request to destroy heap\n"
		"       %s\n"
		"       with active transaction %d.",
		fnPrintObject ( oSelf, (LPSTR) NULL, 0 ),
		OBJID2FIXNUM ( lpHeap->oTractId ) ));
      RETURN ( (BOOL) FALSE );
    }
  } else {
    fnHashDestroy ( &lpHeap->TractIdxTable );
  }
  fnTractUnlink ( lpHeap );
  RETURN ( (BOOL) TRUE );
} /* mfnHeapDestroy */

/* ----------------------------------------------------------------------- */
static void		mfnHeapFlush		( OBJID oSelf )
{
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( mfnHeapFlush );

  lpHeap	= fnGetPlobHeap ( oSelf, (BOOL) FALSE );
  fnFlush ( lpHeap );
} /* mfnHeapFlush */

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
static LPHASHTABLE	fnRestoreIdxTable	( LPPLOBHEAP lpHeap,
						  int nTractId,
						  BOOL bIgnoreError )
{
  TRACTIDX		TractIdx;

  PROCEDURE		( fnRestoreIdxTable );

  if ( boundp ( lpHeap->oGCcounter ) &&
       ObjId2Fixnum ( lpHeap->oGCcounter ) != GetGCcounter () ) {
    fnFlush ( lpHeap );
    if ( ! bIgnoreError ) {
      WARN (( "Cannot restore transaction state\n"
	      "       of %s\n"
	      "       because the Stable Heap was garbage-collected.",
	      fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ) ));
    }
    RETURN ( (LPHASHTABLE) NULL );
  }

  if ( fnTractOpen ( lpHeap, indexFile, szStreamReadWrite ) == NULL ) {
    int		nErrNo;
    nErrNo	= errno;
    fnFlush ( lpHeap );
    if ( ! bIgnoreError ) {
      WARN (( szOpenIndexFailed,
	      fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
    }
    RETURN ( (LPHASHTABLE) NULL );
  }

  fnSeek ( &lpHeap->Idx, 0, SEEK_END );
  /* Resize the hash table to number of elements in the transaction index
     file: */
  fnHashResize ( &lpHeap->TractIdxTable,
		 FGETPOS ( &lpHeap->Idx ) / sizeof ( TractIdx ) );
  fnSeek ( &lpHeap->Idx, 0, SEEK_SET );

  while ( fnRead ( &TractIdx, sizeof ( TractIdx ), 1, &lpHeap->Idx ) == 1 &&
	  TractIdx.nTractId == nTractId ) {
    fnHashInsert ( &lpHeap->TractIdxTable, TractIdx.oFix, &TractIdx );
  }
  lpHeap->oIdxWritten	=
    Fixnum2ObjId ( fnHashOccupied ( &lpHeap->TractIdxTable ) );

  /* Open the transaction heap and position to end-of-file: */
  if ( fnTractOpen ( lpHeap, heapFile, szStreamReadWrite ) != NULL ) {
    fnSeek ( &lpHeap->Log, 0, SEEK_END );
  }

  RETURN ( &lpHeap->TractIdxTable );
} /* fnRestoreIdxTable */

/* -------------------------------------------------------------------------
| Accessor(s)
 ------------------------------------------------------------------------- */
static LPPLOBHEAP	fnGetPlobHeap		( OBJID oSelf,
						  BOOL bIgnoreError )
{
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( fnGetPlobHeap );

  ASSERT ( boundp ( oSelf ) );
  lpHeap	= (LPPLOBHEAP) SH_key_to_address ( oSelf );
  ASSERT ( lpHeap != NULL );
  if ( ! ASSERT_TYPE ( oSelf, lpHeap, eshHeapTag ) ) {
    RETURN ( (LPPLOBHEAP) NULL );
  }

  /* Check if transient pointers are still valid: */
  if ( lpHeap->timeTract != timeGlobalStart ) {
    /* The transient pointers are not valid: */
    lpHeap->timeTract		= timeGlobalStart;
    memset ( &lpHeap->Idx, 0, sizeof ( lpHeap->Idx ) );
    memset ( &lpHeap->Log, 0, sizeof ( lpHeap->Log ) );
    HashCreate ( &lpHeap->TractIdxTable, 128, sizeof ( TRACTIDX ) );
    if ( fixnump ( lpHeap->oTractId ) ) {
      /* Restore TractIdxTable from the transaction index stream: */
      if ( ! fnRestoreIdxTable ( lpHeap,
				 OBJID2FIXNUM ( lpHeap->oTractId ),
				 bIgnoreError ) ) {
	fnTractClose ( lpHeap, (BOOL) TRUE );
	fnFlush ( lpHeap );
	fnTractUnlink ( lpHeap );
      }
    } else {
      /* There was no active transaction; remove the index- and heap-
         file: */
      fnTractUnlink ( lpHeap );
    }
  }
  RETURN ( lpHeap );
} /* fnGetPlobHeap */

/* -------------------------------------------------------------------------
| FilePos I/O
 ------------------------------------------------------------------------- */
static void		fnAllocateBuffer	( LPFILEPOS	pFilePos )
{
  PROCEDURE	( fnAllocateBuffer );

  if ( pFilePos->pBuffer == NULL ) {
    /* Allocate & set the stream buffer: */
    pFilePos->pBuffer	= Malloc ( nTransactionLogBlockSize );
    ASSERT ( pFilePos->pBuffer != NULL );
    setvbuf ( pFilePos->lpFile, pFilePos->pBuffer, _IOFBF,
	      nTransactionLogBlockSize );
  }

  RETURN ( VOID );
} /* fnAllocateBuffer */

/* ----------------------------------------------------------------------- */
static int		fnRead			( LPVOID lpItem,
						  size_t nSizeOfItem,
						  size_t nNumberOfItems,
						  LPFILEPOS lpFilePos )
{
  int		nRead;

  PROCEDURE	( fnRead );

  ASSERT ( lpItem != NULL );
  ASSERT ( lpFilePos != NULL );
  ASSERT ( lpFilePos->lpFile != NULL );

  if ( lpFilePos->pBuffer == NULL ) {
    fnAllocateBuffer ( lpFilePos );
  }
  nRead		= fread ( lpItem, nSizeOfItem, nNumberOfItems,
			  lpFilePos->lpFile );
  if ( nRead == nNumberOfItems ) {
    FINCPOS ( lpFilePos, nSizeOfItem * nNumberOfItems );
    if ( lpFilePos->nMaxPos < FGETPOS ( lpFilePos ) ) {
      lpFilePos->nMaxPos	= FGETPOS ( lpFilePos );
    }
  }
  RETURN ( nRead );
} /* fnRead */

/* ----------------------------------------------------------------------- */
static size_t		fnWrite			( LPVOID lpItem,
						  size_t nSizeOfItem,
						  size_t nNumberOfItems,
						  LPFILEPOS lpFilePos )
{
  static const char	szFseekFailed []	=
    "Could not fseek transaction file to position %lu,"
    " errno %d.";

  size_t	nWrote		= 0;
  unsigned long	nSizeInBytes	=
    (unsigned long) nSizeOfItem * (unsigned long) nNumberOfItems;

  PROCEDURE	( fnWrite );

  ASSERT ( lpItem != NULL );
  ASSERT ( lpFilePos != NULL );
  ASSERT ( lpFilePos->lpFile != NULL );

  if ( lpFilePos->pBuffer == NULL ) {
    fnAllocateBuffer ( lpFilePos );
  }

  if ( FGETPOS ( lpFilePos ) + nSizeInBytes >= lpFilePos->nMaxPos ) {
    /* Extend the file by a multiple of nTransactionLogBlockSize: */
    unsigned long	nGrowOffsetInBytes	=
      lpFilePos->nMaxPos - FGETPOS ( lpFilePos );
    unsigned long	nAlignedMaxPos		=
      ( ( (unsigned long) lpFilePos->nMaxPos +
	  (unsigned long) nTransactionLogBlockSize - 1 ) /
	(unsigned long) nTransactionLogBlockSize ) *
      (unsigned long) nTransactionLogBlockSize;
    unsigned long	nGrowInBytes		=
      ( ( nSizeInBytes - nGrowOffsetInBytes + nTransactionLogBlockSize - 1 ) /
	(unsigned long) nTransactionLogBlockSize ) *
      (unsigned long) nTransactionLogBlockSize +
      nAlignedMaxPos - lpFilePos->nMaxPos;
    LPVOID		pBuffer			= NULL;

    if ( nGrowInBytes <= 0 ) {
      nGrowInBytes	= nTransactionLogBlockSize;
    }
    pBuffer	= Malloc ( nGrowInBytes );
    if ( pBuffer != NULL ) {
      if ( nGrowOffsetInBytes > 0 ) {
	/* Position to max. pos.: */
	if ( fseek ( lpFilePos->lpFile, lpFilePos->nMaxPos, SEEK_SET ) != 0 ) {
	  int	nErrNo	= errno;
	  pBuffer	= Free ( pBuffer );
	  ERROR (( szFseekFailed, lpFilePos->nMaxPos, nErrNo ));
	  RETURN ( nWrote );
	}
      }
      if ( pBuffer != NULL ) {
	memset ( pBuffer, 0, nGrowInBytes );
	/* Grow the file by nGrowInBytes bytes: */
	if ( fwrite ( pBuffer, nGrowInBytes, 1, lpFilePos->lpFile ) != 1 ) {
	  int	nErrNo	= errno;
	  pBuffer	= Free ( pBuffer );
	  if ( nGrowOffsetInBytes > 0 ) {
	    fseek ( lpFilePos->lpFile, FGETPOS ( lpFilePos ), SEEK_SET );
	  }
	  ERROR (( "Could not grow transaction file by %lu bytes, errno %d.",
		   nGrowInBytes, nErrNo ));
	  RETURN ( nWrote );
	}
      }
      if ( pBuffer != NULL ) {
	pBuffer	= Free ( pBuffer );
      }
      if ( nGrowOffsetInBytes > 0 ) {
	/* Position back to previous position: */
	if ( fseek ( lpFilePos->lpFile, FGETPOS ( lpFilePos ),
		     SEEK_SET ) != 0 ) {
	  int	nErrNo	= errno;
	  ERROR (( szFseekFailed, FGETPOS ( lpFilePos ), nErrNo ));
	  RETURN ( nWrote );
	}
      }
      lpFilePos->nMaxPos	+= nGrowInBytes;
      ASSERT ( lpFilePos->nMaxPos % nTransactionLogBlockSize == 0 );
    }
  }

  nWrote	= fwrite ( lpItem, nSizeOfItem, nNumberOfItems,
			   lpFilePos->lpFile );

  if ( nWrote == nNumberOfItems ) {
    FINCPOS ( lpFilePos, nSizeInBytes );
    if ( lpFilePos->nMaxPos < FGETPOS ( lpFilePos ) ) {
      lpFilePos->nMaxPos	= FGETPOS ( lpFilePos );
    }
  }

  RETURN ( nWrote );
} /* fnWrite */

/* ----------------------------------------------------------------------- */
static long		fnSeek			( LPFILEPOS lpFilePos,
						  long nPos,
						  int nMode )
{
  long		nSeeked;

  PROCEDURE	( fnSeek );

  ASSERT ( lpFilePos != NULL );
  ASSERT ( lpFilePos->lpFile != NULL );

  if ( nMode == SEEK_SET && FGETPOS ( lpFilePos ) == nPos )
    RETURN ( 0 );

  nSeeked	= fseek ( lpFilePos->lpFile, nPos, nMode );
  if ( nSeeked == 0 ) {
    if ( nMode == SEEK_SET ) {
      FSETPOS ( lpFilePos, nPos );
    } else {
      FSETPOS ( lpFilePos, ftell ( lpFilePos->lpFile ) );
    }
    if ( lpFilePos->nMaxPos < FGETPOS ( lpFilePos ) ) {
      lpFilePos->nMaxPos	= FGETPOS ( lpFilePos );
    }
  }
  RETURN ( nSeeked );
} /* fnSeek */

/* ----------------------------------------------------------------------- */
static void		fnFlush			( LPPLOBHEAP lpHeap )
{
  PROCEDURE	( fnFlush );

  if ( lpHeap && boundp ( lpHeap->oTractId ) ) {
    if ( lpHeap->Idx.lpFile != NULL ) {
      fflush ( lpHeap->Idx.lpFile );
    }
    if ( lpHeap->Log.lpFile != NULL ) {
      fflush ( lpHeap->Log.lpFile );
    }
  }
  RETURN ( VOID );
} /* fnFlush */

/* -------------------------------------------------------------------------
| Transaction-Support
 ------------------------------------------------------------------------- */
static LPSTR		fnTractFilename		( LPPLOBHEAP lpHeap,
						  FILEKIND nKind,
						  LPSTR lpszBuf,
						  size_t nBuf )
{
  static char	szBuf [ MAX_FNAME ];
  LPCSTR	lpszKind;

  PROCEDURE	( fnTractFilename );

  ASSERT ( lpHeap );
  switch ( nKind ) {
  case indexFile:
    lpszKind	= "idx";
    break;
  case heapFile:
    lpszKind	= "log";
    break;
  default:
    fnFlush ( lpHeap );
    ERROR (( szUnexpectedKind, nKind ));
    RETURN ( (LPSTR) NULL );
  }
  if ( lpszBuf == NULL ) {
    lpszBuf	= szBuf;
    nBuf	= sizeof ( szBuf );
  }
  sprintf ( lpszBuf, szTractFileFormat, szGlobalDirectory,
	    lpszKind, OBJID2FIXNUM ( lpHeap->oTractLogId ) );
  RETURN ( lpszBuf );
} /* fnTractFilename */

/* ----------------------------------------------------------------------- */
static LPFILE		fnTractOpen		( LPPLOBHEAP lpHeap,
						  FILEKIND nKind,
						  LPCSTR lpszMode )
{
  LPFILEPOS	lpFilePos = (LPFILEPOS) NULL;
  char		szFilename [ MAX_FNAME ];

  PROCEDURE	( fnTractOpen );

  ASSERT ( lpHeap != NULL );
  if ( lpHeap == NULL ) {
    RETURN ( (LPFILE) NULL );
  }

  switch ( nKind ) {
  case indexFile:
    lpFilePos	= & lpHeap->Idx;
    break;
  case heapFile:
    lpFilePos	= & lpHeap->Log;
    break;
  default:
    fnFlush ( lpHeap );
    ERROR (( szUnexpectedKind, nKind ));
    RETURN ( (LPFILE) NULL );
  }

  if ( lpFilePos->lpFile != NULL ) {
    /* The file is already open; position to begin of file: */
    if ( fnSeek ( lpFilePos, 0, SEEK_SET ) != 0 ) {
      fnClose ( lpFilePos );
    }
  }
  if ( lpFilePos->lpFile == NULL ) {
    /* Open the transaction file: */
    fnTractFilename ( lpHeap, nKind, szFilename, sizeof ( szFilename ) );
    lpFilePos->nPos		= 0;
    lpFilePos->nMaxPos		= 0;
    lpFilePos->lpFile		= (LPFILE) fopen ( szFilename, lpszMode );
    if ( lpFilePos->lpFile != NULL ) {
      /* Open was successfull; call fnSeek to store the actual file
	 position into the FILEPOS: */
      fnSeek ( lpFilePos, 0, ( *lpszMode == 'a' ) ? SEEK_END : SEEK_SET );
      if ( nKind == indexFile ) {
	lpHeap->oIdxWritten	= o0;
      }
    }
  }
  RETURN ( lpFilePos->lpFile );
} /* fnTractOpen */

/* ----------------------------------------------------------------------- */
static void		fnTractClose		( LPPLOBHEAP lpHeap,
						  BOOL bFlush )
{
  PROCEDURE	( fnTractClose );

  ASSERT ( lpHeap );
  if ( lpHeap == NULL )
    RETURN ( VOID );

  fnHashClear ( &lpHeap->TractIdxTable );
  makunbound ( lpHeap->oTractId );
  makunbound ( lpHeap->oLockStore );
  makunbound ( lpHeap->oGCcounter );
  lpHeap->oIdxWritten	= o0;
  if ( bFlush && GetFlushMode ( lpHeap->oSelf ) >= flushSometimes ) {
    SH_stabilise ();
  }
  RETURN ( VOID );
} /* fnTractClose */

/* ----------------------------------------------------------------------- */
static void		fnClose			( LPFILEPOS	pFilePos )
{
  PROCEDURE	( fnClose );

  if ( pFilePos->lpFile != NULL ) {
    fclose ( pFilePos->lpFile );
    pFilePos->lpFile	= (LPFILE) NULL;
  }

  pFilePos->nPos	= 0;
  pFilePos->nMaxPos	= 0;

  if ( pFilePos->pBuffer != NULL ) {
    pFilePos->pBuffer	= Free ( pFilePos->pBuffer );
  }

  RETURN ( VOID );
} /* fnClose */

/* ----------------------------------------------------------------------- */
static void		fnTractUnlink		( LPPLOBHEAP lpHeap )
{
  char		szFilename [ MAX_FNAME ];

  PROCEDURE	( fnTractUnlink );

  fnClose ( &lpHeap->Idx );
  unlink ( fnTractFilename ( lpHeap, indexFile, szFilename,
			     sizeof ( szFilename ) ) );

  fnClose ( &lpHeap->Log );
  unlink ( fnTractFilename ( lpHeap, heapFile, szFilename,
			     sizeof ( szFilename ) ) );

  RETURN ( VOID );
} /* fnTractUnlink */

/* ----------------------------------------------------------------------- */
static LPTRACTIDX	fnSaveTractIdx		( LPPLOBHEAP lpHeap,
						  LPTRACTIDX lpTractIdx )
{
  long		nPosEnd;

  PROCEDURE	( fnSaveTractIdx );

  ASSERT ( lpHeap );
  ASSERT ( lpTractIdx );

  if ( lpHeap->Idx.lpFile == NULL ) {
    if ( fnTractOpen ( lpHeap, indexFile, szStreamWriteTruncate ) ==
	 NULL ) {
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Creating transaction index file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	       fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
      RETURN ( (LPTRACTIDX) NULL );
    }
    lpHeap->oIdxWritten	= o0;
  }

  if ( lpTractIdx->nPosIndex == nPosNotSavedTag ) {
    /* The index entry was not yet saved; append it to transaction index
       file: */
    nPosEnd	= ObjId2Fixnum ( lpHeap->oIdxWritten ) * sizeof ( TRACTIDX );
    if ( fnSeek ( &lpHeap->Idx, nPosEnd, SEEK_SET ) != 0 ) {
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Positioning to end at position %d of transaction index file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	       nPosEnd,
	       fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
      RETURN ( (LPTRACTIDX) NULL );
    }
    lpTractIdx->nPosIndex	= nPosEnd;
    if ( fnWrite ( lpTractIdx, sizeof ( *lpTractIdx ), 1,
		   &lpHeap->Idx ) != 1 ) {
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Appending to transaction index file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	      fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
      RETURN ( (LPTRACTIDX) NULL );
    }
    INCOBJID ( lpHeap->oIdxWritten, 1 );
  } else {
    /* The index entry was already saved; re-position and write it: */
    if ( fnSeek ( &lpHeap->Idx, lpTractIdx->nPosIndex, SEEK_SET ) != 0 ) {
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Positioning to %d of transaction index file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	       lpTractIdx->nPosIndex,
	       fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
      RETURN ( (LPTRACTIDX) NULL );
    }
    if ( fnWrite ( lpTractIdx, sizeof ( *lpTractIdx ), 1,
		   &lpHeap->Idx ) != 1 ) {
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Updating of transaction index file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	       fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
      RETURN ( (LPTRACTIDX) NULL );
    }
  }

  if ( GetFlushMode ( lpHeap->oSelf ) >= flushAlways ) {
    fflush ( lpHeap->Idx.lpFile );
  }

  RETURN ( lpTractIdx );
} /* fnSaveTractIdx */

/* ----------------------------------------------------------------------- */
static LPTRACTIDX	fnSaveVectorLock	( LPPLOBHEAP lpHeap,
						  OBJID oLocked,
						  SHLOCK nLockNew )
{
  SHLOCK	nLock;
  LPTRACTIDX	lpTractIdx;
  TRACTIDX	TractIdx;
  OBJID		oFix;

  PROCEDURE	( fnSaveVectorLock );

  nLock	= ( ( nLockNew & eshLockLevelMask ) == eshLockLevelElement ) ?
    /* Store the vector lock mode instead of the element lock mode: */
    fnLockGet ( lpHeap->oSelf, eshLockLevelVector, oLocked, -1 ) :
    nLockNew;

  oFix		= Var2FixObjId ( oLocked );
  lpTractIdx	= (LPTRACTIDX) HashGet ( &lpHeap->TractIdxTable, oFix );
  if ( lpTractIdx ) {
    if ( lpTractIdx->nLock != nLock ) {
      /* There is already an entry; update it in the hash table and
	 in the transaction index: */
      lpTractIdx->nLock	= nLock;
      fnSaveTractIdx ( lpHeap, lpTractIdx );
      if ( ( nLockNew & eshLockModeMask ) == 0 ) {
	/* The object was unlocked; remove it from the hash table: */
	fnHashDelete ( &lpHeap->TractIdxTable, oFix );
	lpTractIdx	= (LPTRACTIDX) NULL;
      }
    }
  } else if ( ( nLockNew & eshLockModeMask ) != 0 ) {
    /* Not found in hash table; insert entry into hash table: */
    TractIdx.oFix	= oFix;
    TractIdx.nTractId	= ObjId2Fixnum ( lpHeap->oTractId );
    TractIdx.nLock	= nLock;
    TractIdx.nPosIndex	= nPosNotSavedTag;
    TractIdx.nPosHeap	= nPosNotSavedTag;
    HashInsert ( &lpHeap->TractIdxTable, TractIdx.oFix, &TractIdx );
    lpTractIdx		= (LPTRACTIDX)
      HashGet ( &lpHeap->TractIdxTable, oFix );
    if ( GetFlushMode ( lpHeap->oSelf ) >= flushAlways ) {
      lpTractIdx	= fnSaveTractIdx ( lpHeap, lpTractIdx );
    } else if ( ! ( TractIdx.nLock &
		    ( eshLockModeWrite | eshLockModeWriteIntent ) ) ) {
      /* Only write-locked objects are also saved to the transaction
	 heap; read locks can be saved now into the transaction index
	 without the transaction index stream being fseek'ed because
	 the object state will never change for read-locked objects: */
      lpTractIdx	= fnSaveTractIdx ( lpHeap, lpTractIdx );
    }
  }
  RETURN ( lpTractIdx );
} /* fnSaveVectorLock */

/* ----------------------------------------------------------------------- */
static BOOL		fnSaveLock		( LPPLOBHEAP lpHeap,
						  OBJID oLocked,
						  SHLOCK nLock )
{
  BOOL		bSaved;

  PROCEDURE	( fnSaveLock );

  bSaved	= (BOOL) TRUE;

  AtomicLock ( lpHeap->oSelf, lpHeap->oSelf );
  switch ( nLock & eshLockLevelMask ) {
  case eshLockLevelElement:
  case eshLockLevelVector:
    bSaved		= (BOOL)
      ( fnSaveVectorLock ( lpHeap, oLocked, nLock ) != NULL );
    break;
  case eshLockLevelStore:
    lpHeap->oLockStore	= Fixnum2ObjId ( nLock );
    break;
  default:
    AtomicUnlock ( lpHeap->oSelf, lpHeap->oSelf );
    fnFlush ( lpHeap );
    ERROR (( "Unknown lock level 0x%X\n"
	     "      for %s encountered.",
	     nLock & eshLockLevelMask,
	     fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ) ));
    RETURN ( (BOOL) FALSE );
  }
  AtomicUnlock ( lpHeap->oSelf, lpHeap->oSelf );

  RETURN ( bSaved );
} /* fnSaveLock */

/* ----------------------------------------------------------------------- */
static BOOL		fnUnsaveLock		( LPPLOBHEAP lpHeap,
						  OBJID oLocked,
						  SHLOCK nLock )
{
  BOOL		bUnsaved;

  PROCEDURE	( fnUnsaveLock );

  bUnsaved	= (BOOL) TRUE;

  AtomicLock ( lpHeap->oSelf, lpHeap->oSelf );
  switch ( nLock & eshLockLevelMask ) {
  case eshLockLevelElement:
  case eshLockLevelVector:
    bUnsaved			= (BOOL)
      ( fnSaveVectorLock ( lpHeap, oLocked, nLock ) != NULL );
    break;
  case eshLockLevelStore:
    makunbound ( lpHeap->oLockStore );
    break;
  default:
    AtomicUnlock ( lpHeap->oSelf, lpHeap->oSelf );
    fnFlush ( lpHeap );
    ERROR (( "Unknown unlock level 0x%X\n"
	     "       for %s encountered.",
	     nLock & eshLockLevelMask,
	     fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ) ));
    RETURN ( (BOOL) FALSE );
  }
  AtomicUnlock ( lpHeap->oSelf, lpHeap->oSelf );

  RETURN ( bUnsaved );
} /* fnUnsaveLock */

/* ----------------------------------------------------------------------- */
enum {
  nHeaderSize	= eshSHvectorIdxFirstObjId
};

/* ----------------------------------------------------------------------- */
static BOOL		fnSaveObject		( LPPLOBHEAP lpHeap,
						  OBJID oToSave,
						  LPOBJID lpSHvector )
{
  BOOL		bSaved;
  LPTRACTIDX	lpTractIdx;
  SHLOCK	nLock;
  int		i, n;
  long		nPosHeap;
  OBJID		oFix;

  PROCEDURE	( fnSaveObject );

  bSaved	= (BOOL) TRUE;

  lpTractIdx	= (LPTRACTIDX)
    HashGet ( &lpHeap->TractIdxTable, Var2FixObjId ( oToSave ) );
  if ( lpTractIdx != NULL ) {
    /* Found in hash table; now look if to append the vector to the
       transaction heap: */
    if ( lpTractIdx->nPosHeap != nPosNotSavedTag ) {
      /* Object was already saved, so do nothing more: */
      RETURN ( (BOOL) TRUE );
    }
    /* Not found in hash table; look if there is a store lock and save
       vector to transaction-index and -heap if there's one: */
  } else if ( fixnump ( lpHeap->oLockStore ) ) {
#if 1
    lpTractIdx	= fnSaveVectorLock ( lpHeap, oToSave, (SHLOCK)
				     OBJID2FIXNUM ( lpHeap->oLockStore ) );
    if ( lpTractIdx == NULL ) {
      RETURN ( (BOOL) FALSE );
    }
#else
    /* 1996/12/03 HK: Experiment: Don't save the object state for a
       store lock. For the server operating on localhost, this gives a
       speedup of around 50% */
    RETURN ( (BOOL) TRUE );
#endif
  } else {
    /* Reaching here means that there should be no lock set onto the object
       in the current transaction; although, there can be still locks
       set by previous transactions which were not deleted because of
       error crashes. Now look if such a lock can be located on vector
       level: */
    nLock	= fnLockGet ( lpHeap->oSelf, eshLockLevelVector,
			      oToSave, -1 );
    /* If the lock found is a write- or write-intent-lock, re-use it: */
    if ( nLock & ( eshLockModeWrite | eshLockModeWriteIntent ) ) {
      lpTractIdx	= fnSaveVectorLock ( lpHeap, oToSave, nLock );
      if ( lpTractIdx == NULL ) {
	RETURN ( (BOOL) FALSE );
      }
    } else {
      char	szHeap [ 128 ];
      fnFlush ( lpHeap );
      CERROR (( "Don't save object to transaction log.",
	        "Call to %s without a write lock set\n"
		"       on %s\n"
		"       by %s.",
	        __szProc__, fnPrintObject ( oToSave, (LPSTR) NULL, 0 ),
	        PrintObject ( lpHeap->oSelf, szHeap ) ));
      RETURN ( (BOOL) FALSE );
    }
  }

  /* For objects created within the current transaction, no storing of
     the object's state is necessary; see the comment at the
     definition of flagCreated in plobintern.h: */
  if ( GETFLAGBIT ( lpSHvector [ eshSHvectorIdxTypeTag ], flagCreated ) ) {
    RETURN ( (BOOL) TRUE );
  }

  /* Object was not yet saved; append it to transaction heap file: */
  if ( lpHeap->Log.lpFile == NULL ) {
    if ( fnTractOpen ( lpHeap, heapFile, szStreamWriteTruncate ) ==
	 NULL ) {
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Creating transaction heap file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	       fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
      RETURN ( (BOOL) FALSE );
    }
  }

  nPosHeap	= FGETPOS ( &lpHeap->Log );

  /* Write the object 'header': */
  if ( fnWrite ( lpSHvector, sizeof ( psint ), nHeaderSize,
		 &lpHeap->Log ) != nHeaderSize ) {
    char	szHeap [ 128 ];
    int		nErrNo;
    nErrNo	= errno;
    fnFlush ( lpHeap );
    ERROR (( "Writing object header of %s to transaction heap file\n"
	     "       of %s\n"
	     "       failed. errno is %d.",
	     fnPrintObject ( oToSave, (LPSTR) NULL, 0 ),
	     PrintObject ( lpHeap->oSelf, szHeap ),
	     nErrNo ));
    RETURN ( (BOOL) FALSE );
  }

  /* Write the object's objids: */
  n	= lpSHvector [ eshSHvectorIdxObjIds ];
  for ( i = 0; i < n; i++ ) {
    oFix	= Var2FixObjId ( lpSHvector [ eshSHvectorIdxFirstObjId + i ] );
    if ( fnWrite ( &oFix, sizeof ( oFix ), 1, &lpHeap->Log ) != 1 ) {
      char	szHeap [ 128 ];
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Writing object objid\n"
	       "       of %s\n"
	       "       at index %d to transaction heap file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	       fnPrintObject ( oToSave, (LPSTR) NULL, 0 ),
	       eshSHvectorIdxFirstObjId + i,
	       PrintObject ( lpHeap->oSelf, szHeap ),
	       nErrNo ));
      RETURN ( (BOOL) FALSE );
    }
  }

  /* Write the object's values: */
  i	= lpSHvector [ eshSHvectorIdxSize ] - n - eshSHvectorIdxFirstObjId;
  if ( i > 0 ) {
    if ( fnWrite ( & lpSHvector [ n + eshSHvectorIdxFirstObjId ],
		   sizeof ( psint ), i, &lpHeap->Log ) != i ) {
      char	szHeap [ 128 ];
      int	nErrNo;
      nErrNo	= errno;
      fnFlush ( lpHeap );
      ERROR (( "Writing object values\n"
	       "       of %s\n"
	       "       to transaction heap file\n"
	       "       of %s\n"
	       "       failed. errno is %d.",
	       fnPrintObject ( oToSave, (LPSTR) NULL, 0 ),
	       PrintObject ( lpHeap->oSelf, szHeap ),
	       nErrNo ));
      RETURN ( (BOOL) FALSE );
    }
  }

  if ( GetFlushMode ( lpHeap->oSelf ) >= flushAlways ) {
    fflush ( lpHeap->Log.lpFile );
  }

  /* Update the transaction index: */
  lpTractIdx->nPosHeap	= nPosHeap;
  bSaved		= (BOOL)
    ( fnSaveTractIdx ( lpHeap, lpTractIdx ) != NULL );

  RETURN ( bSaved );
} /* fnSaveObject */

/* ----------------------------------------------------------------------- */
static BOOL		fnTractRollback		( LPPLOBHEAP lpHeap,
						  int nTractId )
{
  TRACTIDX		TractIdx;
  LPTRACTIDX		lpTractIdx;
  OBJID			oRead, oLockedBy;
  psint			Header [ nHeaderSize ];
  LPOBJID		lpSHvector;
  int			i, n, nRead, nLocked, nWritten;

  PROCEDURE		( fnTractRollback );

  ASSERT ( lpHeap != NULL );

  if ( boundp ( lpHeap->oGCcounter ) &&
       ObjId2Fixnum ( lpHeap->oGCcounter ) != GetGCcounter () ) {
    fnTractClose ( lpHeap, (BOOL) TRUE );
    fnFlush ( lpHeap );
    fnTractUnlink ( lpHeap );
    WARN (( "Can't rollback\n"
	    "       %s because\n"
	    "       the stable heap was garbage-collected"
	    " between start of transaction and now.",
	    fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ) ));
    RETURN ( (BOOL) FALSE );
  }

  nWritten	= ObjId2Fixnum ( lpHeap->oIdxWritten );

  if ( fnTractOpen ( lpHeap, indexFile, szStreamReadWrite ) == NULL ) {
    int	nErrNo = errno;
    fnTractClose ( lpHeap, (BOOL) TRUE );
    fnFlush ( lpHeap );
    fnTractUnlink ( lpHeap );
    if ( nWritten > 0 ) {
      WARN (( szOpenIndexFailed,
	      fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ), nErrNo ));
      RETURN ( (BOOL) FALSE );
    }
    RETURN ( (BOOL) TRUE );
  }

  ASSERT ( FGETPOS ( &lpHeap->Idx ) == 0 );
  nRead		= 0;

  while ( TRUE ) {

    int	nReadTractId	=
      fnRead ( &TractIdx, sizeof ( TractIdx ), 1, &lpHeap->Idx );

    if ( nReadTractId != 1 ) {
      break;
    }
    if ( TractIdx.nTractId != nTractId ) {
      break;
    }

    lpTractIdx	= (LPTRACTIDX)
      HashGet ( &lpHeap->TractIdxTable, TractIdx.oFix );
    oRead	= Fix2VarObjId ( TractIdx.oFix );
    lpSHvector	= AtomicLock ( oRead, lpHeap->oSelf );

    if ( ( TractIdx.nLock & ( eshLockModeWrite | eshLockModeWriteIntent ) ) &&
	 TractIdx.nPosHeap != nPosNotSavedTag ) {
      /* Restore the sh-vector: */
      if ( lpHeap->Log.lpFile == NULL ) {
	if ( fnTractOpen ( lpHeap, heapFile, szStreamReadWrite ) == NULL ) {
	  int		nErrNo;
	  nErrNo	= errno;
	  fnTractClose ( lpHeap, (BOOL) TRUE );
	  fnFlush ( lpHeap );
	  fnTractUnlink ( lpHeap );
	  WARN (( "Opening transaction heap file\n"
		  "       of %s\n"
		  "       failed. errno is %d.",
		  fnPrintObject ( lpHeap->oSelf, (LPSTR) NULL, 0 ),
		  nErrNo ));
	  RETURN ( (BOOL) FALSE );
	}
      }

      if ( fnSeek ( &lpHeap->Log, TractIdx.nPosHeap, SEEK_SET ) != 0 ) {
	char	szHeap [ 128 ];
	int	nErrNo;
	nErrNo	= errno;
	fnTractClose ( lpHeap, (BOOL) TRUE );
	fnFlush ( lpHeap );
	fnTractUnlink ( lpHeap );
	ERROR (( "Positioning to %d of transaction heap file\n"
		 "       of %s\n"
		 "       for object %s\n"
		 "       failed. errno is %d.",
		 TractIdx.nPosHeap,
		 PrintObject ( lpHeap->oSelf, szHeap ),
		 fnPrintObject ( oRead, (LPSTR) NULL, 0 ), nErrNo ));
	RETURN ( (BOOL) FALSE );
      }
      if ( fnRead ( Header, sizeof ( psint ), nHeaderSize,
		    &lpHeap->Log ) != nHeaderSize ) {
	char	szHeap [ 128 ];
	int	nErrNo;
	nErrNo	= errno;
	fnTractClose ( lpHeap, (BOOL) TRUE );
	fnFlush ( lpHeap );
	fnTractUnlink ( lpHeap );
	ERROR (( "Reading object header for\n"
		 "       object %s\n"
		 "       from transaction heap file\n"
		 "       of %s\n"
		 "       failed. errno is %d.",
		 fnPrintObject ( oRead, (LPSTR) NULL, 0 ),
		 PrintObject ( lpHeap->oSelf, szHeap ),
		 nErrNo ));
	RETURN ( (BOOL) FALSE );
      }

      if ( lpSHvector [ eshSHvectorIdxObjIds ] !=
	   Header [ eshSHvectorIdxObjIds ] ) {
	char	szHeap [ 128 ];
	AtomicUnlock ( oRead, lpHeap->oSelf );
	fnTractClose ( lpHeap, (BOOL) TRUE );
	fnFlush ( lpHeap );
	fnTractUnlink ( lpHeap );
	WARN (( "Unexpected change in size of objid field of\n"
		"       object %s\n"
		"       from %d to %d at scanning transaction heap file\n"
		"       of %s.",
		fnPrintObject ( oRead, (LPSTR) NULL, 0 ),
		Header [ eshSHvectorIdxObjIds ],
		lpSHvector [ eshSHvectorIdxObjIds ],
		PrintObject ( lpHeap->oSelf, szHeap ) ));
	RETURN ( (BOOL) FALSE );
      }

      if ( lpSHvector [ eshSHvectorIdxSize ] !=
	   Header [ eshSHvectorIdxSize ] ) {
	char	szHeap [ 128 ];
	AtomicUnlock ( oRead, lpHeap->oSelf );
	fnTractClose ( lpHeap, (BOOL) TRUE );
	fnFlush ( lpHeap );
	fnTractUnlink ( lpHeap );
	WARN (( "Unexpected change in total size of\n"
		"       object %s\n"
		"       from %d to %d at scanning transaction heap file\n"
		"       of %s.",
		fnPrintObject ( oRead, (LPSTR) NULL, 0 ),
		Header [ eshSHvectorIdxSize ],
		lpSHvector [ eshSHvectorIdxSize ],
		PrintObject ( lpHeap->oSelf, szHeap ) ));
	RETURN ( (BOOL) FALSE );
      }

      oLockedBy	= lpSHvector [ eshSHvectorIdxLockedBy ];
      n		= Header [ eshSHvectorIdxObjIds ];
      if ( fnRead ( & lpSHvector [ eshSHvectorIdxFirstObjId ],
		    sizeof ( psint ), n, &lpHeap->Log ) != n ) {
	char	szHeap [ 128 ];
	int	nErrNo;
	nErrNo	= errno;
	lpSHvector [ eshSHvectorIdxLockedBy ]	= oLockedBy;
	AtomicUnlock ( oRead, lpHeap->oSelf );
	fnTractClose ( lpHeap, (BOOL) TRUE );
	fnFlush ( lpHeap );
	fnTractUnlink ( lpHeap );
	ERROR (( "Reading object objids for\n"
		 "       object %s\n"
		 "       from transaction heap file\n"
		 "       of %s\n"
		 "       failed. errno is %d.",
		 fnPrintObject ( oRead, (LPSTR) NULL, 0 ),
		 PrintObject ( lpHeap->oSelf, szHeap ),
		 nErrNo ));
	RETURN ( (BOOL) FALSE );
      }

      /* Change the objids from fix to 'var' format: */
      for ( i = 0; i < n; i++ ) {
	lpSHvector [ eshSHvectorIdxFirstObjId + i ]	=
	  Fix2VarObjId ( lpSHvector [ eshSHvectorIdxFirstObjId + i ] );
      }

      /* Restore the locked-by component: */
      lpSHvector [ eshSHvectorIdxLockedBy ]	= oLockedBy;

      /* Read in the values: */
      i	=
	lpSHvector [ eshSHvectorIdxSize ] - n - eshSHvectorIdxFirstObjId;
      if ( i > 0 ) {
	if ( fnRead ( & lpSHvector [ n + eshSHvectorIdxFirstObjId ],
		      sizeof ( psint ), i, &lpHeap->Log ) != i ) {
	  char		szHeap [ 128 ];
	  int		nErrNo;
	  nErrNo	= errno;
	  AtomicUnlock ( oRead, lpHeap->oSelf );
	  fnTractClose ( lpHeap, (BOOL) TRUE );
	  fnFlush ( lpHeap );
	  fnTractUnlink ( lpHeap );
	  ERROR (( "Reading object values for\n"
		   "       object %s\n"
		   "       from transaction heap file\n"
		   "       of %s\n"
		   "       failed. errno is %d.",
		   fnPrintObject ( oRead, (LPSTR) NULL, 0 ),
		   PrintObject ( lpHeap->oSelf, szHeap ),
		   nErrNo ));
	  RETURN ( (BOOL) FALSE );
	}
      }
    }
    SETFLAGBIT ( lpSHvector [ eshSHvectorIdxTypeTag ], flagDirty, FALSE );
    SETFLAGBIT ( lpSHvector [ eshSHvectorIdxTypeTag ], flagCreated, FALSE );
    AtomicUnlock ( oRead, lpHeap->oSelf );
    fnUnlockAll ( lpHeap->oSelf, oRead );
    if ( lpTractIdx ) {
      makunbound ( lpTractIdx->oFix );
    }
    nRead++;
  }

  nLocked	= fnHashOccupied ( &lpHeap->TractIdxTable );
  if ( nRead != nLocked ) {
    /* Unlock the other still locked objects: */
    fnHashEnum ( &lpHeap->TractIdxTable, fnTractUnlock, &lpHeap->oSelf );
  }

  if ( fixnump ( lpHeap->oLockStore ) ) {
    /* Remove the store lock: */
    fnUnlockAll ( lpHeap->oSelf, NULLOBJID );
  }

  if ( nRead != nWritten ) {
    fnTractClose ( lpHeap, (BOOL) TRUE );
    fnFlush ( lpHeap );
    fnTractUnlink ( lpHeap );
    WARN (( "Rolled back %d objects but expected to roll back %d objects.",
	    nRead, nWritten ));
  }

  fnTractClose ( lpHeap, (BOOL) TRUE );
  fnFlush ( lpHeap );
  fnTractUnlink ( lpHeap );

  RETURN ( (BOOL) TRUE );
} /* fnTractRollback */

/* ----------------------------------------------------------------------- */
static HASHENUM		fnTractUnlock		( LPHASHTABLE lpTractIdxTable,
						  HASHKEY nObjIdFix,
						  LPVOID lpTractIdx,
						  size_t nData,
						  LPVOID lpUserData )
{
  OBJID		oToUnlock;

  PROCEDURE	( fnTractUnlock );

  ASSERT ( nData == sizeof ( TRACTIDX ) );
  ASSERT ( lpUserData != NULL );

  oToUnlock	= ((LPTRACTIDX)lpTractIdx)->oFix;
  if ( boundp ( oToUnlock ) ) {
    OBJID	oHeap	= * (LPOBJID) lpUserData;
    SHLOCK	nLock	= ((LPTRACTIDX)lpTractIdx)->nLock;
    LPOBJID	pObject;
    oToUnlock		= Fix2VarObjId ( oToUnlock );
    pObject		= AtomicLock ( oToUnlock, oHeap );
    SETFLAGBIT ( pObject [ eshSHvectorIdxTypeTag ], flagDirty, FALSE );
    SETFLAGBIT ( pObject [ eshSHvectorIdxTypeTag ], flagCreated, FALSE );
    AtomicUnlock ( oToUnlock, oHeap );
    if ( ( (unsigned int) nLock & (unsigned int) eshLockLevelMask ) <
	 (unsigned int) eshLockLevelStore ) {
      fnUnlockAll ( oHeap, oToUnlock );
    }
  }
  RETURN ( hashContinue );
} /* fnTractUnlock */

/* ----------------------------------------------------------------------- */
OBJID		fnMakeHeap	( OBJID	oUser,
				  OBJID	oMachine,
				  OBJID	oDescription )
{
  OBJID		oHeap;
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( fnMakeHeap );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oHeap			=
    fnCreateObject ( (SHTYPETAG) eshHeapTag, 0, NULLTYPETAG, 0 );
  lpHeap		= fnGetPlobHeap ( oHeap, (BOOL) TRUE );
  ASSERT ( lpHeap != NULL );
  lpHeap->oUser		= oUser;
  lpHeap->oMachine	= oMachine;
  lpHeap->oDescription	= oDescription;

  RETURN ( oHeap );
} /* fnMakeHeap */

/* ----------------------------------------------------------------------- */
OBJID		fnHeapUser		( OBJID oHeap )
{
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( fnHeapUser );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpHeap	= fnGetPlobHeap ( oHeap, (BOOL) TRUE );
  ASSERT ( lpHeap != NULL );
  RETURN ( lpHeap->oUser );
} /* fnHeapUser */

/* ----------------------------------------------------------------------- */
OBJID		fnHeapMachine		( OBJID oHeap )
{
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( fnHeapMachine );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpHeap	= fnGetPlobHeap ( oHeap, (BOOL) TRUE );
  ASSERT ( lpHeap != NULL );
  RETURN ( lpHeap->oMachine );
} /* fnHeapMachine */

/* ----------------------------------------------------------------------- */
OBJID		fnHeapDescription	( OBJID oHeap )
{
  LPPLOBHEAP	lpHeap;

  PROCEDURE	( fnHeapDescription );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpHeap	= fnGetPlobHeap ( oHeap, (BOOL) TRUE );
  ASSERT ( lpHeap != NULL );
  RETURN ( lpHeap->oDescription );
} /* fnHeapDescription */

/* ----------------------------------------------------------------------- */
TRACTID		fnTransactionBegin	( OBJID	oHeap,
					  BOOL	bIgnoreError )
{
  LPPLOBHEAP	lpHeap;
  TRACTID	nTractId;

  PROCEDURE	( fnTransactionBegin );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpHeap	= fnGetPlobHeap ( oHeap, bIgnoreError );
  nTractId	= NULLTRACTID;
  if ( lpHeap != NULL ) {
    if ( boundp ( lpHeap->oTractId ) ) {
      if ( ! bIgnoreError ) {
	char	szTractId [ 256 ], szHeap [ 256 ];
	fnFlush ( lpHeap );
	CERROR (( "Ignore request to start a transaction.",
		  "There is already an active transaction %s on\n"
		  "       heap %s.",
		  PrintObject ( lpHeap->oTractId, szTractId ),
		  PrintObject ( oHeap, szHeap ) ));
      }
    } else {
      nTractId	= fnGetNextTractId ();
      fnServerPlobdReply ( (void(*)()) fnServerTransactionBegin, &nTractId );
      AtomicLock ( oHeap, oHeap );
      fnTractClose ( lpHeap, (BOOL) FALSE );
      if ( lpHeap->Idx.lpFile != NULL ) {
	fnSeek ( &lpHeap->Idx, 0, SEEK_SET );
      }
      if ( lpHeap->Log.lpFile != NULL ) {
	fnSeek ( &lpHeap->Log, 0, SEEK_SET );
      }
      makunbound ( lpHeap->oLockStore );
      lpHeap->oTractId		= Fixnum2ObjId ( nTractId );
      lpHeap->oGCcounter	= Fixnum2ObjId ( GetGCcounter () );
      AtomicUnlock ( oHeap, oHeap );
      if ( GetFlushMode ( oHeap ) >= flushOften ) {
	SH_stabilise ();
      }
    }
  }
  RETURN ( nTractId );
} /* fnTransactionBegin */

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnServerTransactionBegin, "c-sh-begin-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  TRACTID	nTractId;
  OBJID		oHeap;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLTRACTID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  nTractId	= fnTransactionBegin ( oHeap, bIgnoreError );

  UnstoreSession ();
  RETURN ( nTractId );
} EndFunction ( fnServerTransactionBegin );

/* ----------------------------------------------------------------------- */
TRACTID		fnTransactionCancel	( OBJID	oHeap,
					  BOOL	bIgnoreError )
{
  LPPLOBHEAP	lpHeap;
  TRACTID	nTractId	= NULLTRACTID;

  PROCEDURE	( fnTransactionCancel );

  INITIALIZEPLOB;

  lpHeap	= fnGetPlobHeap ( oHeap, bIgnoreError );
  nTractId	= NULLTRACTID;
  if ( lpHeap != NULL ) {
    if ( boundp ( lpHeap->oTractId ) ) {
      nTractId		= ObjId2Fixnum ( lpHeap->oTractId );
      fnServerPlobdReply ( (void(*)()) fnServerTransactionCancel, &nTractId );
      if ( ! fnTractRollback ( lpHeap, nTractId ) ) {
	nTractId	= NULLTRACTID;
      }
    } else  if ( ! bIgnoreError ) {
      fnFlush ( lpHeap );
      CERROR (( "Ignore request to cancel a transaction.",
	        szNoTransaction,
	        fnPrintObject ( oHeap, (LPSTR) NULL, 0 ) ));
    }
  }

  RETURN ( nTractId );
} /* fnTransactionCancel */

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnServerTransactionCancel, "c-sh-cancel-transaction",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  TRACTID	nTractId;
  OBJID		oHeap;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLTRACTID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  nTractId	= fnTransactionCancel ( oHeap, bIgnoreError );

  UnstoreSession ();
  RETURN ( nTractId );
} EndFunction ( fnServerTransactionCancel );

/* ----------------------------------------------------------------------- */
TRACTID		fnTransactionEnd	( OBJID	oHeap,
					  BOOL	bIgnoreError )
{
  LPPLOBHEAP		lpHeap;
  TRACTID		nTractId	= NULLTRACTID;

  PROCEDURE	( fnTransactionEnd );

  INITIALIZEPLOB;
  lpHeap	= fnGetPlobHeap ( oHeap, bIgnoreError );
  nTractId	= NULLTRACTID;
  if ( lpHeap != NULL ) {
    if ( boundp ( lpHeap->oTractId ) ) {
      nTractId		= ObjId2Fixnum ( lpHeap->oTractId );
      fnServerPlobdReply ( (void(*)()) fnServerTransactionEnd, &nTractId );
      /* Unlock all locked objects: */
      fnHashEnum ( &lpHeap->TractIdxTable, fnTractUnlock, &lpHeap->oSelf );
      if ( fixnump ( lpHeap->oLockStore ) ) {
	/* Remove the store lock: */
	fnUnlockAll ( lpHeap->oSelf, NULLOBJID );
      }
      fnTractClose ( lpHeap, (BOOL) TRUE );
    } else  if ( ! bIgnoreError ) {
      fnFlush ( lpHeap );
      CERROR (( "Ignore request to end a transaction.",
	        szNoTransaction,
	        fnPrintObject ( oHeap, (LPSTR) NULL, 0 ) ));
    }
  }
  RETURN ( nTractId );
} /* fnTransactionEnd */

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnServerTransactionEnd, "c-sh-end-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  TRACTID	nTractId;
  OBJID		oHeap;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLTRACTID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  nTractId	= fnTransactionEnd ( oHeap, bIgnoreError );

  UnstoreSession ();
  RETURN ( nTractId );
} EndFunction ( fnServerTransactionEnd );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnServerTransactionFlush, "c-sh-flush-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  OBJID		oHeap;
  LPPLOBHEAP	lpHeap;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( VOID );
    }
  }

  oHeap	= SHORT2LONGOBJID ( oShortObjIdHeap );
  if ( heapp ( oHeap ) ) {
    fnFlush ( fnGetPlobHeap ( oHeap, (BOOL) FALSE ) );
  }

  UnstoreSession ();
  RETURN ( VOID );
} EndFunction ( fnServerTransactionFlush );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnServerDbTransactionP, "c-sh-in-transaction-p",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( TRACTID, value_in, nTractId ) ) )
{
  OBJID		oHeap;
  LPPLOBHEAP	lpHeap;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLTRACTID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  lpHeap	= fnGetPlobHeap ( oHeap, (BOOL) FALSE );
  if ( nTractId != NULLTRACTID ) {
    nTractId	= ( lpHeap && boundp ( lpHeap->oTractId ) &&
		    ObjId2Fixnum ( lpHeap->oTractId ) == nTractId ) ?
      nTractId : NULLTRACTID;
  } else {
    nTractId	= ( lpHeap && boundp ( lpHeap->oTractId ) ) ?
      ObjId2Fixnum ( lpHeap->oTractId ) : NULLTRACTID;
  }

  UnstoreSession ();
  RETURN ( nTractId );
} EndFunction ( fnServerDbTransactionP );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
