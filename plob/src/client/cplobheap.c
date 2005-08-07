/* -------------------------------------------------------------------------
| Module	cplobheap.c
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
#include	<errno.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif
#include	<rpc/rpc.h>

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobnumber.h"
#include	"cplobsequ.h"
#include	"cplobstruct.h"
#include	"cplobclos.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobregex.h"
#include	"cplobroot.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
/* 0 (no), 1 (insert lock), 2 (create object), 4 (peek messages) */
#define	LOGGING	0x00

/* ----------------------------------------------------------------------- */
/* 1997/06/25 HK: Debugging: Write some informations on the internal
   used hash tables into the log file. I'm assuming that the hash
   tables are a little bit memory-hungry: */
#if 0
static const char	szLogHashFormat []	=
"Number of active transactions: %d\n"
"       Number of mallocs(): %5d; free()s: %5d\n"
"       Client cache statistics:\n"
"       ClientCacheHeaps:   Occ = %4d, elm = %4d, dat = %4d\n"
"       ClientCacheObjects: Occ = %4d, elm = %4d, dat = %4d";
#define	LOGHASH()	LOG (( szLogHashFormat, \
			       nTransactions, nMallocs, nFrees, \
			       (int) pClientCacheHeaps->nOccupied, \
			       (int) pClientCacheHeaps->nElements, \
			       (int) pClientCacheHeaps->nSizeOfDataField, \
			       (int) pClientCacheObjects->nOccupied, \
			       (int) pClientCacheObjects->nElements, \
			       (int) pClientCacheObjects->nSizeOfDataField ))
#else
/* 1997/06/25 HK: Debugging: Hmm, at least the hash table sizes don't
   increase dramatically from one transaction to the next; so perhaps
   the memory leak is not within the hash tables. */
#define	LOGHASH()
#endif
/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */
BOOL		bGlobalDoCaching	= TRUE;
const char	szFormatNotOpened []	=
"Heap %s wasn't opened from this process.";

static const char	szRpcFlushFailed []	=
  "Flushing RPC connection failed.";

/* ----------------------------------------------------------------------- */
static LPHASHTABLE	pClientCacheHeaps	= NULL;
static LPHASHTABLE	pClientCacheObjects	= NULL;

static int		nMallocs	= 0;
static int		nFrees		= 0;
static int		nTransactions	= 0;

static const char	szLeakError []	=
"Memory leak detected: %d malloc()s, %d free()s.";
static const char	szDanglingTransaction []	=
"Dangling %s-transaction encountered; transaction counter is %d.";

static const psint	NullValues [ 1 ] = { 0 };

enum {
  eForceFlushThreshold	= /* Force an RPC flush after */ 1024 /* bytes */
};

/* ----------------------------------------------------------------------- */
void	fnHeapCloseAllExcept1	( SHORTOBJID	oNotThis )
{
  BOOL		bMapped;
  SHORTOBJID	oHeap;

  PROCEDURE	( fnHeapCloseAllExcept1 );
  INITIALIZEPLOB;

  fnInvalidateAllCaches ();
  for ( bMapped = fnHashFirst ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
				(LPVOID *) NULL, (size_t *) NULL );
	bMapped;
	bMapped = fnHashNext ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
			       (LPVOID *) NULL, (size_t *) NULL ) ) {
    ASSERT ( oHeap != NULLOBJID );
    if ( oHeap != oNotThis ) {
      fnServerDbClose ( oHeap, TRUE );
    }
  }
  fnCacheDestroy ( NULLOBJID );

  RETURN ( VOID );
} /* fnHeapCloseAllExcept1 */

/* ----------------------------------------------------------------------- */
void		fnHeapCloseAll	( void )
{
  PROCEDURE	( fnHeapCloseAll );
  INITIALIZEPLOB;

  fnHeapCloseAllExcept1 ( NULLOBJID );

  RETURN ( VOID );
} /* fnHeapCloseAll */

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeHeapModule	( void )
{
  PROCEDURE	( fnInitializeHeapModule );

  pClientCacheHeaps	= HashCreate ( NULL, 16, sizeof ( HEAPCACHE ) );
  pClientCacheObjects	= HashCreate ( NULL, 359, sizeof ( POBJECTCACHE ) );

  RETURN ( VOID );
} /* fnInitializeHeapModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeHeapModule	( void )
{
  int		nSessions	= 0;
  FILE		* pStream	= NULL;

  PROCEDURE	( fnDeinitializeHeapModule );

#if !WIN32
  pStream	 = stdout;
#endif

  nSessions	= ( pClientCacheHeaps != NULL ) ?
    fnHashOccupied ( pClientCacheHeaps ) : 0;

  if ( nSessions > 0 ) {
    PCLIENT	pClient	= (PCLIENT) NULL;
    BOOL	bClosed	= FALSE;
    if ( pStream != NULL ) {
      putc ( '\n', stdout );
    }
    pClient	= fnClientPlobd ();
    if ( pClient != NULL ) {
      if ( pStream != NULL ) {
	fprintf ( pStream, "%s(%d): %s:\n"
		 "\tPLOB! is trying to close %d pending session(s),"
		 " please wait",
		 __szFile__, __LINE__, __szProc__, nSessions );
	fflush ( pStream );
      }
      if ( fnClientPlobdFlush ( pClient ) ) {
	fnHeapCloseAll ();
	fnClientPlobdFlush ( pClient );
	fnClientDestroy ( pClient );
	if ( pStream != NULL ) {
	  fputs ( " ... done!\n\n", pStream );
	}
	bClosed	= TRUE;
      } else {
	/* 2005-04-06 hkirschk: Hmmm, when the DLL gets unloaded by
	   LispWorks, the fnClientPlobdFlush call from above fails
	   with RPC_CANTSEND. Looks as if some other ressource has
	   been freed already ... */
	if ( pStream != NULL ) {
	  fputs ( ".\n", pStream );
	}
      }
    }
    if ( ! bClosed ) {
      /* 2005-04-06 hkirschk: Debug: */
      LOG (( "Could not close %u pending sessions"
	     " since the connection to the server is lost.",
	     nSessions ));
      if ( pStream != NULL ) {
	fprintf ( pStream, "%s(%d): %s: PLOB! can't close %d\n"
		  "\tpending session(s) since the connection"
		  " to the server is lost.\n\n",
		  __szFile__, __LINE__, __szProc__, nSessions );
      }
    }
    if ( pStream != NULL ) {
      fflush ( pStream );
    }
  }

  if ( pClientCacheHeaps != NULL ) {
    pClientCacheHeaps	= fnHashDestroy ( pClientCacheHeaps );
  }
  if ( pClientCacheObjects ) {
    pClientCacheObjects	= fnHashDestroy ( pClientCacheObjects );
  }

  RETURN ( VOID );
} /* fnDeinitializeHeapModule */

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */

PHEAPCACHE	fnCacheCreate	( SHORTOBJID	oShortObjIdHeap )
{
  HASHRESULT	eResult		= hashNotFound;
  PHEAPCACHE	pHeapCache	= (PHEAPCACHE) NULL;
  HEAPCACHE	HeapCache;

  PROCEDURE	( fnCacheCreate );

  ASSERT ( oShortObjIdHeap != NULLOBJID );

  pHeapCache	= (PHEAPCACHE) HashGet ( pClientCacheHeaps, oShortObjIdHeap );

  if ( pHeapCache != NULL ) {
    /* Re-use the heap cache: */
    fnCacheClear ( oShortObjIdHeap );
  } else {
    memset ( &HeapCache, 0, sizeof ( HeapCache ) );
    eResult	=
      HashInsert ( pClientCacheHeaps, oShortObjIdHeap, &HeapCache );
    ASSERT ( eResult == hashInserted );
    pHeapCache	= (PHEAPCACHE) HashGet ( pClientCacheHeaps, oShortObjIdHeap );
    ASSERT ( pHeapCache != NULL );
    HashCreate ( &pHeapCache->Objects, 16, sizeof ( HEAPOBJECTCACHE ) );
  }

  makunbound ( pHeapCache->onTractId );

  RETURN ( pHeapCache );
} /* fnCacheCreate */

/* ----------------------------------------------------------------------- */
static BOOL	fnObjectCacheFree	( POBJECTCACHE	*ppObjectCache,
					  /* The heap referencing the object
					     cache: */
					  SHORTOBJID	oHeap,
					  /* The cached object: */
					  SHORTOBJID	oCached,
					  BOOL		bOnlyFreeObject )
{
  HASHRESULT	eDeleted;

  PROCEDURE	( fnObjectCacheFree );

  ASSERT ( ppObjectCache != NULL );

  if ( *ppObjectCache ) {
    if ( bOnlyFreeObject ) {
      Free ( *ppObjectCache );
      *ppObjectCache	= (POBJECTCACHE) NULL;
      nFrees++;
    } else {
      if ( oHeap != NULLOBJID ) {
	(*ppObjectCache)->nReferences--;
      }
      ASSERT ( (*ppObjectCache)->nReferences >= 0 );
      if ( (*ppObjectCache)->nReferences <= 0 ) {
	Free ( *ppObjectCache );
	*ppObjectCache	= (POBJECTCACHE) NULL;
	nFrees++;
	eDeleted	= fnHashDelete ( pClientCacheObjects, oCached );
	ASSERT ( eDeleted == hashDeleted );
      }
    }
  }
  RETURN ( (BOOL) ( *ppObjectCache != NULL ) );
} /* fnObjectCacheFree */

/* ----------------------------------------------------------------------- */
static PHEAPCACHE	fnHeapCacheFree	( PHEAPCACHE	pHeapCache,
					  SHORTOBJID	oHeap,
					  BOOL		bOnlyFreeObject )
{
  BOOL			bMapped;
  SHORTOBJID		oCached;
  PHEAPOBJECTCACHE	pHeapObjectCache;

  PROCEDURE	( fnHeapCacheFree );

  ASSERT ( pHeapCache != NULL );

  for ( bMapped = fnHashFirst ( &pHeapCache->Objects, (LPHASHKEY) &oCached,
				(LPVOID FAR *) &pHeapObjectCache,
				(size_t *) NULL );
	bMapped;
	bMapped = fnHashNext ( &pHeapCache->Objects, (LPHASHKEY) &oCached,
			       (LPVOID FAR *) &pHeapObjectCache,
			       (size_t *) NULL ) ) {
    ASSERT ( oCached != NULLOBJID );
    ASSERT ( pHeapObjectCache != NULL );
    fnObjectCacheFree ( &pHeapObjectCache->pObjectCache, oHeap, oCached,
			bOnlyFreeObject );
    pHeapObjectCache->pObjectCache	= (POBJECTCACHE) NULL;
    pHeapObjectCache->nVectorLockOld	= (SHLOCK)
      ( (unsigned int) eshLockLevelNothing |
	(unsigned int) eshLockModeNothing );
    pHeapObjectCache->nVectorLockNow	= (SHLOCK)
      ( (unsigned int) eshLockLevelNothing |
	(unsigned int) eshLockModeNothing );
  }
  fnHashDestroy ( &pHeapCache->Objects );

  RETURN ( pHeapCache );
} /* fnHeapCacheFree */

/* ----------------------------------------------------------------------- */
FIXNUM		fnCacheDestroy	( SHORTOBJID	oShortObjIdHeap )
{
  PHEAPCACHE	pHeapCache;
  BOOL		bMapped;
  SHORTOBJID	oHeap;
  HASHRESULT	eDeleted;

  PROCEDURE	( fnCacheDestroy );
  INITIALIZEPLOB;

  if ( oShortObjIdHeap != NULLOBJID ) {
    pHeapCache	= (PHEAPCACHE) HashGet ( pClientCacheHeaps, oShortObjIdHeap );
    if ( pHeapCache != NULL ) {
      fnHeapCacheFree ( pHeapCache, oShortObjIdHeap, FALSE );
      eDeleted	= fnHashDelete ( pClientCacheHeaps, oShortObjIdHeap );
      ASSERT ( eDeleted == hashDeleted );
    }
  } else {
    /* Free all cached heaps and objects: */
    for ( bMapped = fnHashFirst ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
				  (LPVOID *) &pHeapCache, (size_t *) NULL );
	  bMapped;
	  bMapped = fnHashNext ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
				 (LPVOID *) &pHeapCache, (size_t *) NULL ) ) {
      ASSERT ( oHeap != NULLOBJID );
      ASSERT ( pHeapCache != NULL );
      fnHeapCacheFree ( pHeapCache, oHeap, TRUE );
    }
    fnHashClear ( pClientCacheHeaps );
    fnHashClear ( pClientCacheObjects );
  }
  RETURN ( fnHashOccupied ( pClientCacheHeaps ) );
} /* fnCacheDestroy */

/* ----------------------------------------------------------------------- */
static PHEAPCACHE	fnHeapCacheClear( PHEAPCACHE	pHeapCache,
					  SHORTOBJID	oHeap,
					  BOOL		bOnlyFreeObject )
{
  BOOL			bMapped;
  SHORTOBJID		oCached;
  PHEAPOBJECTCACHE	pHeapObjectCache;

  PROCEDURE	( fnHeapCacheClear );

  ASSERT ( pHeapCache != NULL );

  for ( bMapped = fnHashFirst ( &pHeapCache->Objects, (LPHASHKEY) &oCached,
				(LPVOID FAR *) &pHeapObjectCache,
				(size_t *) NULL );
	bMapped;
	bMapped = fnHashNext ( &pHeapCache->Objects, (LPHASHKEY) &oCached,
			       (LPVOID FAR *) &pHeapObjectCache,
			       (size_t *) NULL ) ) {
    ASSERT ( oCached != NULLOBJID );
    ASSERT ( pHeapObjectCache != NULL );
    fnObjectCacheFree ( &pHeapObjectCache->pObjectCache, oHeap, oCached,
			bOnlyFreeObject );
    pHeapObjectCache->pObjectCache	= (POBJECTCACHE) NULL;
    pHeapObjectCache->nVectorLockOld	= (SHLOCK)
      ( (unsigned int) eshLockLevelNothing |
	(unsigned int) eshLockModeNothing );
    pHeapObjectCache->nVectorLockNow	= (SHLOCK)
      ( (unsigned int) eshLockLevelNothing |
	(unsigned int) eshLockModeNothing );
  }
  fnHashClear ( &pHeapCache->Objects );

  RETURN ( pHeapCache );
} /* fnHeapCacheClear */

/* ----------------------------------------------------------------------- */
FIXNUM		fnCacheClear	( SHORTOBJID	oShortObjIdHeap )
{
  PHEAPCACHE	pHeapCache;
  BOOL		bMapped;
  SHORTOBJID	oHeap;

  PROCEDURE	( fnCacheClear );
  INITIALIZEPLOB;

  if ( oShortObjIdHeap != NULLOBJID ) {
    pHeapCache	= (PHEAPCACHE) HashGet ( pClientCacheHeaps, oShortObjIdHeap );
    ASSERT ( pHeapCache != NULL );
    fnHeapCacheClear ( pHeapCache, oShortObjIdHeap, FALSE );
  } else {
    /* Clear all cached objects: */
    for ( bMapped = fnHashFirst ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
				  (LPVOID *) &pHeapCache, (size_t *) NULL );
	  bMapped;
	  bMapped = fnHashNext ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
				 (LPVOID *) &pHeapCache, (size_t *) NULL ) ) {
      ASSERT ( oHeap != NULLOBJID );
      ASSERT ( pHeapCache != NULL );
      fnHeapCacheClear ( pHeapCache, oHeap, TRUE );
    }
    fnHashClear ( pClientCacheObjects );
  }
  RETURN ( fnHashOccupied ( pClientCacheHeaps ) );
} /* fnCacheClear */

/* ----------------------------------------------------------------------- */
/* Put the object referenced by pPeekObject into the client's cache: */
static BOOL	fnCacheObject	( SHORTOBJID	oShortObjIdHeap,
				  SHLOCK	nVectorLockNow,
				  u_int		*pPeekObject,
				  u_int		*pPeekValues )
{
  BOOL			bInserted = FALSE;
  SHORTOBJID		oToCache;
  POBJECTCACHE		*ppObjectCache, pObjectCache = (POBJECTCACHE) NULL;
  PHEAPOBJECTCACHE	pHeapObjectCache = (PHEAPOBJECTCACHE) NULL;
  unsigned int		s;

  PROCEDURE	( fnCacheObject );

  ASSERT ( pPeekObject != NULL );

  oToCache	= LONG2SHORTOBJID ( PEEKSLOTSSELF ( pPeekObject ) );
  ppObjectCache	= (POBJECTCACHE *) HashGet ( pClientCacheObjects, oToCache );

  if ( ppObjectCache != NULL ) {
    pObjectCache	= *ppObjectCache;
    ASSERT ( pObjectCache != NULL );
  }

  pHeapObjectCache	=
    fnCacheCreateObject ( oShortObjIdHeap, oToCache,
			  PEEKSLOTSTYPETAG ( pPeekObject ),
			  PEEKSLOTSSLOTS ( pPeekObject ),
			  PEEKSLOTSTYPETAGVALUES ( pPeekObject ),
			  PEEKSLOTSVALUES ( pPeekObject ),
			  nVectorLockNow );
  if ( pHeapObjectCache == NULL ) {
    RETURN ( bInserted );
  }

  if ( pObjectCache == NULL ) {
    /* The object was created into the cache: */
    SHTYPETAG	nTypeTagValues = (SHTYPETAG) NULLTYPETAG;
    u_int	nValues	= 0;

    pObjectCache	= pHeapObjectCache->pObjectCache;
    ASSERT ( pObjectCache != NULL );
    for ( s = 0; s < pObjectCache->nSlots; s++ ) {
      pObjectCache->poSlots [ s ]	=
	PEEKSLOTSSLOTOBJID ( pPeekObject, s );
      pObjectCache->pnTypeTags [ s ]	=
	PEEKSLOTSSLOTTYPETAG ( pPeekObject, s );
    }

    nTypeTagValues	= PEEKSLOTSTYPETAGVALUES ( pPeekObject );
    nValues		= PEEKSLOTSVALUES ( pPeekObject );

    if ( nTypeTagValues != NULLTYPETAG && nValues > 0 ) {
      ASSERT ( pPeekValues != NULL );
      ASSERT ( pObjectCache->pValues != NULL );
      memcpy ( pObjectCache->pValues, pPeekValues,
	       fnTypeTagSizeValue ( 1, &nTypeTagValues, &nValues ) *
	       nSizeOfPostoreWord );
    }
  }
  RETURN ( bInserted );
} /* fnCacheObject */

/* ----------------------------------------------------------------------- */
PHEAPOBJECTCACHE fnCacheCreateObject( SHORTOBJID	oShortObjIdHeap,
				      SHORTOBJID	oShortObjId,
				      SHTYPETAG		nTypeTag,
				      unsigned int	nSlots,
				      SHTYPETAG		nTypeTagValues,
				      unsigned int	nValues,
				      SHLOCK		nLockNow )
{
  POBJECTCACHE		*ppObjectCache = (POBJECTCACHE *) NULL,
    pObjectCache = (POBJECTCACHE) NULL;
  unsigned int		nSize, s;
  PHEAPCACHE		pHeapCache = (PHEAPCACHE) NULL;
  HEAPOBJECTCACHE	HeapObjectCache;
  PHEAPOBJECTCACHE	pHeapObjectCache = (PHEAPOBJECTCACHE) NULL;
  HASHRESULT		eResult = hashNotFound;
  unsigned int		nValuesInWords;

  PROCEDURE	( fnCacheCreateObject );
  INITIALIZEPLOB;

  /* Step 1: Find resp. allocate the cache for the persistent object: */
  ppObjectCache	= (POBJECTCACHE *)
    HashGet ( pClientCacheObjects, oShortObjId );
  if ( ppObjectCache != NULL ) {
    pObjectCache	= *ppObjectCache;
    ASSERT ( pObjectCache != NULL );
  } else {
    /* The object wasn't found in the cache, so allocate the object
       cache entry ... */
    nValuesInWords	= ( nValues > 0 && nTypeTagValues != NULLTYPETAG ) ?
      fnTypeTagSizeValue ( 1, &nTypeTagValues, &nValues ) :
      0;

    nSize		= sizeof ( OBJECTCACHE ) -
      sizeof ( ((POBJECTCACHE)NULL)->bBuffer ) +
      nSlots * ( sizeof ( * ((POBJECTCACHE)NULL)->poSlots ) +
		 sizeof ( * ((POBJECTCACHE)NULL)->pnTypeTags ) ) +
      nValuesInWords * sizeof ( psint );

    pObjectCache	= (POBJECTCACHE) Malloc ( nSize );
    if ( pObjectCache == NULL ) {
      char	szObject [ 256 ];
      fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjId, nTypeTag,
				  szObject, sizeof ( szObject ) );
      CERROR (( "Don't cache the object.",
		"Allocating the cache of %d bytes (%d slots, %d values) for\n"
		"       object %s\n"
		"       failed.",
		nSize, nSlots, nValuesInWords, szObject ));
    } else {
      nMallocs++;
      /* ... fill it ... */
      pObjectCache->nReferences	= 0;
      pObjectCache->oHeap	= NULLOBJID;
      pObjectCache->nChanges	= 0;
      pObjectCache->nTypeTag	= nTypeTag;
      pObjectCache->nSlots	= nSlots;
      if ( nSlots > 0 ) {
	pObjectCache->poSlots		=
	  (LPOBJID) & pObjectCache->bBuffer [ 0 ];
	pObjectCache->pnTypeTags	= (LPSHTYPETAG) &
	  pObjectCache->bBuffer
	  [ nSlots * sizeof ( * ((POBJECTCACHE)NULL)->poSlots ) ];
	for ( s = 0; s < nSlots; s++ ) {
	  makunbound ( pObjectCache->poSlots [ s ] );
	  pObjectCache->pnTypeTags [ s ]	= (SHTYPETAG)
	    pObjectCache->poSlots [ s ];
	}
      } else {
	pObjectCache->poSlots		= (LPOBJID) NULL;
	pObjectCache->pnTypeTags	= (LPSHTYPETAG) NULL;
      }
      if ( nValues > 0 && nTypeTagValues != NULLTYPETAG ) {
	pObjectCache->nTypeTagValues	= nTypeTagValues;
	pObjectCache->nValues		= nValues;
	pObjectCache->pValues	= (void *) &
	  pObjectCache->bBuffer
	  [ nSlots * ( sizeof ( * ((POBJECTCACHE)NULL)->poSlots ) +
		       sizeof ( * ((POBJECTCACHE)NULL)->pnTypeTags ) ) ];
	memset ( pObjectCache->pValues, 0, nValuesInWords * sizeof ( psint ) );
      } else {
	pObjectCache->nTypeTagValues	= NULLTYPETAG;
	pObjectCache->nValues		= 0;
	pObjectCache->pValues		= NULL;
      }
      /* ... and insert it into the hash table: */
      eResult	=
	HashInsert ( pClientCacheObjects, oShortObjId, &pObjectCache );
      ASSERT ( eResult == hashInserted );
    }
  }

  if ( pObjectCache == NULL ) {
    /* Allocating the object cache failed: */
    RETURN ( (PHEAPOBJECTCACHE) NULL );
  }

  /* Step 2: Insert the cached object into the heap cache: */
  pHeapCache	= (PHEAPCACHE) HashGet ( pClientCacheHeaps, oShortObjIdHeap );
  ASSERT ( pHeapCache != NULL );

  pHeapObjectCache	= (PHEAPOBJECTCACHE)
    HashGet ( &pHeapCache->Objects, oShortObjId );
  if ( pHeapObjectCache != NULL ) {
    /* The object is already in the cache; check if the pointer to the
       object cache is the same one as computed in step 1: */
    ASSERT ( pHeapObjectCache->pObjectCache == pObjectCache );
    memcpy ( &HeapObjectCache, pHeapObjectCache, sizeof ( HeapObjectCache ) );
  } else {
    memset ( &HeapObjectCache, 0, sizeof ( HeapObjectCache ) );
    HeapObjectCache.nVectorLockOld	= (SHLOCK)
      ( (unsigned int) eshLockLevelNothing |
	(unsigned int) eshLockModeNothing );
  }

  HeapObjectCache.nVectorLockNow	= nLockNow;
  HeapObjectCache.pObjectCache		= pObjectCache;

  if ( nLockNow & eshLockModeExcl ) {
    /* An exclusive (write) vector lock was placed on the object;
       store the locking object into the object's cache: */
#if (LOGGING+0) & 0x02
    char	szObject [ 256 ], szHeap [ 256 ];
    fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjId,
				eshShortObjIdTag,
				szObject, sizeof ( szObject ) );
    fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjIdHeap,
				eshShortObjIdTag,
				szHeap, sizeof ( szHeap ) );
    INFO (( "Created %s\n"
	    "       into %s\n"
	    "       with exclusive lock mode 0x%X",
	    szObject, szHeap, nLockNow ));
#endif /* #if (LOGGING+0) & 0x01 */
    if ( boundp ( pObjectCache->oHeap ) ) {
      /* If this assert fails, an exclusive (write) lock was given to
	 at least two objects; this should never happen: */
      ASSERT ( pObjectCache->oHeap == oShortObjIdHeap );
    } else {
      pObjectCache->oHeap	= oShortObjIdHeap;
    }
  }

  if ( HashInsert ( &pHeapCache->Objects, oShortObjId, &HeapObjectCache ) ==
       hashInserted ) {
    pObjectCache->nReferences++;
  }

  /* Do a re-hash to get the pointer into the hash table: */
  pHeapObjectCache	= (PHEAPOBJECTCACHE)
    HashGet ( &pHeapCache->Objects, oShortObjId );
  ASSERT ( pHeapObjectCache != NULL );

  RETURN ( pHeapObjectCache );
} /* fnCacheCreateObject */

/* ----------------------------------------------------------------------- */
FIXNUM		fnCacheInsert	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjIdLockBy,
				  HPEEK		hPeek,
				  FIXNUM	nObjIdWords )
{
  FIXNUM	nInserted	= 0;
  u_int		*pObjIds	= (u_int *) NULL;
  u_int		*pValues	= (u_int *) NULL;
  u_int		*pnElementTypeTags	= (u_int *) NULL;
  u_int		*pnSizesInElements	= (u_int *) NULL;
  u_int		nValueObjects	= 0, nValueWords = 0;
  SHLOCK	nVectorLockNow;
  SHTYPETAG	nTypeTagValues;
  u_int		i, j, nValues;

  PROCEDURE	( fnCacheInsert );

  INITIALIZEPLOB;

  if ( oShortObjIdLockBy == NULLOBJID || hPeek == NULLHPEEK ) {
    RETURN ( nInserted );
  }

#if (LOGGING+0) & 0x04
  INFO (( "Handle %d, words %d", hPeek, nObjIdWords ));
#endif /* #if (LOGGING+0) & 0x04 */

  if ( nObjIdWords > 0 ) {
    pObjIds	= (u_int *) Malloc ( nObjIdWords * sizeof ( psint ) );
    ASSERT ( pObjIds != NULL );
  } else {
    pObjIds	= (u_int *) NULL;
  }

  nVectorLockNow	=
    fnClientObjectPeekSlots ( oShortObjIdHeap, hPeek, nObjIdWords, pObjIds );

  if ( (int) nVectorLockNow < 0 ) {
    if ( pObjIds != NULL ) {
      Free ( pObjIds );
      pObjIds	= (u_int *) NULL;
    }
    RETURN ( nInserted );
  }

  /* Sum up the number of values: */
  for ( i = 0; i < nObjIdWords; i += PEEKSLOTSSIZE ( pObjIds + i ) ) {
    nValues		= PEEKSLOTSVALUES ( pObjIds + i );
    nTypeTagValues	= PEEKSLOTSTYPETAGVALUES ( pObjIds + i );
    if ( nValues > 0 && nTypeTagValues != NULLTYPETAG ) {
      nValueObjects++;
#if (LOGGING+0) & 0x04
      INFO (( "oPeeked = %d,\n"
	      "\ttype tag values 0x%X, size in words %d\n"
	      "\tnumber of values %d",
	      LONG2SHORTOBJID ( PEEKSLOTSSELF ( pObjIds + i ) ),
	      nTypeTagValues,
	      fnTypeTagSizeValue ( 1, &nTypeTagValues, &nValues ),
	      nValues ));
#endif
    }
  }

  if ( nValueObjects > 0 ) {
    pnElementTypeTags	= (u_int *)
      Malloc ( nValueObjects * sizeof ( u_int ) );
    ASSERT ( pnElementTypeTags != NULL );
    pnSizesInElements	= (u_int *)
      Malloc ( nValueObjects * sizeof ( u_int ) );
    ASSERT ( pnSizesInElements != NULL );
    for ( i = 0, j = 0; i < nObjIdWords; i += PEEKSLOTSSIZE ( pObjIds + i ) ) {
      nValues		= PEEKSLOTSVALUES ( pObjIds + i );
      nTypeTagValues	= PEEKSLOTSTYPETAGVALUES ( pObjIds + i );
      if ( nValues > 0 && nTypeTagValues != NULLTYPETAG ) {
	pnSizesInElements [ j ]		= nValues;
	pnElementTypeTags [ j++ ]	= nTypeTagValues;
      }
    }
    nValueWords	=
      fnTypeTagSizeValue ( nValueObjects, pnElementTypeTags,
			   pnSizesInElements );
    pValues	= (u_int *) Malloc ( nValueWords * nSizeOfPostoreWord );
    ASSERT ( pValues != NULL );
    nVectorLockNow	=
      fnClientObjectPeekValues ( oShortObjIdHeap, hPeek,
				 nValueObjects, pnElementTypeTags,
				 pnSizesInElements, pValues );
    if ( pnSizesInElements != NULL ) {
      Free ( pnSizesInElements );
      pnSizesInElements	= NULL;
    }
    if ( pnElementTypeTags != NULL ) {
      Free ( pnElementTypeTags );
      pnElementTypeTags	= NULL;
    }
    if ( (int) nVectorLockNow < 0 ) {
      if ( pValues != NULL ) {
	Free ( pValues );
	pValues	= (u_int *) NULL;
      }
      if ( pObjIds != NULL ) {
	Free ( pObjIds );
	pObjIds	= (u_int *) NULL;
      }
      RETURN ( nInserted );
    }
  } else {
    pValues	= (u_int *) NULL;
  }

  for ( i = 0, j = 0; i < nObjIdWords; i += PEEKSLOTSSIZE ( pObjIds + i ) ) {
    SHTYPETAG	nTypeTagValues = (SHTYPETAG) NULLTYPETAG;
    u_int	nValues	= 0;
    nTypeTagValues	= PEEKSLOTSTYPETAGVALUES ( pObjIds + i );
    nValues		= PEEKSLOTSVALUES ( pObjIds + i );
    if ( nTypeTagValues != NULLTYPETAG && nValues > 0 ) {
      ASSERT ( pValues != NULL );
      ASSERT ( j < nValueWords );
#if (LOGGING+0) & 0x04
      INFO (( "oPeeked = %d,\n"
	      "\ttype tag values 0x%X, size in words %d\n"
	      "\tnumber of values %d",
	      LONG2SHORTOBJID ( PEEKSLOTSSELF ( pObjIds + i ) ),
	      nTypeTagValues,
	      fnTypeTagSizeValue ( 1, &nTypeTagValues, &nValues ),
	      nValues ));
#endif /* #if (LOGGING+0) & 0x04 */
      fnCacheObject ( oShortObjIdLockBy, nVectorLockNow,
		      pObjIds + i, pValues + j );
      j	+= fnTypeTagSizeValue ( 1, &nTypeTagValues, &nValues );
    } else {
      fnCacheObject ( oShortObjIdLockBy, nVectorLockNow, pObjIds + i, NULL );
    }
    nInserted++;
  }

  if ( pValues != NULL ) {
    Free ( pValues );
    pValues	= (u_int *) NULL;
  }
  if ( pObjIds != NULL ) {
    Free ( pObjIds );
    pObjIds	= (u_int *) NULL;
  }

  RETURN ( nInserted );
} /* fnCacheInsert */

/* ----------------------------------------------------------------------- */
HASHRESULT	fnCacheDelete	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjId )
{
  HASHRESULT	eResult	= hashNotFound;
  PHEAPCACHE	pHeapCache;
  POBJECTCACHE	*ppObjectCache;
  BOOL		bMapped;
  SHORTOBJID	oHeap;

  PROCEDURE	( fnCacheDelete );
  INITIALIZEPLOB;

  if ( oShortObjId != NULLOBJID ) {
    ppObjectCache = (POBJECTCACHE *)
      HashGet ( pClientCacheObjects, oShortObjId );
    if ( ppObjectCache != NULL ) {
      ASSERT ( *ppObjectCache != NULL );
      if ( oShortObjIdHeap != NULLOBJID  ) {
	/* Remove the object from one cache: */
	pHeapCache	= fnCacheGetHeap ( oShortObjIdHeap );
	if ( pHeapCache != NULL ) {
	  eResult	= fnHashDelete ( &pHeapCache->Objects, oShortObjId );
	  ASSERT ( eResult == hashDeleted );
	  fnObjectCacheFree ( ppObjectCache, oShortObjIdHeap,
			      oShortObjId, FALSE );
	}
      } else {
	/* Remove the object from all caches of all heaps: */
	for ( bMapped = fnHashFirst ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
				      (LPVOID *) &pHeapCache,
				      (size_t *) NULL );
	      bMapped;
	      bMapped = fnHashNext ( pClientCacheHeaps, (LPHASHKEY) &oHeap,
				     (LPVOID *) &pHeapCache,
				     (size_t *) NULL ) ) {
	  ASSERT ( oHeap != NULLOBJID );
	  ASSERT ( pHeapCache != NULL );
	  if ( HashGet ( &pHeapCache->Objects, oShortObjId ) ) {
	    eResult	= fnHashDelete ( &pHeapCache->Objects, oShortObjId );
	    ASSERT ( eResult == hashDeleted );
	    if ( ! fnObjectCacheFree ( ppObjectCache, oShortObjIdHeap,
				       oShortObjId, FALSE ) ) {
	      /* Freed the last reference: */
	      fnHashLast ( pClientCacheHeaps );
	      break;
	    }
	  }
	}
      }
    }
  } else {
    /* oShortObjId == NULLOBJID, i.e. clear the heap's cache: */    
    eResult	= hashDeleted;
    fnCacheClear ( oShortObjIdHeap );
  }
  RETURN ( eResult );
} /* fnCacheDelete */

/* ----------------------------------------------------------------------- */
static int	fnObjectCacheFlush( POBJECTCACHE	pObjectCache,
				    SHORTOBJID		oCached )
{
  /* 1998/11/20 HK: Multi-threading restriction: */
  static int	nGlobalBytesFlushed = 0;

  SHORTOBJID	oData;
  POBJECTCACHE	*ppObjectData;
  int		i, nBytesFlushed = 0;
  CLIENT	* pClient = (CLIENT *) NULL;

  PROCEDURE	( fnObjectCacheFlush );

  if ( pObjectCache != NULL ) {
    if ( pObjectCache->nChanges > 0 ) {
      ASSERT ( boundp ( pObjectCache->oHeap ) );
      /* 1996/11/11 HK: Debug: */
#if (LOGGING+0) & 0x04
      INFO (( "Poking short-objid %d, type tag 0x%X with %d slots,\n"
	      "type tag values 0x%X, %d values",
	      oCached,
	      pObjectCache->nTypeTag,
	      pObjectCache->nSlots,
	      pObjectCache->nTypeTagValues,
	      pObjectCache->nValues ));
#endif /* #if (LOGGING+0) & 0x04 */
      pObjectCache->nChanges	= 0;
      for ( i = 0; i < pObjectCache->nSlots; i++ ) {
	if ( boundp ( pObjectCache->poSlots [ i ] ) &&
	     ! immediatep ( pObjectCache->poSlots [ i ] ) &&
	     ! ObjId_is_valid ( pObjectCache->poSlots [ i ] ) ) {
	  char	szElement [ 256 ], szCached [ 256 ], szTypeTag [ 256 ];
	  fnClientObjectPrettyPrint
	    ( LONG2SHORTOBJID ( pObjectCache->oHeap ),
	      LONG2SHORTOBJID ( pObjectCache->poSlots [ i ] ),
	      eshShortObjIdTag,
	      szElement, sizeof ( szElement ) );
	  fnClientObjectPrettyPrint
	    ( LONG2SHORTOBJID ( pObjectCache->oHeap ),
	      oCached, pObjectCache->nTypeTag,
	      szCached, sizeof ( szCached ) );
	  fnPrintImmediateObject ( pObjectCache->nTypeTag, eshBuiltInTag,
				   szTypeTag, sizeof ( szTypeTag ) );
	  CERROR (( "Mark the slot as unbound.",
		    "Encountered invalid long\n"
		    "       objid %d (%s),\n"
		    "       type %s,\n"
		    "       in cached object %s,\n"
		    "       at slot location %d",
		    pObjectCache->poSlots [ i ], szElement,
		    szTypeTag, szCached, i ));
	  makunbound ( pObjectCache->poSlots [ i ] );
	}
      }
      fnClientObjectPoke ( pObjectCache->oHeap, oCached,
			   pObjectCache->nSlots,
			   (u_int *) pObjectCache->poSlots,
			   pObjectCache->nTypeTagValues,
			   pObjectCache->nValues,
			   ( ( pObjectCache->nTypeTagValues != NULLTYPETAG &&
			       pObjectCache->nValues > 0 ) ?
			     (LPVOID) pObjectCache->pValues :
			     (LPVOID) NullValues ) );
      nBytesFlushed		+=
	( 3 + pObjectCache->nSlots + 2 +
	  fnTypeTagSizeValue ( 1,
			       & pObjectCache->nTypeTagValues,
			       & pObjectCache->nValues ) ) *
	nSizeOfPostoreWord;
      /* 1998/11/20 HK: Force an RPC flush after eForceFlushThreshold
         bytes have been written: */
      nGlobalBytesFlushed	+= nBytesFlushed;
      if ( nGlobalBytesFlushed >= (int) eForceFlushThreshold ) {
	if ( pClient == NULL ) {
	  pClient	= fnClientPlobd ();
	}
	if ( pClient != NULL ) {
	  if ( ! fnClientPlobdFlush ( pClient ) ) {
	    WARN (( szRpcFlushFailed ));
	  }
 	}
	nGlobalBytesFlushed	= 0;
      }
    }
    /* Check if the flushed object was a CLOS instance; if it was,
       flush the instance data vector too: */
    if ( pObjectCache->nTypeTag == eshInstanceTag ) {
      oData	= LONG2SHORTOBJID ( pObjectCache->poSlots
				    [ eshInstIdxDataVector ] );
      ppObjectData	= (POBJECTCACHE	*)
	HashGet ( pClientCacheObjects, oData );
      if ( ppObjectData != NULL ) {
	ASSERT ( *ppObjectData != NULL );
	nBytesFlushed	+= fnObjectCacheFlush ( *ppObjectData, oData );
      }
    }
  }
  RETURN ( nBytesFlushed );
} /* fnObjectCacheFlush */

/* ----------------------------------------------------------------------- */
FIXNUM		fnCacheFlush	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjId )
{
  FIXNUM		nBytesFlushed = 0;
  FIXNUM		nFlushed = 0;
  POBJECTCACHE		*ppObjectCache, pObjectCache;
  PHEAPCACHE		pHeapCache	= NULL;
  PHEAPOBJECTCACHE	pHeapObjectCache;
  BOOL			bMapped;
  SHORTOBJID		oCached;

  PROCEDURE	( fnCacheFlush );

  INITIALIZEPLOB;

  if ( oShortObjId != NULLOBJID ) {
    /* Flush one object: */
    ppObjectCache	= (POBJECTCACHE *)
      HashGet ( pClientCacheObjects, oShortObjId );
    if ( ppObjectCache != NULL ) {
      ASSERT ( *ppObjectCache != NULL );
      nFlushed++;
      nBytesFlushed	+= fnObjectCacheFlush ( *ppObjectCache, oShortObjId );
    }
  } else if ( oShortObjIdHeap != NULLOBJID ) {
    /* Flush all objects referenced by the heap: */
    pHeapCache	= (PHEAPCACHE) HashGet ( pClientCacheHeaps, oShortObjIdHeap );
    ASSERT ( pHeapCache != NULL );
    for ( bMapped = fnHashFirst ( &pHeapCache->Objects,
				  (LPHASHKEY) &oCached,
				  (LPVOID FAR *) &pHeapObjectCache,
				  (size_t *) NULL );
	  bMapped;
	  bMapped = fnHashNext ( &pHeapCache->Objects,
				 (LPHASHKEY) &oCached,
				 (LPVOID FAR *) &pHeapObjectCache,
				 (size_t *) NULL ) ) {
      ASSERT ( oCached != NULLOBJID );
      ASSERT ( pHeapObjectCache != NULL );
      ASSERT ( pHeapObjectCache->pObjectCache != NULL );
      nFlushed++;
      nBytesFlushed	+=
	fnObjectCacheFlush ( pHeapObjectCache->pObjectCache, oCached );
    }
  } else {
    /* Flush all objects: */
    for ( bMapped = fnHashFirst ( pClientCacheObjects, (LPHASHKEY) &oCached,
				  (LPVOID *) &pObjectCache,
				  (size_t *) NULL );
	  bMapped;
	  bMapped = fnHashNext ( pClientCacheObjects, (LPHASHKEY) &oCached,
				 (LPVOID *) &pObjectCache,
				 (size_t *) NULL ) ) {
      /* If the following assert fails, the object wasn't found in
         the object cache: */
      ASSERT ( oCached != NULLOBJID );
      ASSERT ( pObjectCache != NULL );
      nFlushed++;
      nBytesFlushed	+= fnObjectCacheFlush ( pObjectCache, oCached );
    }
  }
  RETURN ( nFlushed );
} /* fnCacheFlush */

/* ----------------------------------------------------------------------- */
PHEAPCACHE	fnCacheGetHeap	( SHORTOBJID	oShortObjIdHeap )
{
  PHEAPCACHE	pHeapCache	= (PHEAPCACHE) NULL;

  PROCEDURE	( fnCacheGetHeap );

  INITIALIZEPLOB;

  pHeapCache	= (PHEAPCACHE) HashGet ( pClientCacheHeaps, oShortObjIdHeap );
  if ( pHeapCache == NULL ) {
    char	szHeap [ 256 ];
    fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjIdHeap,
		      (SHTYPETAG) eshHeapTag, szHeap, sizeof ( szHeap ) );
    ERROR (( szFormatNotOpened, szHeap ));
    RETURN ( (PHEAPCACHE) NULL );
  }

  RETURN ( pHeapCache );
} /* fnCacheHetHeap */

/* ----------------------------------------------------------------------- */
PHEAPOBJECTCACHE fnCacheGetObject( SHORTOBJID	oShortObjIdHeap,
				   SHORTOBJID	oShortObjId )
{
  PHEAPCACHE		pHeapCache		=
    fnCacheGetHeap ( oShortObjIdHeap );
  PHEAPOBJECTCACHE	pHeapObjectCache	= (PHEAPOBJECTCACHE) NULL;

  PROCEDURE	( fnCacheGetObject );

  INITIALIZEPLOB;

  if ( pHeapCache != NULL ) {
    pHeapObjectCache	= (PHEAPOBJECTCACHE)
      HashGet ( &pHeapCache->Objects, oShortObjId );
  }

  RETURN ( pHeapObjectCache );
} /* fnCacheGetObject */

/* ----------------------------------------------------------------------- */
BOOL		fnCacheCompletedP( SHORTOBJID	oShortObjIdHeap,
				   SHORTOBJID	oShortObjId )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;
  BOOL			bCompleted = FALSE;

  PROCEDURE	( fnCacheCompletedP );

  INITIALIZEPLOB;

  pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
  if ( pHeapObjectCache != NULL ) {
    bCompleted	= (BOOL)
      ( ( ! ( pHeapObjectCache->nVectorLockNow & eshLockModeRead ) ||
	  ( pHeapObjectCache->nSlotsRead >=
	    pHeapObjectCache->pObjectCache->nSlots &&
	    pHeapObjectCache->nValuesRead >=
	    pHeapObjectCache->pObjectCache->nValues ) ) &&
	( ! ( pHeapObjectCache->nVectorLockNow & eshLockModeWrite ) ||
	  ( pHeapObjectCache->nSlotsWritten >=
	    pHeapObjectCache->pObjectCache->nSlots &&
	    pHeapObjectCache->nValuesWritten >=
	    pHeapObjectCache->pObjectCache->nValues ) ) );
    if ( bCompleted ) {
      fnCacheFlush  ( oShortObjIdHeap, oShortObjId );
      fnCacheDelete ( oShortObjIdHeap, oShortObjId );
    }
  }

  RETURN ( bCompleted );
} /* fnCacheCompletedP */

/* ----------------------------------------------------------------------- */
SHLOCK		fnCacheLockP	( SHORTOBJID	oShortObjIdLockBy,
				  SHORTOBJID	oShortObjIdToLock,
				  SHLOCK	nLock )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;
  PHEAPCACHE		pHeapCache;
  SHLOCK		nLockOld = eshLockFailed;

  PROCEDURE	( fnCacheLockP );

  INITIALIZEPLOB;

  switch ( nLock & eshLockLevelMask ) {

  case eshLockLevelVector:
    pHeapObjectCache	=
      fnCacheGetObject ( oShortObjIdLockBy, oShortObjIdToLock );
    if ( pHeapObjectCache != NULL ) {
      nLockOld		= pHeapObjectCache->nVectorLockOld;
      if ( ! ( nLockOld & nLock & eshLockModeMask ) &&
	   /* The lock wasn't acquired yet; try if a vector lock was set
	      in a previous object peek: */
	   ! ( pHeapObjectCache->nVectorLockNow & nLock & eshLockModeMask ) ) {
	/* The requested lock mode could not be found by using only
	   the cache: */
	nLockOld	= eshLockFailed;
      }
    }
    break;

  case eshLockLevelStore:
    pHeapCache	= fnCacheGetHeap ( oShortObjIdLockBy );
    if  ( pHeapCache != NULL ) {
      nLockOld	= (SHLOCK)
	( ( boundp ( pHeapCache->onLockStore ) ) ?
	  ObjId2Fixnum ( pHeapCache->onLockStore ) :
	  eshLockLevelNothing | eshLockModeNothing );
      if ( ! ( nLockOld & nLock & eshLockModeMask ) ) {
	/* The requested lock mode could not be found by using only
	   the cache: */
	nLockOld	= eshLockFailed;
      }
    }
    break;

  default:
    /* Other levels are not supported by client caching: */
    break;
  }
  RETURN ( nLockOld );
} /* fnCacheLockP */

/* ----------------------------------------------------------------------- */
SHLOCK		fnCacheInsertLock( SHORTOBJID	oShortObjIdLockBy,
				   SHORTOBJID	oShortObjIdToLock,
				   SHLOCK	nLock )
{
  static const char	szFormatExpectingHeap []	=
    "Expected object\n"
    "       %s being\n"
    "       marked as locked by %s.";

  PHEAPOBJECTCACHE	pHeapObjectCache;
  PHEAPCACHE		pHeapCache;
  SHLOCK		nLockOld = eshLockFailed;

  PROCEDURE	( fnCacheInsertLock );

  INITIALIZEPLOB;

  switch ( nLock & eshLockLevelMask ) {

  case eshLockLevelVector:
    pHeapObjectCache	=
      fnCacheGetObject ( oShortObjIdLockBy, oShortObjIdToLock );
    if ( pHeapObjectCache != NULL ) {
      ASSERT ( pHeapObjectCache->pObjectCache != NULL );
      nLockOld	= pHeapObjectCache->nVectorLockOld;
      if ( nLock & eshLockModeExcl ) {
	if ( ! boundp ( pHeapObjectCache->pObjectCache->oHeap ) ) {
	  char	szObject [ 256 ], szHeap [ 256 ];
	  fnClientObjectPrettyPrint ( oShortObjIdLockBy, oShortObjIdToLock,
				      eshShortObjIdTag,
				      szObject, sizeof ( szObject ) );
	  fnClientObjectPrettyPrint ( oShortObjIdLockBy, oShortObjIdLockBy,
				      eshShortObjIdTag,
				      szHeap, sizeof ( szHeap ) );
	  ERROR (( szFormatExpectingHeap, szObject, szHeap ));
	  RETURN ( nLockOld );
	}
	if ( pHeapObjectCache->pObjectCache->oHeap != oShortObjIdLockBy ) {
	  char	szObject [ 256 ], szHeap [ 256 ];
	  fnClientObjectPrettyPrint ( oShortObjIdLockBy, oShortObjIdToLock,
				      eshShortObjIdTag,
				      szObject, sizeof ( szObject ) );
	  fnClientObjectPrettyPrint ( oShortObjIdLockBy, oShortObjIdLockBy,
				      eshShortObjIdTag,
				      szHeap, sizeof ( szHeap ) );
	  ERROR (( szFormatExpectingHeap, szObject, szHeap ));
	  RETURN ( nLockOld );
	}
      }
      pHeapObjectCache->nVectorLockNow = (SHLOCK)
	( (unsigned int) pHeapObjectCache->nVectorLockNow |
	  ( (unsigned int) nLock & (unsigned int) eshLockModeMask ) );
      pHeapObjectCache->nVectorLockOld = pHeapObjectCache->nVectorLockNow;
    }
    break;

  case eshLockLevelStore:
    pHeapCache		= fnCacheGetHeap ( oShortObjIdLockBy );
    if  ( pHeapCache != NULL ) {
      nLockOld	= (SHLOCK)
	( ( boundp ( pHeapCache->onLockStore ) ) ?
	  ObjId2Fixnum ( pHeapCache->onLockStore ) :
	  eshLockLevelNothing | eshLockModeNothing );
      pHeapCache->onLockStore	=
	Fixnum2ObjId ( nLockOld | ( nLock & eshLockModeMask ) );
    }
#if (LOGGING+0) & 0x01
    {
      char	szHeap [ 256 ];
      fnClientObjectPrettyPrint ( oShortObjIdLockBy, oShortObjIdLockBy,
				  eshShortObjIdTag,
				  szHeap, sizeof ( szHeap ) );
      INFO (( "Received store lock 0x%x\n"
	      "       for %s\n"
	      "       pHeapCache 0x%X, fnCacheLockP returns %d",
	      nLock, szHeap, pHeapCache,
	      fnCacheLockP ( oShortObjIdLockBy, NULLOBJID,
			     eshLockStoreWrite ) ));
    }
#endif /* #if (LOGGING+0) & 0x01 */
    break;

  default:
    /* Other levels are not supported by client caching: */
    break;
  }

  RETURN ( nLockOld );
} /* fnCacheInsertLock */

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientTransactionBegin, "c-sh-begin-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  PHEAPCACHE	pHeapCache	= fnCacheGetHeap ( oShortObjIdHeap );
  TRACTID	nTractId;

  INITIALIZEPLOB;

  if ( pHeapCache != NULL && bGlobalDoCaching ) {
    nTractId	= NULLTRACTID;
    if ( ! boundp ( pHeapCache->onTractId ) || ! bIgnoreError ) {
      nTractId	= fnServerTransactionBegin ( oShortObjIdHeap, bIgnoreError );
    }
    if ( nTractId != NULLTRACTID ) {
      pHeapCache->onTractId	= Fixnum2ObjId ( nTractId );
    }
  } else {
    nTractId	= fnServerTransactionBegin ( oShortObjIdHeap, bIgnoreError );
  }
  if ( nTractId != NULLTRACTID ) {
    if ( nTransactions == 0 ) {
      /* 1997/06/27 HK: Debug: */
      /* MallocLog (); */
      /* The first transaction has been started; check if all
	 memory is freed as it ought to be: */
      if ( nMallocs != nFrees ) {
	WARN (( szLeakError, nMallocs, nFrees ));
	nMallocs	= 0;
	nFrees		= 0;
      }
    }
    nTransactions++;
  }
  LOGHASH ();
  RETURN ( nTractId );
} EndFunction ( fnClientTransactionBegin );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientTransactionCancel, "c-sh-cancel-transaction",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  PHEAPCACHE	pHeapCache	= fnCacheGetHeap ( oShortObjIdHeap );
  TRACTID	nTractId;

  INITIALIZEPLOB;

  if ( pHeapCache != NULL && bGlobalDoCaching ) {
    makunbound ( pHeapCache->onTractId );
    makunbound ( pHeapCache->onLockStore );
    fnCacheClear ( oShortObjIdHeap );
  }
  nTractId	= fnServerTransactionCancel ( oShortObjIdHeap, bIgnoreError );
  if ( nTractId != NULLTRACTID ) {
    nTransactions--;
    if ( nTransactions < 0 ) {
      WARN (( szDanglingTransaction, "cancel", nTransactions ));
      nTransactions	= 0;
    } else if ( nTransactions == 0 ) {
      /* 1997/06/27 HK: Debug: */
      /* MallocLog (); */
      /* The last transaction has been finished; check if all
	 memory is freed as it ought to be: */
      if ( nMallocs !=  nFrees ) {
	WARN (( szLeakError, nMallocs, nFrees ));
	nMallocs	= 0;
	nFrees		= 0;
      }
    }
  }
  LOGHASH ();
  RETURN ( nTractId );
} EndFunction ( fnClientTransactionCancel );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientTransactionEnd, "c-sh-end-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  PHEAPCACHE	pHeapCache	= fnCacheGetHeap ( oShortObjIdHeap );
  TRACTID	nTractId;

  INITIALIZEPLOB;

  if ( pHeapCache != NULL && bGlobalDoCaching ) {
    makunbound ( pHeapCache->onTractId );
    makunbound ( pHeapCache->onLockStore );
    fnCacheFlush ( oShortObjIdHeap, NULLOBJID );
    fnCacheClear ( oShortObjIdHeap );
  }
  nTractId	= fnServerTransactionEnd ( oShortObjIdHeap, bIgnoreError );
  if ( nTractId != NULLTRACTID ) {
    nTransactions--;
    if ( nTransactions < 0 ) {
      WARN (( szDanglingTransaction, "end", nTransactions ));
      nTransactions	= 0;
    } else if ( nTransactions == 0 ) {
      /* 1997/06/27 HK: Debug: */
      /* MallocLog (); */
      /* The last transaction has been finished; check if all
	 memory is freed as it ought to be: */
      if ( nMallocs != nFrees ) {
	WARN (( szLeakError, nMallocs, nFrees ));
	nMallocs	= 0;
	nFrees		= 0;
      }
    }
  }
  LOGHASH ();
  RETURN ( nTractId );
} EndFunction ( fnClientTransactionEnd );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnClientTransactionFlush, "c-sh-flush-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  INITIALIZEPLOB;

  if ( fnCacheGetHeap ( oShortObjIdHeap ) != NULL && bGlobalDoCaching ) {
    fnCacheFlush ( oShortObjIdHeap, NULLOBJID );
  }
  fnServerTransactionFlush ( oShortObjIdHeap );
} EndFunction ( fnClientTransactionFlush );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientDbTransactionP, "c-sh-in-transaction-p",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( TRACTID, value_in, nTractId ) ) )
{
  PHEAPCACHE	pHeapCache	= fnCacheGetHeap ( oShortObjIdHeap );

  INITIALIZEPLOB;

  if ( pHeapCache != NULL && bGlobalDoCaching ) {
    if ( nTractId != NULLTRACTID ) {
      RETURN ( ( pHeapCache && boundp ( pHeapCache->onTractId ) &&
		 ObjId2Fixnum ( pHeapCache->onTractId ) == nTractId ) ?
	       nTractId : NULLTRACTID );
    }
    RETURN ( ( pHeapCache && boundp ( pHeapCache->onTractId ) ) ?
	     ObjId2Fixnum ( pHeapCache->onTractId ) : NULLTRACTID );
  }
  RETURN ( fnServerDbTransactionP ( oShortObjIdHeap, nTractId ) );
} EndFunction ( fnClientDbTransactionP );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
