/* -------------------------------------------------------------------------
| Module	plob.c
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
#include	<rpc/rpc.h>

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobsequ.h"
#include	"cplobstruct.h"
#include	"cploblock.h"
#include	"cplobclos.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobroot.h"
#include	"cplobnumber.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
enum {
  nPreallocatedObjects	= 64
};
/* Persistent CLOS instance pre-allocation. The idea is not to
   allocate CLOS instances one-by-one on client requests done by the
   LISP layer to the server, but to allocate a whole bunch of CLOS
   instances in this layer from the server and to give it one-by-one
   to the LISP layer: */
typedef struct {
  /* Number of slots in the object: */
  u_int		nSlots;
  /* Number of preallocated objects: */
  u_int		nShortObjIds;
  /* The short objids of the preallocated objects: */
  SHORTOBJID	ShortObjIds [ nPreallocatedObjects ];
  /* The short objids of the preallocated instance data vectors: */
  SHORTOBJID	ShortObjIdDatas [ nPreallocatedObjects ];
}	OBJBUFFER, * POBJBUFFER;

/* key is a structure description object, data is an OBJBUFFER: */
static HASHTABLE	PreAllocated;

static OBJBUFFER	ZeroObjBuffer;

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeCLOSModule	( void )
{
  PROCEDURE	( fnInitializeCLOSModule );

  HashCreate ( &PreAllocated, 32, sizeof ( OBJBUFFER ) );
  memset ( &ZeroObjBuffer, 0, sizeof ( ZeroObjBuffer ) );

  RETURN ( VOID );
} /* fnInitializeCLOSModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeCLOSModule	( void )
{
  PROCEDURE	( fnDeinitializeCLOSModule );

  fnInvalidateCLOSCache ();
  fnHashDestroy ( &PreAllocated );

  RETURN ( VOID );
} /* fnDeinitializeCLOSModule */

/* ----------------------------------------------------------------------- */
void			fnInvalidateCLOSCache		( void )
{
  CLIENT	*pClient	= (CLIENT *) NULL;
  BOOL		bMapped;
  POBJBUFFER	pObjBuffer;
  u_int		j, n;

  PROCEDURE	( fnInvalidateCLOSCache );

  pClient	= fnClientPlobd ();
  for ( bMapped = fnHashFirst ( &PreAllocated, (LPHASHKEY) NULL,
				(LPVOID *) &pObjBuffer, (size_t *) NULL );
	bMapped;
	bMapped = fnHashNext ( &PreAllocated, (LPHASHKEY) NULL,
			       (LPVOID *) &pObjBuffer, (size_t *) NULL ) ) {
    ASSERT ( pObjBuffer->nShortObjIds <=
	     length ( pObjBuffer->ShortObjIds ) );
    if ( pClient != NULL ) {
      for ( j = 0, n = pObjBuffer->nShortObjIds; j < n; j++ ) {
	ASSERT ( pObjBuffer->ShortObjIds [ j ] != NULLOBJID );
	ASSERT ( pObjBuffer->ShortObjIdDatas [ j ] != NULLOBJID );
	fnServerObjectDestroy ( NULLOBJID, pObjBuffer->ShortObjIds [ j ] );
	fnServerObjectDestroy ( NULLOBJID, pObjBuffer->ShortObjIdDatas [ j ] );
	pObjBuffer->nShortObjIds--;
	pObjBuffer->ShortObjIds [ j ]	= NULLOBJID;
	pObjBuffer->ShortObjIdDatas [ j ]	= NULLOBJID;
      }
      ASSERT ( pObjBuffer->nShortObjIds == 0 );
    } else {
      /* No connection to server; simply forget all cached objids: */
      pObjBuffer->nShortObjIds	= 0;
      memset ( pObjBuffer->ShortObjIds, 0,
	       sizeof ( pObjBuffer->ShortObjIds ) );
      memset ( pObjBuffer->ShortObjIdDatas, 0,
	       sizeof ( pObjBuffer->ShortObjIdDatas ) );
    }
  }
  RETURN ( VOID );
} /* fnInvalidateCLOSCache */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbCreateInstance, "c-sh-create-instance",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in,
			     oShortObjIdClassDescr ) ) )
{
  SHORTOBJID		oShortObjId = NULLOBJID, oShortObjIdData;
  POBJBUFFER		pObjBuffer = (POBJBUFFER) NULL;
  u_int			nSlots;
  SHLOCK		nLockStore;
  PHEAPOBJECTCACHE	pHeapObjectCache = (PHEAPOBJECTCACHE) NULL;

  INITIALIZEPLOB;
  if ( bGlobalDoCaching && oShortObjIdClassDescr != NULLOBJID ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdClassDescr );
    pObjBuffer	= (POBJBUFFER)
      HashGet ( &PreAllocated, oShortObjIdClassDescr );
    if ( pObjBuffer == NULL ) {
      HashInsert ( &PreAllocated, oShortObjIdClassDescr, &ZeroObjBuffer );
      pObjBuffer	= (POBJBUFFER)
	HashGet ( &PreAllocated, oShortObjIdClassDescr );
      ASSERT ( pObjBuffer != NULL );
    }
    /* Check if there are still objects left in the pre-allocate buffer;
       if not, allocate a new bunch of objects: */
    if ( pObjBuffer->nShortObjIds <= 0 ) {
      pObjBuffer->nShortObjIds	=
	fnServerDbCreateInstances ( oShortObjIdHeap, oShortObjIdClassDescr,
				    nPreallocatedObjects,
				    (u_int *) pObjBuffer->ShortObjIds,
				    &pObjBuffer->nSlots,
				    (u_int *) pObjBuffer->ShortObjIdDatas );
    }
    if ( pObjBuffer->nShortObjIds > 0 ) {
      /* Take one object from the pre-allocate buffer: */
      oShortObjId	=
	pObjBuffer->ShortObjIds [ --(pObjBuffer->nShortObjIds) ];
      pObjBuffer->ShortObjIds [ pObjBuffer->nShortObjIds ]	= NULLOBJID;
      ASSERT ( oShortObjId != NULLOBJID );
      oShortObjIdData	=
	pObjBuffer->ShortObjIdDatas [ pObjBuffer->nShortObjIds ];
      pObjBuffer->ShortObjIdDatas [ pObjBuffer->nShortObjIds ]	= NULLOBJID;
      ASSERT ( oShortObjIdData != NULLOBJID );
    }
  }
  if ( oShortObjId == NULLOBJID ) {
    fnServerDbCreateInstances ( oShortObjIdHeap, oShortObjIdClassDescr,
				1, (u_int *) &oShortObjId,
				&nSlots, (u_int *) &oShortObjIdData );
  }
  ASSERT ( oShortObjId != NULLOBJID );
  if ( pObjBuffer != NULL ) {
    nLockStore	= fnCacheLockP ( oShortObjIdHeap, NULLOBJID,
				 eshLockStoreWrite );
    if ( (int) nLockStore > 0 ) {
      /* We have a store write lock. This implies that no lock on vector
	 level will be necessary for modifying the object. So, create
	 the object into the cache and mark it as write locked: */
      pHeapObjectCache	=
	fnCacheCreateObject ( oShortObjIdHeap, oShortObjId,
			      (SHTYPETAG) eshInstanceTag,
			      eshInstObjIdSize, NULLTYPETAG, 0, nLockStore );
      ASSERT ( pHeapObjectCache != NULL );
      ASSERT ( pHeapObjectCache->pObjectCache != NULL );
      pHeapObjectCache->pObjectCache->pnTypeTags [ 0 ]	=
	(SHTYPETAG) eshInstanceTag;
      pHeapObjectCache->pObjectCache->poSlots [ 0 ]	=
	SHORT2LONGOBJID ( oShortObjIdClassDescr );
      pHeapObjectCache->pObjectCache->pnTypeTags [ 1 ]	=
	(SHTYPETAG) eshVectorTag;
      pHeapObjectCache->pObjectCache->poSlots [ 1 ]	=
	SHORT2LONGOBJID ( oShortObjIdData );
      pHeapObjectCache	=
	fnCacheCreateObject ( oShortObjIdHeap, oShortObjIdData,
			      (SHTYPETAG) eshVectorTag,
			      pObjBuffer->nSlots, NULLTYPETAG, 0, nLockStore );
      ASSERT ( pHeapObjectCache != NULL );
      ASSERT ( pHeapObjectCache->pObjectCache != NULL );
    }
  }
  RETURN ( oShortObjId );
} EndFunction ( fnClientDbCreateInstance );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientInstanceWriteWrapper,
		"c-sh-write-instance-wrapper",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdClassDescr ) ) )
{
  SHLOCK		nLockOld;
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  nLockOld	=
    fnServerInstanceWriteWrapper ( oShortObjIdHeap, oShortObjIdInstance,
			       oShortObjIdClassDescr );
  if ( bGlobalDoCaching && (int) nLockOld >= 0 ) {
    pHeapObjectCache	=
      fnCacheGetObject ( oShortObjIdHeap, oShortObjIdInstance );
    if ( pHeapObjectCache != NULL ) {
      pHeapObjectCache->pObjectCache->poSlots [ eshInstIdxClassWrapper ] =
	SHORT2LONGOBJID ( oShortObjIdClassDescr );
      pHeapObjectCache->pObjectCache->pnTypeTags [ eshInstIdxClassWrapper ] =
	(SHTYPETAG) eshInstanceTag;
    }
  }
  RETURN ( nLockOld );
} EndFunction ( fnClientInstanceWriteWrapper );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientInstanceWriteData,
		"c-sh-write-instance-data",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdData ) ) )
{
  SHLOCK		nLockOld;
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  nLockOld	=
    fnServerInstanceWriteData ( oShortObjIdHeap, oShortObjIdInstance,
			    oShortObjIdData );
  if ( bGlobalDoCaching && (int) nLockOld >= 0 ) {
    pHeapObjectCache	=
      fnCacheGetObject ( oShortObjIdHeap, oShortObjIdInstance );
    if ( pHeapObjectCache != NULL ) {
      pHeapObjectCache->pObjectCache->poSlots [ eshInstIdxDataVector ]	=
	SHORT2LONGOBJID ( oShortObjIdData );
      pHeapObjectCache->pObjectCache->pnTypeTags [ eshInstIdxDataVector ] =
	(SHTYPETAG) eshVectorTag;
    }
  }
  RETURN ( nLockOld );
} EndFunction ( fnClientInstanceWriteData );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
