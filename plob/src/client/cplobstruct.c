/* -------------------------------------------------------------------------
| Module	cplobstruct.c
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
#include	"cplobnumber.h"
#include	"cplobsequ.h"
#include	"cplobstruct.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobroot.h"
#include	"cplobclos.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* Persistent structure object pre-allocation. The idea is not to
   allocate structure objects one-by-one on client requests done by
   the LISP layer to the server, but to allocate a whole bunch of
   structure objects in this layer from the server and to give it
   one-by-one to the LISP layer: */
typedef struct {
  /* Number of slots in the structure: */
  u_int		nSlots;
  /* Number of preallocated objects: */
  u_int		nShortObjIds;
  /* The short objids of the preallocated objects: */
  SHORTOBJID	ShortObjIds [ 64 ];
}	OBJBUFFER, * POBJBUFFER;

/* key is a structure description object, data is an OBJBUFFER: */
static HASHTABLE	PreAllocated;

static OBJBUFFER	ZeroObjBuffer;

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeStructModule	( void )
{
  PROCEDURE	( fnInitializeStructModule );

  HashCreate ( &PreAllocated, 32, sizeof ( OBJBUFFER ) );
  memset ( &ZeroObjBuffer, 0, sizeof ( ZeroObjBuffer ) );

  RETURN ( VOID );
} /* fnInitializeStructModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeStructModule	( void )
{
  PROCEDURE	( fnDeinitializeStructModule );

  fnInvalidateStructCache ();
  fnHashDestroy ( &PreAllocated );

  RETURN ( VOID );
} /* fnDeinitializeStructModule */

/* ----------------------------------------------------------------------- */
void			fnInvalidateStructCache		( void )
{
  CLIENT	*pClient	= (CLIENT *) NULL;
  BOOL		bMapped;
  POBJBUFFER	pObjBuffer;
  u_int		j, n;

  PROCEDURE	( fnInvalidateStructCache );

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
	fnServerObjectDestroy ( NULLOBJID, pObjBuffer->ShortObjIds [ j ] );
	pObjBuffer->nShortObjIds--;
	pObjBuffer->ShortObjIds [ j ]	= NULLOBJID;
      }
      ASSERT ( pObjBuffer->nShortObjIds == 0 );
    } else {
      /* No connection to server; simply forget all cached objids: */
      pObjBuffer->nShortObjIds	= 0;
      memset ( pObjBuffer->ShortObjIds, 0,
	       sizeof ( pObjBuffer->ShortObjIds ) );
    }
  }
  RETURN ( VOID );
} /* fnInvalidateStructCache */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbCreateStructure, "c-sh-create-structure",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in,
			     oShortObjIdStructDescr ) ) )
{
  SHORTOBJID		oShortObjId = NULLOBJID;
  POBJBUFFER		pObjBuffer = (POBJBUFFER) NULL;
  u_int			nSlots;
  SHLOCK		nLockStore;
  PHEAPOBJECTCACHE	pHeapObjectCache = (PHEAPOBJECTCACHE) NULL;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching && oShortObjIdStructDescr != NULLOBJID ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdStructDescr );
    pObjBuffer	= (POBJBUFFER)
      HashGet ( &PreAllocated, oShortObjIdStructDescr );
    if ( pObjBuffer == NULL ) {
      HashInsert ( &PreAllocated, oShortObjIdStructDescr, &ZeroObjBuffer );
      pObjBuffer	=  (POBJBUFFER)
	HashGet ( &PreAllocated, oShortObjIdStructDescr );
      ASSERT ( pObjBuffer != NULL );
    }
    /* Check if there are still objects left in the pre-allocate buffer;
       if not, allocate a new bunch of objects: */
    if ( pObjBuffer->nShortObjIds <= 0 ) {
      pObjBuffer->nShortObjIds	=
	fnServerDbCreateStructures ( oShortObjIdHeap, oShortObjIdStructDescr,
			       length ( pObjBuffer->ShortObjIds ),
			       (u_int *) pObjBuffer->ShortObjIds,
			       &pObjBuffer->nSlots );
    }
    if ( pObjBuffer->nShortObjIds > 0 ) {
      /* Take one object from the pre-allocate buffer: */
      oShortObjId	=
	pObjBuffer->ShortObjIds [ --(pObjBuffer->nShortObjIds) ];
      pObjBuffer->ShortObjIds [ pObjBuffer->nShortObjIds ] = NULLOBJID;
      ASSERT ( oShortObjId != NULLOBJID );
    }
  }
  if ( oShortObjId == NULLOBJID ) {
    fnServerDbCreateStructures ( oShortObjIdHeap, oShortObjIdStructDescr,
			   1, (u_int *) &oShortObjId, &nSlots );
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
			      (SHTYPETAG) eshStructureTag,
			      pObjBuffer->nSlots, NULLTYPETAG, 0, nLockStore );
      ASSERT ( pHeapObjectCache != NULL );
      ASSERT ( pHeapObjectCache->pObjectCache != NULL );
      pHeapObjectCache->pObjectCache->pnTypeTags [ 0 ]	= (SHTYPETAG)
	eshStructureTag;
      pHeapObjectCache->pObjectCache->poSlots [ 0 ]	=
	SHORT2LONGOBJID ( oShortObjIdStructDescr );
    }
  }
  RETURN ( oShortObjId );
} EndFunction ( fnClientDbCreateStructure );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
