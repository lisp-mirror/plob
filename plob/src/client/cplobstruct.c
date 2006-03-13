/* -------------------------------------------------------------------------
| Module	cplobstruct.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB client source code.
|
| Copyright	PLOB! Copyright 1994--2006 Heiko Kirschke.
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
| $Id$
|
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
#include	"cplobregex.h"
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
  BOOL		bMapped, bFreeObjIds;
  POBJBUFFER	pObjBuffer;
  u_int		j, n;

  PROCEDURE	( fnInvalidateStructCache );

  pClient	= fnClientPlobd ();
  bFreeObjIds	= ( pClient != NULL && fnClientPlobdFlush ( pClient ) );

  for ( bMapped = fnHashFirst ( &PreAllocated, (LPHASHKEY) NULL,
				(LPVOID *) &pObjBuffer, (size_t *) NULL );
	bMapped;
	bMapped = fnHashNext ( &PreAllocated, (LPHASHKEY) NULL,
			       (LPVOID *) &pObjBuffer, (size_t *) NULL ) ) {
    ASSERT ( pObjBuffer->nShortObjIds <=
	     length ( pObjBuffer->ShortObjIds ) );
    if ( bFreeObjIds ) {
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
  buffer-file-coding-system: raw-text-unix
  End:
*/
