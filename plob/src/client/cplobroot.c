/* -------------------------------------------------------------------------
| Module	cplobroot.c
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
#include	"cplobsequ.h"
#include	"cplobstruct.h"
#include	"cplobclos.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobroot.h"
#include	"cplobadmin.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeRootModule		( void )
{
  PROCEDURE	( fnInitializeRootModule );

  RETURN ( VOID );
} /* fnInitializeRootModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeRootModule	( void )
{
  PROCEDURE	( fnDeinitializeRootModule );

  RETURN ( VOID );
} /* fnDeinitializeRootModule */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbWriteRoot, "c-sh-write-root",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  SHORTOBJID	oShortObjIdHeapNew;
  PHEAPCACHE	pHeapCache;
  TRACTID	nTractId;

  INITIALIZEPLOB;
  /* Flush all objects ... */
  fnCacheFlush ( NULLOBJID, NULLOBJID );
  /* ... and invalidate all caches: */
  fnInvalidateAllCaches ();
  oShortObjIdHeapNew	= fnServerDbWriteRoot ( oShortObjIdHeap, oShortObjId );
  if ( oShortObjId == NULLOBJID && oShortObjIdHeapNew != NULLOBJID ) {
    /* The root object has been re-formatted, so all cached data
       becomes invalid.  Destroy the client's object cache ... */
    fnCacheDestroy ( NULLOBJID );
    /* ... and reload the session: */
    pHeapCache	= fnCacheCreate ( oShortObjIdHeapNew );
    ASSERT ( pHeapCache != NULL );
    if ( bGlobalDoCaching ) {
      nTractId	= fnServerDbTransactionP ( oShortObjIdHeapNew, NULLTRACTID );
      pHeapCache->onTractId	= ( nTractId == NULLTRACTID ) ?
	unbound : Fixnum2ObjId ( nTractId );
    }
  }
  RETURN ( oShortObjIdHeapNew );
} EndFunction ( fnClientDbWriteRoot );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientGetVersion, "c-sh-get-version",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( GETVERSION, value_in, eWhat ) ) )
{
  FIXNUM	nVersion	= 0;

  INITIALIZEPLOB;

  switch ( eWhat ) {
  case esvClientCcode:
    nVersion	= PlobVersion;
    break;
  default:
    if ( fnClientPlobd () != NULL ) {
      nVersion	= fnServerGetVersion ( oShortObjIdHeap, eWhat );
    }
    break;
  }

  RETURN ( nVersion );
} EndFunction ( fnClientGetVersion );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
