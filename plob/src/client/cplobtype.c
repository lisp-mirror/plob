/* -------------------------------------------------------------------------
| Module	cplobtype.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB client source code.
 ------------------------------------------------------------------------- */

#include	<limits.h>
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
#include	"cplobclos.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobroot.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
OBJID			__oTypeTagHeap__	= NULLOBJID;
OBJID			__oTypeTagOf__		= NULLOBJID;
OBJID			oTypeTagCache		= NULLOBJID;
SHTYPETAG		nTypeTagCache		= (SHTYPETAG) NULLTYPETAG;

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeTypeModule		( void )
{
  PROCEDURE	( fnInitializeTypeModule );

  RETURN ( VOID );
} /* fnInitializeTypeModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeTypeModule	( void )
{
  PROCEDURE	( fnDeinitializeTypeModule );

  RETURN ( VOID );
} /* fnDeinitializeTypeModule */

/* ----------------------------------------------------------------------- */
SHTYPETAG		fnTypeTagOf		( OBJID oHeap,
						  OBJID oObjId )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;
  SHTYPETAG		nTypeTag;

  PROCEDURE	( fnTypeTagOf );

  INITIALIZEPLOB;
  switch ( oObjId & nTagMask ) {
  case eshObjIdTag:
    pHeapObjectCache	= ( oHeap != NULLOBJID ) ?
      fnCacheGetObject ( LONG2SHORTOBJID ( oHeap ),
			 LONG2SHORTOBJID ( oObjId ) ) :
			   (PHEAPOBJECTCACHE) NULL;
    nTypeTag		= ( pHeapObjectCache != NULL ) ?
      pHeapObjectCache->pObjectCache->nTypeTag :
	fnServerObjectTypeTag ( LONG2SHORTOBJID ( oHeap ),
				LONG2SHORTOBJID ( oObjId ) );
    break;
  case eshFixnumTag: case eshFixnumTag | ( 1 << nFixnumBitOffset ):
    nTypeTag	= eshFixnumTag;
    break;
  case eshMarkerTag:
    nTypeTag	= (SHTYPETAG) oObjId;
    break;
  default:
    nTypeTag	= (SHTYPETAG) ( (unsigned int) oObjId &
				(unsigned int) nTagMask );
    break;
  }
  if ( oObjId != NULLOBJID ) {
    oTypeTagCache	= oObjId;
    nTypeTagCache	= nTypeTag;
  }
  RETURN ( nTypeTag );
} /* fnTypeTagOf */

/* ----------------------------------------------------------------------- */
#ifdef __cplusplus
typedef LPSTR	(* LPFNSTR) (...);
#else
typedef LPSTR	(* LPFNSTR) ();
#endif
LPSTR		gfnPrintObjectDetails	( OBJID oSelf,
					  LPSTR lpszBuffer,
					  size_t nBuffer )
{
  LPSTR		lpszDetails;
  LPCLASSINFO	lpClassInfo;
  LPFNMETHOD	lpfnMethod;
  LPOBJID	lpSHvector	= (LPOBJID) NULL;

  PROCEDURE	( gfnPrintObjectDetails );

  ASSERT ( lpszBuffer != NULL );
  *lpszBuffer	= '\0';
  lpszDetails	= lpszBuffer;
  lpClassInfo	= (LPCLASSINFO)
    FindClassInfo ( typetagof ( NULLOBJID, oSelf ) );
  if ( lpClassInfo ) {
    lpfnMethod	= (LPFNMETHOD) FindMethod ( lpClassInfo->nTypeTag );
    if ( lpfnMethod ) {
      lpszDetails	=
	( * (LPFNSTR) lpfnMethod ) ( oSelf, lpSHvector, lpClassInfo,
				     lpszBuffer, nBuffer );
    }
  }
  RETURN ( lpszDetails );
} /* gfnPrintObjectDetails */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientObjectPrettyPrint, "c-sh-pprint-objid",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTag )
		  and
		  argument ( STRING ( nBuffer ),
			     vector_out, lpszBuffer )
		  and
		  argument ( FIXNUM, value_in, nBuffer ) ) )
{
  char	szBuffer [ 256 ];

  INITIALIZEPLOB;

  ASSERT ( lpszBuffer != NULL );

  if ( immediatep ( nTypeTag ) ) {
    /* Print immediates locally on client side: */
    fnPrintImmediateObject ( oShortObjId, nTypeTag, lpszBuffer, nBuffer );
    RETURN ( strlen ( lpszBuffer ) );
  }

  if ( fnClientPlobd () != NULL ) {
    if ( bGlobalDoCaching && oShortObjId != NULLOBJID ) {
      fnCacheFlush ( NULLOBJID, oShortObjId );
    }
    RETURN ( fnServerObjectPrettyPrint ( oShortObjIdHeap, oShortObjId,
					 nTypeTag, lpszBuffer, nBuffer ) );
  }

  sprintf ( szBuffer,
	    UNREADABLE_OBJECT_PREFIX
	    "t short-objid=%d"
	    UNREADABLE_OBJECT_SUFFIX, oShortObjId );
  strncpy ( lpszBuffer, szBuffer, nBuffer );

  RETURN ( strlen ( lpszBuffer ) );
} EndFunction ( fnClientObjectPrettyPrint );


/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
