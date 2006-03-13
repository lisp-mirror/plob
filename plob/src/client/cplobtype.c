/* -------------------------------------------------------------------------
| Module	cplobtype.c
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
#include	"cplobregex.h"
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

/* ----------------------------------------------------------------------- */
BeginFunction ( COMPARETAG,
		fnClientObjectCompare, "c-sh-compare",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( FIXNUM, value_in, nValueFirst )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagFirst )
		  and
		  argument ( FIXNUM, value_in, nValueSecond )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagSecond ) ) )
{
  OBJID		oFirst = NULLOBJID, oSecond = NULLOBJID;
  COMPARETAG	eCompared = eshNotEq;

  INITIALIZEPLOB;

  oFirst	= ( immediatep ( nTypeTagFirst ) ) ?
    fnImmediate2ObjId ( nValueFirst, &nTypeTagFirst ) :
    SHORT2LONGOBJID ( nValueFirst );
  oSecond	= ( immediatep ( nTypeTagSecond ) ) ?
    fnImmediate2ObjId ( nValueSecond, &nTypeTagSecond ) :
    SHORT2LONGOBJID ( nValueSecond );

  if ( oFirst == oSecond ) {
    eCompared	= eshEq;
  } else if ( minmarkerp ( oFirst ) || maxmarkerp ( oSecond )) {
    eCompared	= eshLess;
  } else if ( maxmarkerp ( oFirst ) || minmarkerp ( oSecond )) {
    eCompared	= eshGreater;
  } else if ( matchanymarkerp ( oFirst ) || matchanymarkerp ( oSecond ) ) {
    eCompared	= eshEqual;
  } else if ( matchnevermarkerp ( oFirst ) || matchnevermarkerp ( oSecond ) ) {
    eCompared	= eshNotEqual;
  } else if ( immediatep ( oFirst ) && immediatep ( oSecond ) ) {
    if ( nTypeTagFirst == nTypeTagSecond ) {
      switch ( nTypeTagFirst ) {
      case eshShortFloatTag:
	eCompared	= ( OBJID2SINGLEFLOAT ( oFirst ) <
			    OBJID2SINGLEFLOAT ( oSecond ) ) ?
	  eshLess : eshGreater;
	break;
      default:
	eCompared	= ( nValueFirst < nValueSecond ) ?
	  eshLess : eshGreater;
	break;
      }
    } else {
      eCompared	= eshNotEq;
    }
  } else {
    if ( bGlobalDoCaching ) {
      if ( oFirst != NULLOBJID && ! immediatep ( nTypeTagFirst)) {
	fnCacheFlush ( NULLOBJID, LONG2SHORTOBJID ( oFirst ) );
      }
      if ( oSecond != NULLOBJID && ! immediatep ( nTypeTagSecond)) {
	fnCacheFlush ( NULLOBJID, LONG2SHORTOBJID ( oSecond ) );
      }
    }
    eCompared	=
      fnServerObjectCompare ( SHORT2LONGOBJID ( oShortObjIdHeap ),
			      oFirst, oSecond );
  }

  RETURN ( eCompared );

} EndFunction ( fnClientObjectCompare );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
