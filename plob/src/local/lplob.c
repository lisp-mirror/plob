/* -------------------------------------------------------------------------
| Module	lplob.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
| Description	PLOB local source code.
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

#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobintern.h"
#include	"lplobmisc.h"
#include	"lplobtype.h"
#include	"lplobnumber.h"
#include	"lplobstruct.h"
#include	"lplobclos.h"
#include	"lploblock.h"
#include	"lplobheap.h"
#include	"lplobbtree.h"
#include	"lplobroot.h"
#include	"lplobadmin.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnClientDbClose, "c-sh-close",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bWithGarbageCollection ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  fnServerDbClose ( oShortObjIdHeap, bWithGarbageCollection );
  RETURN ( VOID );
} EndFunction ( fnClientDbClose );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbCreateObject, "c-sh-create-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( FIXNUM, value_in, nExtraReferences )
		   and
		   argument ( SHTYPETAG, value_in, eTypeTagValues )
		   and
		   argument ( FIXNUM, value_in, nExtraValues ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  RETURN ( fnServerDbCreateObject ( oShortObjIdHeap,
				    nTypeTag, nExtraReferences,
				    eTypeTagValues, nExtraValues ) );
} EndFunction ( fnClientDbCreateObject );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnClientObjectDestroy, "c-sh-destroy-object",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  fnServerObjectDestroy ( oShortObjIdHeap, oShortObjId );
  RETURN ( VOID );
} EndFunction ( fnClientObjectDestroy );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientObjectObjIdSize, "c-sh-objid-size",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{ 
  INITIALIZEPLOB;
  UNSTORESESSION ();
  RETURN ( fnServerObjectObjIdSize ( oShortObjIdHeap, oShortObjId ) );
} EndFunction ( fnClientObjectObjIdSize );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientDbConnect, "c-sh-connect",
		( argument ( CONST_STRING, vector_in, szURL ) ) )
{
  BOOL	bDone = FALSE;

  INITIALIZEPLOB;

  strncpy ( szGlobalDirectory, szURL, sizeof ( szGlobalDirectory ) );
  bDone	= TRUE;

  RETURN ( bDone );
} EndFunction ( fnClientDbConnect );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbOpen, "c-sh-open",
		 ( argument ( CONST_STRING, vector_in, szDescription ) 
		   and
		   argument ( FIXNUM, value_in, nMinAddrInK ) ) )
{
  SHORTOBJID	oShortHeap = NULLOBJID;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  /* 1998/07/17 HK: Debug: */
  /*
  INFO (( "&nGlobalFlagWord 0x%X", &nGlobalFlagWord ));
  INFO (( "nGlobalFlagWord    %d", nGlobalFlagWord ));
  */

  if ( ! szGlobalDirectory [ 0 ] ) {
    ERROR (( "Missing call to connect() before open()" ));
  } else {
    char	szDirectory [ MAX_FNAME ];
    LPCSTR	pszUser	= (LPCSTR) NULL;
    szDirectory [ 0 ]	= '\0';
    fnSplitURL ( szGlobalDirectory, (LPSTR) NULL, (LPSTR) NULL, (LPSTR) szDirectory );
    pszUser		= fnGetUser ();
    if ( ! fnDatabaseP ( szDirectory ) ) {
      CERROR (( "Try to create it.",
		"Could not locate database local:%s", szDirectory ));
      SH_create_database ( szDirectory, fnPLOBerrorCallback );
      if ( ! fnDatabaseP ( szDirectory ) ) {
	ERROR (( "Could not create database local:%s", szDirectory ));
	RETURN ( NULLOBJID );
      }
    }

    fnLogSetDirectory ( szDirectory );
    oShortHeap	=
      fnServerDbOpen ( szDirectory, pszUser, szDescription, nMinAddrInK,
		       (LPOBJID) NULL, (LPOBJID) NULL );
  }

  RETURN ( oShortHeap );
} EndFunction ( fnClientDbOpen );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientObjectFlush, "c-sh-flush-object",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( BOOL, value_in, bRemoveFromCache ) ) )
{
  BOOL	bDone = FALSE;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  bDone	= fnServerObjectFlush ( oShortObjIdHeap, oShortObjId );

  RETURN ( bDone );
} EndFunction ( fnClientObjectFlush );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectReadAtIndex, "c-sh-read-index",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( INTEGER, value_out, pnValue )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTag ) ) )

{
  SHLOCK	nLockOld;

  INITIALIZEPLOB;
  UNSTORESESSION ();
  ASSERT ( pnValue != NULL );
  ASSERT ( pnTypeTag != NULL );

  nLockOld	=  fnServerObjectReadAtIndex ( oShortObjIdHeap, oShortObjId,
					       oExpectingClass,
					       nExpectingTypeTag,
					       nIndex, pnValue, pnTypeTag );
  RETURN ( nLockOld );
} EndFunction ( fnClientObjectReadAtIndex );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectReadAtIndices, "c-sh-read-indices",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( int, nObjIds ), vector_out, pObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ), vector_out,
			     pnTypeTags ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  ASSERT ( pObjIds != NULL );
  ASSERT ( pnTypeTags != NULL );
  RETURN ( fnServerObjectReadAtIndices ( oShortObjIdHeap, oShortObjId,
					 oExpectingClass, nExpectingTypeTag,
					 nIndex, nObjIds, (int *) pObjIds,
					 pnTypeTags ) );
} EndFunction ( fnClientObjectReadAtIndices );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectReadObjId, "c-sh-read-objid",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( INTEGER, value_out, pnObjId ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  ASSERT ( pnObjId != NULL );
  RETURN ( fnServerObjectReadObjId ( oShortObjIdHeap, oShortObjId,
				     oExpectingClass, nExpectingTypeTag,
				     nIndex, pnObjId ) );
} EndFunction ( fnClientObjectReadObjId );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectReadValues, "c-sh-read-values",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( SHTYPETAG, value_in, nElementTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInElements )
		  and
		  argument ( VECTOR ( void,
				      fnTypeTagSizeValue(1,&nElementTypeTag,
							 &nSizeInElements ) ),
			     vector_out, pBuffer )
		  and
		  argument ( FIXNUM, value_in, nUnmask )
		  and
		  argument ( FIXNUM, value_in, nBufferOffset ) ) )
{
  LPVOID	pCooked;
  int		nR;
  SHTYPETAG	nE;

  INITIALIZEPLOB;
  UNSTORESESSION ();
  ASSERT ( nSizeInElements == 0 || pBuffer != NULL );
  pCooked	=
    fnMakeLispPointer ( pBuffer, FALSE, nUnmask, nBufferOffset );
  RETURN ( fnServerObjectReadValues ( oShortObjIdHeap, oShortObjId,
				      oExpectingClass, nExpectingTypeTag,
				      nIndex, nElementTypeTag,
				      nSizeInElements, &nE, &nR, pCooked ) );
} EndFunction ( fnClientObjectReadValues );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnClientDbStabilise, "c-sh-stabilise",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  fnServerDbStabilise ( oShortObjIdHeap );
  RETURN ( VOID );
} EndFunction ( fnClientDbStabilise );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHTYPETAG,
	        fnClientObjectTypeTag, "c-sh-type-tag",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  RETURN ( fnServerObjectTypeTag ( oShortObjIdHeap, oShortObjId ) );
} EndFunction ( fnClientObjectTypeTag );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectWriteAtIndex, "c-sh-write-index",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( FIXNUM, value_in, nValue )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagValue ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  RETURN ( fnServerObjectWriteAtIndex ( oShortObjIdHeap, oShortObjId,
					oExpectingClass, nExpectingTypeTag,
					nIndex, nValue, nTypeTagValue ) );
} EndFunction ( fnClientObjectWriteAtIndex );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectWriteAtIndices, "c-sh-write-indices",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( int, nObjIds ),
			     vector_in, pObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_in, pnTypeTags ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  RETURN ( fnServerObjectWriteAtIndices ( oShortObjIdHeap, oShortObjId,
					  oExpectingClass, nExpectingTypeTag,
					  nIndex, nObjIds, (int *) pObjIds,
					  pnTypeTags ) );
} EndFunction ( fnClientObjectWriteAtIndices );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectWriteObjId, "c-sh-write-objid",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdWrite ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  RETURN ( fnServerObjectWriteObjId ( oShortObjIdHeap, oShortObjId,
				      oExpectingClass, nExpectingTypeTag,
				      nIndex, oShortObjIdWrite ) );
} EndFunction ( fnClientObjectWriteObjId );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectWriteValues, "c-sh-write-values",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( SHTYPETAG, value_in, nElementTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInElements )
		  and
		  argument ( VECTOR ( void,
				      fnTypeTagSizeValue(1,&nElementTypeTag,
							 &nSizeInElements ) ),
			     vector_in,
			     pBuffer )
		  and
		  argument ( FIXNUM, value_in, nUnmask )
		  and
		  argument ( FIXNUM, value_in, nBufferOffset ) ) )
{
  LPVOID	pCooked;

  INITIALIZEPLOB;
  UNSTORESESSION ();
  pCooked	=
    fnMakeLispPointer ( pBuffer, FALSE, nUnmask, nBufferOffset );
  RETURN ( fnServerObjectWriteValues ( oShortObjIdHeap, oShortObjId,
				       oExpectingClass, nExpectingTypeTag,
				       nIndex, nElementTypeTag,
				       nSizeInElements, pCooked ) );
} EndFunction ( fnClientObjectWriteValues );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
