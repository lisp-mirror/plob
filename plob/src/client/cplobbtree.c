/* -------------------------------------------------------------------------
| Module	cplobbtree.c
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
#include	"cplobsequ.h"
#include	"cplobtype.h"
#include	"cplobnumber.h"
#include	"cplobroot.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobstruct.h"
#include	"cplobclos.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeBTreeModule	( void )
{
  PROCEDURE	( fnInitializeBTreeModule );

  RETURN ( VOID );
} /* fnInitializeBTreeModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeBTreeModule	( void )
{
  PROCEDURE	( fnDeinitializeBTreeModule );

  RETURN ( VOID );
} /* fnDeinitializeBTreeModule */

/* ----------------------------------------------------------------------- */
static void	fnFlushObject	( SHORTOBJID	oHeap,
				  FIXNUM	nValue,
				  SHTYPETAG	nTypeTag )
{
  PROCEDURE	( fnFlushObject );

  if ( ! immediatep ( nTypeTag ) ) {
    fnCacheFlush ( oHeap, (SHORTOBJID) nValue );
  }
  RETURN ( VOID );
} /* fnFlushObject */

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
	        fnClientBtreeDelete, "c-sh-btree-delete",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( FIXNUM, value_in, nValueKey )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKey ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnFlushObject ( oShortObjIdHeap, nValueKey, nTypeTagKey );
  }
  RETURN ( fnServerBtreeDelete ( oShortObjIdHeap, oShortObjIdBTree,
				 nValueKey, nTypeTagKey ) );
} EndFunction ( fnClientBtreeDelete );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeDeleteByFloat, "c-sh-btree-delete-by-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKey ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeDeleteByFloat ( oShortObjIdHeap, oShortObjIdBTree,
					fKey ) );
} EndFunction ( fnClientBtreeDeleteByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeDeleteByDouble, "c-sh-btree-delete-by-double",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKey ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeDeleteByDouble ( oShortObjIdHeap, oShortObjIdBTree,
					 fKey ) );
} EndFunction ( fnClientBtreeDeleteByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeDeleteByString, "c-sh-btree-delete-by-string",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( CONST_STRING, vector_in, szKey ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeDeleteByString ( oShortObjIdHeap, oShortObjIdBTree,
					 szKey ) );
} EndFunction ( fnClientBtreeDeleteByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
	        fnClientBtreeInsert, "c-sh-btree-insert",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( FIXNUM, value_in, nValueKey )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKey )
		  and
		  argument ( FIXNUM, value_in, nValueData )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnFlushObject ( oShortObjIdHeap, nValueKey, nTypeTagKey );
    fnFlushObject ( oShortObjIdHeap, nValueData, nTypeTagData );
  }
  RETURN ( fnServerBtreeInsert ( oShortObjIdHeap, oShortObjIdBTree,
				 nValueKey, nTypeTagKey,
				 nValueData, nTypeTagData ) );
} EndFunction ( fnClientBtreeInsert );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeInsertByFloat, "c-sh-btree-insert-by-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKey )
		  and
		  argument ( FIXNUM, value_in, nValueData )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeInsertByFloat ( oShortObjIdHeap, oShortObjIdBTree,
					fKey, nValueData, nTypeTagData ) );
} EndFunction ( fnClientBtreeInsertByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeInsertByDouble, "c-sh-btree-insert-by-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKey )
		  and
		  argument ( FIXNUM, value_in, nValueData )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeInsertByDouble ( oShortObjIdHeap, oShortObjIdBTree,
					 fKey, nValueData, nTypeTagData ) );
} EndFunction ( fnClientBtreeInsertByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeInsertByString, "c-sh-btree-insert-by-string",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( CONST_STRING, vector_in, szKey )
		  and
		  argument ( FIXNUM, value_in, nValueData )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeInsertByString ( oShortObjIdHeap, oShortObjIdBTree,
					 szKey, nValueData, nTypeTagData ) );
} EndFunction ( fnClientBtreeInsertByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreemapSearch, "c-sh-btree-map-search",
		( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( FIXNUM, value_in, nValueKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( FIXNUM, value_in, nValueKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdMapper );
    fnFlushObject ( oShortObjIdHeap, nValueKeyLower, nTypeTagKeyLower );
    fnFlushObject ( oShortObjIdHeap, nValueKeyUpper, nTypeTagKeyUpper );
  }
  RETURN ( fnServerBtreemapSearch ( oShortObjIdMapper, oShortObjIdHeap,
				    oShortObjIdBTree,
				    nValueKeyLower, nTypeTagKeyLower,
				    eCompareLower,
				    nValueKeyUpper, nTypeTagKeyUpper,
				    eCompareUpper,
				    bDescending ) );
} EndFunction ( fnClientBtreemapSearch );

/* ----------------------------------------------------------------------- */
/* 1996/10/29 HK: The following declaration of
   fnClientBtreemapSearchByFloat should look like:
BeginFunction ( FIXNUM,
		fnClientBtreemapSearchByFloat, "c-sh-btree-map-search-by-float",
		( argument ( SHORTOBJID,
			     value_out, lpoShortObjIdMapper )
		  and
		  <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
BeginFunction ( voidResult,
		fnClientBtreemapSearchByFloat, "c-sh-btree-map-search-by-float",
		( argument ( FIXNUM, value_out, pnReturnValue )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending ) ) )
{
  INITIALIZEPLOB;
  ASSERT ( pnReturnValue != NULL );
  if ( bGlobalDoCaching ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdMapper );
  }
  *pnReturnValue	=
    fnServerBtreemapSearchByFloat ( oShortObjIdMapper,
				    oShortObjIdHeap, oShortObjIdBTree,
				    fKeyLower, nTypeTagKeyLower, eCompareLower,
				    fKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				    bDescending );
  RETURN ( VOID );
} EndFunction ( fnClientBtreemapSearchByFloat );

/* ----------------------------------------------------------------------- */
/* 1996/10/29 HK: The following declaration of
   fnClientBtreemapSearchByDouble should look like:
BeginFunction ( FIXNUM,
                fnClientBtreemapSearchByDouble,
		"c-sh-btree-map-search-by-double",
		( argument ( SHORTOBJID,
			     value_out, lpoShortObjIdMapper )
		  and
		  <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
BeginFunction ( voidResult,
		fnClientBtreemapSearchByDouble,
		"c-sh-btree-map-search-by-double",
		( argument ( FIXNUM, value_out, pnReturnValue )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending ) ) )
{
  INITIALIZEPLOB;
  ASSERT ( pnReturnValue != NULL );
  if ( bGlobalDoCaching ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdMapper );
  }
  *pnReturnValue	=
    fnServerBtreemapSearchByDouble ( oShortObjIdMapper,
				     oShortObjIdHeap, oShortObjIdBTree,
				     fKeyLower, nTypeTagKeyLower,
				     eCompareLower,
				     fKeyUpper, nTypeTagKeyUpper,
				     eCompareUpper,
				     bDescending );
  RETURN ( VOID );
} EndFunction ( fnClientBtreemapSearchByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreemapSearchByString,
		"c-sh-btree-map-search-by-string",
		( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( CONST_STRING, vector_in, szKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( CONST_STRING, vector_in, szKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdMapper );
  }
  RETURN ( fnServerBtreemapSearchByString ( oShortObjIdMapper,
					    oShortObjIdHeap, oShortObjIdBTree,
					    szKeyLower, nTypeTagKeyLower,
					    eCompareLower,
					    szKeyUpper, nTypeTagKeyUpper,
					    eCompareUpper,
					    bDescending ) );
} EndFunction ( fnClientBtreemapSearchByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreemapSeek, "c-sh-btree-map-seek",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( FIXNUM, value_in, nIncrement )
		  and
		  argument ( SEEK, value_in, eOrigin )
		  and
		  argument ( FIXNUM, value_out, pnValueKey )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdMapper );
  }
  RETURN ( fnServerBtreemapSeek ( oShortObjIdHeap, oShortObjIdMapper,
				  nIncrement, eOrigin,
				  pnValueKey, pnTypeTagKey,
				  pnValueData, pnTypeTagData ) );
} EndFunction ( fnClientBtreemapSeek );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreemapSeekSet, "c-sh-btree-map-seek-set",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( FIXNUM, value_in, nIncrement )
		  and
		  argument ( SEEK, value_in, eOrigin )
		  and
		  argument ( FIXNUM, value_in, nValueData )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnCacheFlush ( oShortObjIdHeap, oShortObjIdMapper );
    fnFlushObject ( oShortObjIdHeap, nValueData, nTypeTagData );
  }
  RETURN ( fnServerBtreemapSeekSet ( oShortObjIdHeap, oShortObjIdMapper,
				     nIncrement, eOrigin,
				     nValueData, nTypeTagData ) );
} EndFunction ( fnClientBtreemapSeekSet );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreemapFirst, "c-sh-btree-map-first",
		( argument ( SHORTOBJID, value_out, lpoShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( FIXNUM, value_in, nValueKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( FIXNUM, value_in, nValueKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending )
		  and
		  argument ( FIXNUM, value_in, nMap )
		  and	/* ouput arguments: */
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueKey )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagKey )
		  and
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueData )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnFlushObject ( oShortObjIdHeap, nValueKeyLower, nTypeTagKeyLower );
    fnFlushObject ( oShortObjIdHeap, nValueKeyUpper, nTypeTagKeyUpper );
  }
  RETURN ( fnServerBtreemapFirst ( lpoShortObjIdMapper, oShortObjIdHeap,
				   oShortObjIdBTree,
				   nValueKeyLower, nTypeTagKeyLower,
				   eCompareLower,
				   nValueKeyUpper, nTypeTagKeyUpper,
				   eCompareUpper,
				   bDescending, nMap, pnValueKey, pnTypeTagKey,
				   pnValueData, pnTypeTagData ) );
} EndFunction ( fnClientBtreemapFirst );

/* ----------------------------------------------------------------------- */
/* 1996/10/29 HK: The following declaration of
   fnClientBtreemapFirstByFloat should look like:
BeginFunction ( FIXNUM,
		fnClientBtreemapFirstByFloat, "c-sh-btree-map-first-by-float",
		( argument ( SHORTOBJID,
			     value_out, lpoShortObjIdMapper )
		  and
		  <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
BeginFunction ( voidResult,
		fnClientBtreemapFirstByFloat, "c-sh-btree-map-first-by-float",
		( argument ( FIXNUM,
			      value_out, pnReturnValue )
		  and
		  argument ( SHORTOBJID,
			     value_out, lpoShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending )
		  and
		  argument ( FIXNUM, value_in, nMap )
		  and	/* ouput arguments: */
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueKey )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagKey )
		  and
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueData )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  ASSERT ( pnReturnValue != NULL );
  *pnReturnValue	=
    fnServerBtreemapFirstByFloat ( lpoShortObjIdMapper,
				   oShortObjIdHeap, oShortObjIdBTree,
				   fKeyLower, nTypeTagKeyLower, eCompareLower,
				   fKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				   bDescending, nMap,
				   pnValueKey, pnTypeTagKey,
				   pnValueData, pnTypeTagData );
  RETURN ( VOID );
} EndFunction ( fnClientBtreemapFirstByFloat );

/* ----------------------------------------------------------------------- */
/* 1996/10/29 HK: The following declaration of
   fnClientBtreemapFirstByDouble should look like:
BeginFunction ( FIXNUM,
                fnClientBtreemapFirstByDouble,
		"c-sh-btree-map-first-by-double",
		( argument ( SHORTOBJID,
			     value_out, lpoShortObjIdMapper )
		  and
		  <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
BeginFunction ( voidResult,
		fnClientBtreemapFirstByDouble,
		"c-sh-btree-map-first-by-double",
		( argument ( FIXNUM,
			     value_out, pnReturnValue )
		  and
		  argument ( SHORTOBJID,
			     value_out, lpoShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending )
		  and
		  argument ( FIXNUM, value_in, nMap )
		  and	/* ouput arguments: */
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueKey )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagKey )
		  and
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueData )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  *pnReturnValue	=
    fnServerBtreemapFirstByDouble ( lpoShortObjIdMapper,
				    oShortObjIdHeap, oShortObjIdBTree,
				    fKeyLower, nTypeTagKeyLower, eCompareLower,
				    fKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				    bDescending, nMap,
				    pnValueKey, pnTypeTagKey,
				    pnValueData, pnTypeTagData );
  RETURN ( VOID );
} EndFunction ( fnClientBtreemapFirstByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreemapFirstByString,
		"c-sh-btree-map-first-by-string",
		( argument ( SHORTOBJID,
			     value_out, lpoShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( CONST_STRING, vector_in, szKeyLower )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		  and
		  argument ( COMPARETAG, value_in, eCompareLower )
		  and
		  argument ( CONST_STRING, vector_in, szKeyUpper )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		  and
		  argument ( COMPARETAG, value_in, eCompareUpper )
		  and
		  argument ( BOOL, value_in, bDescending )
		  and
		  argument ( FIXNUM, value_in, nMap )
		  and	/* ouput arguments: */
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueKey )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagKey )
		  and
		  argument ( VECTOR ( int, nMap ),
			     vector_out, pnValueData )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreemapFirstByString ( lpoShortObjIdMapper,
					   oShortObjIdHeap, oShortObjIdBTree,
					   szKeyLower, nTypeTagKeyLower,
					   eCompareLower,
					   szKeyUpper, nTypeTagKeyUpper,
					   eCompareUpper,
					   bDescending, nMap,
					   pnValueKey, pnTypeTagKey,
					   pnValueData, pnTypeTagData ) );
} EndFunction ( fnClientBtreemapFirstByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
	        fnClientBtreeSearch, "c-sh-btree-search",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( FIXNUM, value_in, nValueKey )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueKey )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  if ( bGlobalDoCaching ) {
    fnFlushObject ( oShortObjIdHeap, nValueKey, nTypeTagKey );
  }
  RETURN ( fnServerBtreeSearch ( oShortObjIdHeap, oShortObjIdBTree,
				 nValueKey, nTypeTagKey,
				 pnValueKey, pnTypeTagKey,
				 pnValueData, pnTypeTagData ) );
} EndFunction ( fnClientBtreeSearch );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeSearchByFloat, "c-sh-btree-search-by-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKey )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueKey )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeSearchByFloat ( oShortObjIdHeap, oShortObjIdBTree,
					fKey, nTypeTagKey,
					pnValueKey, pnTypeTagKey,
					pnValueData, pnTypeTagData ) );
} EndFunction ( fnClientBtreeSearchByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeSearchByDouble, "c-sh-btree-search-by-double",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKey )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueKey )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeSearchByDouble ( oShortObjIdHeap, oShortObjIdBTree,
					 fKey, nTypeTagKey,
					 pnValueKey, pnTypeTagKey,
					 pnValueData, pnTypeTagData ) );
} EndFunction ( fnClientBtreeSearchByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeSearchByString, "c-sh-btree-search-by-string",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( CONST_STRING, vector_in, szKey )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueKey )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTagData ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnServerBtreeSearchByString ( oShortObjIdHeap, oShortObjIdBTree,
					 szKey, nTypeTagKey,
					 pnValueKey, pnTypeTagKey,
					 pnValueData, pnTypeTagData ) );
} EndFunction ( fnClientBtreeSearchByString );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
