/* -------------------------------------------------------------------------
| Module	plobbtree.h
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		17.12.93
| Description	Foreign language interface to PLOB BTrees.
|
| Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
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
 ------------------------------------------------------------------------- */

#if defined(LISP)
;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plobbtree.h
;;;; --------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#if defined(C2C)

/* -------------------------------------------------------------------------
| PLOB btree instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;		/* PLOB header information */
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oSelf;		/* ObjId of object itself */
  OBJID		oCompare;	/* Kind of compare to use for search */
  OBJID		onCount;	/* Number of items in btree */
  OBJID		obCached;	/* Flag if btree is a cached btree */
  OBJID		onTimeStamp;	/* Time stamp, is incremented for each write */
  OBJID		onPages;	/* Number of pages in btree */
  OBJID		onGCcounter;	/* GC counter at last fnGetBTree */
  OBJID		oRoot;		/* ObjId of first page of btree */
  OBJID		onPageSize;	/* Size of one btree page in elements */
  /* --- Persistent values: ---------------------------------------------- */
}	PLOBBTREE, * LPPLOBBTREE;

/* Struct BTREEITEM is similar to the well-known (?) structure named
   tlatter in LispWorks Common LISP (used there for implementing hash
   tables): */
typedef struct {
  OBJID	oKey;	/* Key field */
  OBJID	oData;	/* Data field */
  OBJID	oNext;	/* ObjId of next btree page */
}	BTREEITEM, * LPBTREEITEM;

/* -------------------------------------------------------------------------
| PLOB btree page instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;		/* PLOB header information */
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oSelf;		/* ObjId of object itself */
  OBJID		oUnderflow;	/* ObjId of next object with underflow */
  OBJID		oNext;		/* ObjId of -1th next btree page */
  OBJID		oParentPage;	/* ObjId of parent page */
  OBJID		oParentIndex;	/* Index into parent page */
  OBJID		onCount;	/* Number of items in this page */
  BTREEITEM	Items [ /* nBtreeDefaultPageSize */ 678 ];	/* Items */
  /* --- Persistent values: ---------------------------------------------- */
}	BTREEPAGE, * LPBTREEPAGE;

/* -------------------------------------------------------------------------
| PLOB btree mapper instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;		/* PLOB header information */
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oSelf;		/* ObjId of object itself */
  OBJID		oHeap;		/* The heap the mapper works on */
  OBJID		oBTree;		/* The btree the mapper works on */
  OBJID		oKeyLower;	/* Lower interval key to map */
  OBJID		onTypeTagKeyLower;/* Type tag of lower key to map */
  OBJID		onCompareLower;	/* Comparision for lower key: >, >= */
  OBJID		oKeyUpper;	/* Upper interval key to map */
  OBJID		onTypeTagKeyUpper;/* Type tag of upper key to map */
  OBJID		onCompareUpper;	/* Comparision for upper key: >, >= */
  OBJID		onIncrement;	/* The direction of traversal, -1 or 1 */
  OBJID		onTimeStamp;	/* Time stamp of associated btree */
  OBJID		oBTreePage;	/* Current page the mapper works on */
  OBJID		onIndex;	/* Current index the mapper works on */
  OBJID		oKey;		/* Current key the mapper works on */
  OBJID		oData;		/* Current data the mapper works on */
  /* --- Persistent values: ---------------------------------------------- */
  time_t	timeMapper;	/* Time the mapper was started */
  LPVOID	pValueKeyLower;	/* Pointer to transient state of oKeyLower */
  LPVOID	pValueKeyUpper;	/* Pointer to transient state of oKeyUpper */
}	BTREEMAPPER, * LPBTREEMAPPER;

#define		make_btree(testmode)	fnMakeBTree(testmode)
#define		btreep(oSelf)		(typetagof(oSelf)==eshBTreeTag)
#define		btreepagep(oSelf)	(typetagof(oSelf)==eshBTreePageTag)

void		fnInitCommonBTreeModule		( void );
void		fnInitializeBTreeModule		( void );
void		fnDeinitializeBTreeModule	( void );
void		fnDeinitCommonBTreeModule	( void );

OBJID		fnMakeBTree		( COMPARETAG	nTestMode );

#endif	/* defined(C2C) */

DefineConstant ( eshBTreeTag, "+btree-type-tag+", hex ( 98 ),
		 "Type tag for plob objects of type BTREE." );

DefineConstant ( eshBTreePageTag, "+btree-page-tag+", hex ( A0 ),
		 "Type tag for plob objects of type BTREEPAGE." );

DefineConstant ( eshBTreeMapperTag, "+btree-mapper-tag+", hex ( D8 ),
		 "Type tag for plob objects of type BTREEMAPPER." );

DefineConstant ( eshGetTestMode, "+btree-get-test-mode+", -2,
		 "A tag for fnClientBtreeTestMode indicating to return the\
 current mode." );

/* Possible results from various btree functions. Additionally values of
   type SHLOCK are returned for all btree functions: */
BeginEnum ( BTREERESULT )
  enumerator ( btreeNotFound, "+btree-not-found+", 0,
	       "The key has not been found in the btree." )
  and
  enumerator ( btreeFound, "+btree-found+", 1,
	       "The key has been found in the btree." )
  and
  enumerator ( btreeInserted, "+btree-inserted+", 2,
	       "The key and data have been inserted into the btree." )
  and
  enumerator ( btreeUpdated, "+btree-updated+", 3,
	       "The key was found in the btree; updated data field." )
  and
  enumerator ( btreeDeleted, "+btree-deleted+", 4,
	       "The key was deleted from the btree." )
EndEnum ( BTREERESULT );

BeginEnum ( SHBTREEIDX )
  enumerator ( eshBTreeIdxSelf, "+btree-location-self+", 0,
	       "Index of plob btree self field." )
  and
  enumerator ( eshBTreeIdxCompare, "+btree-location-compare+", 1,
	       "Index of plob btree compare field." )
  and
  enumerator ( eshBTreeIdxCount, "+btree-location-count+", 2,
	       "Index of plob btree field with number of items." )
  and
  enumerator ( eshBTreeIdxCached, "+btree-location-cached+", 3,
	       "Index of plob btree field with flag if the btree\
 should be cached." )
  and
  enumerator ( eshBTreeIdxTimeStamp,
	       "+btree-location-duplicate-keys-p+", 4,
	       "Index of plob btree field with flag if duplicate\
 keys are allowed." )
EndEnum ( SHBTREEIDX );

DefineFunction ( BTREERESULT,
		 fnClientBtreeClear, "c-sh-btree-clear",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) );

DefineFunction ( FIXNUM,
		 fnClientBtreeCount, "c-sh-btree-count",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) );

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeDelete, "c-sh-btree-delete",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( FIXNUM, value_in, nValueKey )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKey ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
		 fnClientBtreeDelete, "c-sh-btree-delete",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( FIXNUM, value_in, nValueKey )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKey ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeDeleteByFloat, "c-sh-btree-delete-by-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( SINGLE_FLOAT, value_in, fKey ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
		 fnClientBtreeDeleteByFloat, "c-sh-btree-delete-by-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKey ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeDeleteByDouble, "c-sh-btree-delete-by-double",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKey ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
		 fnClientBtreeDeleteByDouble, "c-sh-btree-delete-by-double",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKey ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeDeleteByString, "c-sh-btree-delete-by-string",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( CONST_STRING, vector_in, szKey ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
		 fnClientBtreeDeleteByString, "c-sh-btree-delete-by-string",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( CONST_STRING, vector_in, szKey ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeInsert, "c-sh-btree-insert",
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
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
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
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeInsertByFloat, "c-sh-btree-insert-by-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( SINGLE_FLOAT, value_in, fKey )
		   and
		   argument ( FIXNUM, value_in, nValueData )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
		 fnClientBtreeInsertByFloat, "c-sh-btree-insert-by-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKey )
		   and
		   argument ( FIXNUM, value_in, nValueData )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeInsertByDouble, "c-sh-btree-insert-by-double",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKey )
		   and
		   argument ( FIXNUM, value_in, nValueData )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
		 fnClientBtreeInsertByDouble, "c-sh-btree-insert-by-double",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKey )
		   and
		   argument ( FIXNUM, value_in, nValueData )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeInsertByString, "c-sh-btree-insert-by-string",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( CONST_STRING, vector_in, szKey )
		   and
		   argument ( FIXNUM, value_in, nValueData )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
		 fnClientBtreeInsertByString, "c-sh-btree-insert-by-string",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( CONST_STRING, vector_in, szKey )
		   and
		   argument ( FIXNUM, value_in, nValueData )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapSearch, "c-sh-btree-map-search",
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
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
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
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapSearchByFloat,
		 "c-sh-btree-map-search-by-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( SINGLE_FLOAT, value_in, fKeyLower )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		   and
		   argument ( COMPARETAG, value_in, eCompareLower )
		   and
		   argument ( SINGLE_FLOAT, value_in, fKeyUpper )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		   and
		   argument ( COMPARETAG, value_in, eCompareUpper )
		   and
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
/* 1996/10/29 HK: The following declaration of
   SH_btree_map_by_float should look like:
DefineFunction ( FIXNUM,
		 fnClientBtreemapSearchByFloat,
		 "c-sh-btree-map-search-by-float",
		 ( argument ( SHORTOBJID,
			      value_out, lpoShortObjIdMapper )
		   and
		   <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
DefineFunction ( voidResult,
		 fnClientBtreemapSearchByFloat,
		 "c-sh-btree-map-search-by-float",
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
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapSearchByDouble,
		 "c-sh-btree-map-search-by-double",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKeyLower )
		   and
		   argument ( COMPARETAG, value_in, eCompareLower )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKeyUpper )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKeyUpper )
		   and
		   argument ( COMPARETAG, value_in, eCompareUpper )
		   and
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
/* 1996/10/29 HK: The following declaration of
   SH_btree_map_by_double should look like:
DefineFunction ( FIXNUM,
		 fnClientBtreemapSearchByDouble,
		 "c-sh-btree-map-search-by-double",
		 ( argument ( SHORTOBJID,
			      value_out, lpoShortObjIdMapper )
		   and
		   <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
DefineFunction ( voidResult,
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
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapSearchByString,
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
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
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
		   argument ( BOOL, value_in, bDescending ) ) );
#endif	/* ! RPC */

BeginEnum ( SEEK )
  enumerator ( seekSet, "+seek-set+", 0,
	       "Seek position relative to start position." )
  and
  enumerator ( seekCur, "+seek-current+", 1,
	       "Seek position relative to current position." )
  and
  enumerator ( seekEnd, "+seek-end+", 2,
	       "Seek position relative to end position." )
EndEnum ( SEEK );

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapSeek, "c-sh-btree-map-seek",
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
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
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
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapSeekSet, "c-sh-btree-map-seek-set",
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
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
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
		   argument ( SHTYPETAG, value_in, nTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapFirst, "c-sh-btree-map-first",
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapFirstByFloat, "c-sh-btree-map-first-by-float",
		 ( argument ( SHORTOBJID, value_out, lpoShortObjIdMapper )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( SINGLE_FLOAT, value_in, fKeyLower )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
		   and
		   argument ( COMPARETAG, value_in, eCompareLower )
		   and
		   argument ( SINGLE_FLOAT, value_in, fKeyUpper )
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
/* 1996/10/29 HK: The following declaration of
   fnClientBtreemapFirstByFloat should look like:
DefineFunction ( FIXNUM,
		 fnClientBtreemapFirstByFloat, "c-sh-btree-map-first-by-float",
		 ( argument ( SHORTOBJID,
			      value_out, lpoShortObjIdMapper )
		   and
		   <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
DefineFunction ( voidResult,
		 fnClientBtreemapFirstByFloat,
		 "c-sh-btree-map-first-by-float",
		 ( argument ( FIXNUM, value_out, pnReturnValue )
		   and
		   argument ( SHORTOBJID, value_out, lpoShortObjIdMapper )
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapFirstByDouble,
		 "c-sh-btree-map-first-by-double",
		 ( argument ( SHORTOBJID, value_out, lpoShortObjIdMapper )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fKeyLower )
		   and
		   argument ( COMPARETAG, value_in, eCompareLower )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKeyLower )
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
/* 1996/10/29 HK: The following declaration of
   fnClientBtreemapFirstByDouble should look like:
DefineFunction ( FIXNUM,
		 fnClientBtreemapFirstByDouble,
		 "c-sh-btree-map-first-by-double",
		 ( argument ( SHORTOBJID,
			      value_out, lpoShortObjIdMapper )
		   and
		   <rest see below> ) );
but was changed to the one found below because of a bug in the LispWorks
foreign function interface (the return value wasn't passed back correctly
from C to LispWorks): */
DefineFunction ( voidResult,
		 fnClientBtreemapFirstByDouble,
		 "c-sh-btree-map-first-by-double",
		 ( argument ( FIXNUM, value_out, pnReturnValue )
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerBtreemapFirstByString,
		 "c-sh-btree-map-first-by-string",
		 ( argument ( SHORTOBJID, value_out, lpoShortObjIdMapper )
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientBtreemapFirstByString,
		 "c-sh-btree-map-first-by-string",
		 ( argument ( SHORTOBJID, value_out, lpoShortObjIdMapper )
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
			      vector_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

DefineFunction ( FIXNUM,
		 fnClientBtreemapNext, "c-sh-btree-map-next",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdHeap )
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
			      vector_out, pnTypeTagData ) ) );

DefineFunction ( SHORTOBJID,
		 fnClientBtreemapLast, "c-sh-btree-map-last",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );

DefineFunction ( FIXNUM,
		 fnClientBtreePrint, "c-sh-btree-print",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( NUMERICSTDSTREAM, value_in, nStdStream ) ) );

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeSearch, "c-sh-btree-search",
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
		   argument ( SHTYPETAG, value_out, pnTypeTagKey )
		   and
		   argument ( FIXNUM, value_out, pnValueData )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
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
		   argument ( SHTYPETAG, value_out, pnTypeTagKey )
		   and
		   argument ( FIXNUM, value_out, pnValueData )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeSearchByFloat, "c-sh-btree-search-by-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( SINGLE_FLOAT, value_in, fKey )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagKey )
		   and
		   argument ( FIXNUM, value_out, pnValueKey )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTagKey )
		   and
		   argument ( FIXNUM, value_out, pnValueData )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
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
		   argument ( SHTYPETAG, value_out, pnTypeTagKey )
		   and
		   argument ( FIXNUM, value_out, pnValueData )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeSearchByDouble, "c-sh-btree-search-by-double",
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
		   argument ( SHTYPETAG, value_out, pnTypeTagKey )
		   and
		   argument ( FIXNUM, value_out, pnValueData )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
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
		   argument ( SHTYPETAG, value_out, pnTypeTagKey )
		   and
		   argument ( FIXNUM, value_out, pnValueData )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BTREERESULT,
		 fnServerBtreeSearchByString, "c-sh-btree-search-by-string",
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
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BTREERESULT,
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
		   argument ( SHTYPETAG, value_out, pnTypeTagData ) ) );
#endif	/* ! RPC */

DefineFunction ( SHORTOBJID,
		 fnClientBtreeRoot, "c-sh-btree-root",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) );

DefineFunction ( FIXNUM,
		 fnClientBtreeSize, "c-sh-btree-size",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) );

DefineFunction ( COMPARETAG,
		 fnClientBtreeTestMode, "c-sh-btree-test-mode",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( COMPARETAG, value_in, nNewTestMode ) ) );

DefineFunction ( FIXNUM,
		 fnClientBtreePageSize, "c-sh-btree-set-page-size",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		   and
		   argument ( FIXNUM, value_in, nNewPageSize ) ) );

DefineFunction ( OBJID,
		 fnClientBtreepageParent, "c-sh-btreepage-parent",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTreePage ) ) );

DefineFunction ( FIXNUM,
		 fnClientBtreepageCount, "c-sh-btreepage-count",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTreePage ) ) );

DefineFunction ( FIXNUM,
		 fnClientBtreepageGetSize, "c-sh-btreepage-size",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTreePage ) ) );

DefineFunction ( FIXNUM,
		 fnClientBtreepageItem, "c-sh-btreepage-item",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdBTreePage )
		   and
		   argument ( FIXNUM, value_in, nIndex )
		   and
		   argument ( FIXNUM, value_in, nItems )
		   and	/* ouput arguments: */
		   argument ( VECTOR ( int, nItems ), vector_out,
			      pnValueKey )
		   and
		   argument ( VECTOR ( u_int, nItems ), vector_out,
			      pnTypeTagKey )
		   and
		   argument ( VECTOR ( int, nItems ), vector_out,
			      pnValueData )
		   and
		   argument ( VECTOR ( u_int, nItems ), vector_out,
			      pnTypeTagData )
		   and
		   argument ( VECTOR ( u_int, nItems ), vector_out,
			      poNext ) ) );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
