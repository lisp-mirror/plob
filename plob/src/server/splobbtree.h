/* -------------------------------------------------------------------------
| Module	splobbtree.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
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

#include	"plobbtree.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */

#define	BTreeSearchByObjId( oHeap, oBTree, oKey, lpoKey, lpoData )	\
(__oBTreeSearchByObjId__=(oBTree),					\
 __oBTreeSearchByObjIdKey__=(oKey),					\
 __lpoBTreeSearchByObjIdKey__=(LPOBJID)(lpoKey),			\
 __lpoBTreeSearchByObjIdData__=(LPOBJID)(lpoData),			\
 (oBTreeSearchByObjIdCache!=NULLOBJID&&					\
  __oBTreeSearchByObjId__==oBTreeSearchByObjIdCache&&			\
  __oBTreeSearchByObjIdKey__==oBTreeSearchByObjIdKeyCache)?		\
 (((__lpoBTreeSearchByObjIdKey__)?					\
   *__lpoBTreeSearchByObjIdKey__=oBTreeSearchByObjIdKeyFoundCache:	\
   NULLOBJID),								\
  ((__lpoBTreeSearchByObjIdData__)?					\
   *__lpoBTreeSearchByObjIdData__=oBTreeSearchByObjIdDataCache:		\
   NULLOBJID),								\
  nBTreeSearchByObjIdResultCache):					\
 fnBTreeSearchByObjId(oHeap,__oBTreeSearchByObjId__,			\
		      __oBTreeSearchByObjIdKey__,			\
		      __lpoBTreeSearchByObjIdKey__,			\
		      __lpoBTreeSearchByObjIdData__))

#define	BTreeCount( oSelf )						\
(__oBTreeCount__=(oSelf),						\
 (oBTreeCountCache!=NULLOBJID&&__oBTreeCount__==oBTreeCountCache)?	\
 nBTreeCountCache:fnBTreeCount(__oBTreeCount__))

#define	BTreePageCount( oSelf )						\
(__oBTreePageCount__=(oSelf),						\
 (oBTreePageCountCache!=NULLOBJID&&					\
  __oBTreePageCount__==oBTreePageCountCache)?				\
 nBTreePageCountCache:fnBTreePageCount(__oBTreePageCount__))

/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */
extern OBJID		__oBTreeSearchByObjId__		/* = NULLOBJID */;
extern OBJID		__oBTreeSearchByObjIdKey__	/* = NULLOBJID */;
extern LPOBJID		__lpoBTreeSearchByObjIdData__	/* = NULL */;
extern LPOBJID		__lpoBTreeSearchByObjIdKey__	/* = NULL */;
extern OBJID		oBTreeSearchByObjIdCache	/* = NULLOBJID */;
extern OBJID		oBTreeSearchByObjIdKeyCache	/* = NULLOBJID */;
extern OBJID		oBTreeSearchByObjIdKeyFoundCache/* = NULLOBJID */;
extern OBJID		oBTreeSearchByObjIdDataCache	/* = NULLOBJID */;
extern BTREERESULT	nBTreeSearchByObjIdResultCache	/* = btreeNotFound */;

extern OBJID		__oBTreeCount__			/* = NULLOBJID */;
extern OBJID		oBTreeCountCache		/* = NULLOBJID */;
extern int		nBTreeCountCache		/* = 0 */;

extern OBJID		__oBTreePageCount__		/* = NULLOBJID */;
extern OBJID		oBTreePageCountCache		/* = NULLOBJID */;
extern int		nBTreePageCountCache		/* = 0 */;

/* -------------------------------------------------------------------------
| Functions on BTrees
 ------------------------------------------------------------------------- */
BTREERESULT DLLEXPORT	fnBTreeClear	( OBJID		oHeap,
					  OBJID		oBTree );

BTREERESULT DLLEXPORT	fnBTreeDelete	( OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKey,
					  SHTYPETAG	nTypeTagKey );

BTREERESULT DLLEXPORT	fnBTreeDeleteByObjId	( OBJID		oHeap,
						  OBJID		oBTree,
						  OBJID		oKey );

BTREERESULT DLLEXPORT	fnBTreeInsert	( OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKey,
					  SHTYPETAG	nTypeTagKey,
					  OBJID		oData );

BTREERESULT DLLEXPORT	fnBTreeInsertByObjId	( OBJID		oHeap,
						  OBJID		oBTree,
						  OBJID		oKey,
						  OBJID		oData );

/* The enumeration function should return with TRUE if the enumeration
   should be continued: */
typedef BOOL	( FAR * LPFNMAPBTREE ) ( LPVOID		lpUserData,
					 OBJID		oBTree,
					 OBJID		oKey,
					 OBJID		oData,
					 OBJID		oBTreePage,
					 int		nIndex );

int DLLEXPORT	fnBTreeMap		( OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKeyLower,
					  SHTYPETAG	nTypeTagKeyLower,
					  COMPARETAG	eCompareLower,
					  LPCVOID	pValueKeyUpper,
					  SHTYPETAG	nTypeTagKeyUpper,
					  COMPARETAG	eCompareUpper,
					  BOOL		bDescending,
					  LPFNMAPBTREE	lpfnMapItem,
					  LPVOID	lpUserData );

int DLLEXPORT	fnBTreeMapAll		( OBJID		oHeap,
					  OBJID		oBTree,
					  LPFNMAPBTREE	pfnMapItem,
					  LPVOID	pUserData );

int DLLEXPORT	fnBTreeMapByObjId	( OBJID		oHeap,
					  OBJID		oBTree,
					  OBJID		oKeyLower,
					  COMPARETAG	eCompareLower,
					  OBJID		oKeyUpper,
					  COMPARETAG	eCompareUpper,
					  BOOL		bDescending,
					  LPFNMAPBTREE	lpfnMapItem,
					  LPVOID	lpUserData );

FIXNUM DLLEXPORT fnBTreeMapFirstByObjId	( LPOBJID	lpoMapper,
					  OBJID		oHeap,
					  OBJID		oBTree,
					  OBJID		oKeyLower,
					  COMPARETAG	eCompareLower,
					  OBJID		oKeyUpper,
					  COMPARETAG	eCompareUpper,
					  BOOL		bDescending,
					  FIXNUM	nMap,
					  LPOBJID	lpoKey,
					  LPOBJID	lpoData );

BTREERESULT DLLEXPORT fnBTreeSearch	( OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKey,
					  SHTYPETAG	nTypeTagKey,
					  LPOBJID	lpoKey,
					  LPOBJID	lpoData );

BTREERESULT DLLEXPORT fnBTreeSearchByObjId	( OBJID		oHeap,
						  OBJID		oBTree,
						  OBJID		oKey,
						  LPOBJID	lpoKey,
						  LPOBJID	lpoData );

COMPARETAG DLLEXPORT fnBTreeTestMode	( OBJID		oBTree,
					  COMPARETAG	nNewTestMode );

OBJID DLLEXPORT	fnBTreeRoot		( OBJID		oBTree );

int DLLEXPORT	fnBTreeCount		( OBJID		oBTree );

int DLLEXPORT	fnBTreeSize		( OBJID		oBTree );

int DLLEXPORT	fnBTreeSetPageSize	( OBJID		oBTree,
					  int		nNewPageSize );

/* -------------------------------------------------------------------------
| Functions on BTree mappers
 ------------------------------------------------------------------------- */
FIXNUM DLLEXPORT fnBTreeMapSearch	( OBJID		oMapper,
					  OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKeyLower,
					  SHTYPETAG	nTypeTagKeyLower,
					  COMPARETAG	eCompareLower,
					  LPCVOID	pValueKeyUpper,
					  SHTYPETAG	nTypeTagKeyUpper,
					  COMPARETAG	eCompareUpper,
					  BOOL		bDescending );

FIXNUM DLLEXPORT fnBTreeMapSeek		( OBJID		oMapper,
					  FIXNUM	nIncrement,
					  SEEK		eOrigin,
					  LPOBJID	poKey,
					  LPOBJID	poData );

FIXNUM DLLEXPORT fnBTreeMapFirst	( LPOBJID	lpoMapper,
					  OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKeyLower,
					  SHTYPETAG	nTypeTagKeyLower,
					  COMPARETAG	eCompareLower,
					  LPCVOID	pValueKeyUpper,
					  SHTYPETAG	nTypeTagKeyUpper,
					  COMPARETAG	eCompareUpper,
					  BOOL		bDescending,
					  FIXNUM	nMap,
					  LPOBJID	lpoKey,
					  LPOBJID	lpoData );

FIXNUM DLLEXPORT fnBTreeMapNext		( OBJID		oMapper,
					  FIXNUM	nMap,
					  LPOBJID	lpoKey,
					  LPOBJID	lpoData );

OBJID DLLEXPORT fnBTreeMapLast		( OBJID		oMapper );

/* -------------------------------------------------------------------------
| Functions on BTreePages
 ------------------------------------------------------------------------- */

OBJID DLLEXPORT	fnBTreePageParent	( OBJID		oBTreePage );

int DLLEXPORT	fnBTreePageCount	( OBJID		oBTreePage );

int DLLEXPORT	fnBTreePageSize		( OBJID		oBTreePage );

LPBTREEITEM DLLEXPORT fnBTreePageItem	( OBJID		oBTreePage,
					  int		nIndex );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
