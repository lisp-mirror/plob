/* -------------------------------------------------------------------------
| Module	splobbtree.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94 Derived from c-plob.c
| Description	PLOB BTrees. BTree algorithms taken from:
|		Niklaus Wirth:
|		Algorithmen und Datenstrukturen
|		p. 283
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

#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobsequ.h"
#include	"splobtype.h"
#include	"splobnumber.h"
#include	"sploblock.h"
#include	"splobbtree.h"
#include	"splobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
#if 0
#define	LOGGING
#endif

/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */
OBJID		__oBTreeSearchByObjId__		= NULLOBJID;
OBJID		__oBTreeSearchByObjIdKey__	= NULLOBJID;
LPOBJID		__lpoBTreeSearchByObjIdData__	= (LPOBJID) NULL;
LPOBJID		__lpoBTreeSearchByObjIdKey__	= (LPOBJID) NULL;
OBJID		oBTreeSearchByObjIdCache	= NULLOBJID;
OBJID		oBTreeSearchByObjIdKeyCache	= NULLOBJID;
OBJID		oBTreeSearchByObjIdKeyFoundCache= NULLOBJID;
OBJID		oBTreeSearchByObjIdDataCache	= NULLOBJID;
BTREERESULT	nBTreeSearchByObjIdResultCache	= btreeNotFound;

OBJID		__oBTreeCount__			= NULLOBJID;
OBJID		oBTreeCountCache		= NULLOBJID;
int		nBTreeCountCache		= 0;

OBJID		__oBTreePageCount__		= NULLOBJID;
OBJID		oBTreePageCountCache		= NULLOBJID;
int		nBTreePageCountCache		= 0;

/* -------------------------------------------------------------------------
| Static constants
 ------------------------------------------------------------------------- */
static const char	szIllegalCompare []	=
  "Invalid %s limit compare criterion %s (%d);\n"
  "       allowed are %s (%d) and %s (%d).";
static const char	szLower []	= "lower";
static const char	szUpper []	= "upper";

/* -------------------------------------------------------------------------
| LISP callback
 ------------------------------------------------------------------------- */
static BOOL	fnPLOBmapCallback		( LPVOID lpUserData,
						  OBJID oBTree,
						  OBJID oKey, OBJID oData,
						  OBJID oBTreePage,
						  int nIndex );

/* define TESTING for making the default btree page size very small: */
#if 0
#define	TESTING
#endif

/* ----------------------------------------------------------------------- */
enum {
  nRawBTreePageSize		= 8192,	/* Raw BTree page size in bytes */
#ifdef	TESTING
  nBtreeDefaultPageSizeDiv2	= 2,
#else
  nBtreeDefaultPageSizeDiv2	=
    ( nRawBTreePageSize - 6 /* number of slots in BTREEPAGE */ *
      sizeof ( OBJID ) - sizeof ( PLOBHEADER ) ) /
      ( 2 * sizeof ( BTREEITEM ) ),
#endif
  nBtreeDefaultPageSize		= nBtreeDefaultPageSizeDiv2 * 2,

  nBTreeMinPageSize		= 4,
  nBTreeMaxPageSize		= nBtreeDefaultPageSize * 4,
};

typedef enum {
  mapmodeContinue,
  mapmodeRestart,
  mapmodeFinished
}	MAPMODE;

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */
static BOOL		mfnInitBTree		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnPrintBTree		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static unsigned int	mfnBTreeCount		( OBJID oSelf );
static BOOL		mfnBTreeDestroy		( OBJID oSelf,
						  BOOL bKill );
static BOOL		mfnInitBTreePage	( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnPrintBTreePage	( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static unsigned int	mfnBTreePageCount	( OBJID oSelf );
static BOOL		mfnInitBTreeMapper	( OBJID oSelf,
						  LPOBJID pSelf,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnPrintBTreeMapper	( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static BOOL		mfnBTreeMapperDestroy	( OBJID oSelf,
						  BOOL bKill );
static BOOL		fnCheck			( LPVOID lpUserData,
						  OBJID oBTree,
						  OBJID oKey, OBJID oData,
						  OBJID oBTreePage,
						  int nIndex );
static BOOL		fnReInsert		( LPVOID lpUserData,
						  OBJID oBTree,
						  OBJID oKey, OBJID oData,
						  OBJID oBTreePage,
						  int nIndex );
static void		fnReleaseAllPages	( OBJID oHeap,
						  LPBTREEPAGE lpPage );
static void		fnReOrganize		( OBJID oHeap,
						  LPPLOBBTREE lpBTree );
static LPPLOBBTREE	fnGetBTree		( OBJID oHeap,
						  OBJID oObjId,
						  SHLOCK * pnLock );
static SHLOCK		fnLockBTree		( OBJID oHeap,
						  LPPLOBBTREE lpBTree );
static OBJID		fnCreateBTreePage	( LPPLOBBTREE lpBTree );
static LPBTREEPAGE	fnGetBTreePage		( OBJID oObjId );
static SHLOCK		fnLockBTreePage		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage );
static BOOL		fnSetPageSize		( LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  int nNewSize,
						  LPOBJID lpoUnderflow );
static void		fnSetPageNext		( LPBTREEPAGE lpPage,
						  int nIndex,
						  OBJID oNext );
static void		fnSetPageItem		( LPBTREEPAGE lpPageDest,
						  int nIndexDest,
						  LPBTREEPAGE lpPageSrce,
						  int nIndexSrce );
static LPBTREEMAPPER	fnGetBTreeMapper	( OBJID oHeap,
						  OBJID oObjId,
						  SHLOCK * pnLock );
static SHLOCK		fnLockBTreeMapper	( OBJID oHeap,
						  LPBTREEMAPPER lpMapper );
static COMPARETAG	fnCompare		( COMPARETAG nCompare,
						  LPCVOID pValue1,
						  SHTYPETAG nTypeTag1,
						  OBJID oObj2 );
static int		fnKeyCmp		( LPPLOBBTREE lpBTree,
						  LPCVOID pValueKey1,
						  SHTYPETAG nTypeTagKey1,
						  OBJID oKey2 );

static int		fnSearchKey		( LPPLOBBTREE	lpBTree,
						  LPBTREEPAGE	lpPage,
						  LPCVOID	pValueKeyLower,
						  SHTYPETAG
						  nTypeTagKeyLower,
						  LPINT		lpnIndexMiddle,
						  LPINT		lpnCompare );
/* PROCEDURE insert: */
static SHLOCK		fnInsert		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  int nIndex,
						  LPBOOL lpbMove,
						  LPBTREEITEM lpInsert,
						  LPBTREEITEM lpMove );
/* PROCEDURE search: */
static BTREERESULT	fnSearch		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPCVOID pValueKey,
						  SHTYPETAG nTypeTagKey,
						  LPOBJID lpoKeyFound,
						  LPOBJID lpoDataFound,
						  LPOBJID lpoDataInsert,
						  LPBOOL lpbMove,
						  LPBTREEITEM lpMove );
/* PROCEDURE underflow: */
static SHLOCK		fnUnderflow		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPBTREEPAGE lpPagePrev,
						  int nIndex,
						  LPOBJID lpoUnderflow,
						  LPBOOL lpbUnderflow );
/* PROCEDURE del: */
static SHLOCK		fnDel			( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPBTREEPAGE lpPagePrev,
						  int nIndex,
						  LPOBJID lpoUnderflow,
						  LPBOOL lpbUnderflow );
/* PROCEDURE delete: */
static BTREERESULT	fnDelete		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPCVOID pValueKey,
						  SHTYPETAG nTypeTagKey,
						  LPOBJID lpoUnderflow,
						  LPBOOL lpbUnderflow );
/* PROCEDURE printtree: */
static int		fnDumpBTree		( OBJID oHeap,
						  FILE FAR * lpStream,
						  LPBTREEPAGE lpPage,
						  int nIndent,
						  LPINT lpnLine );

static MAPMODE		fnMap			( OBJID		oHeap,
						  LPPLOBBTREE	pBTree,
						  LPBTREEPAGE	pPage,
						  LPINT		pnKeys,
						  LPOBJID	poKeyLower,
						  LPSHTYPETAG
						  pnTypeTagKeyLower,
						  LPCVOID	pValueKeyLower,
						  COMPARETAG	eCompareLower,
						  LPOBJID	poKeyUpper,
						  LPSHTYPETAG
						  pnTypeTagKeyUpper,
						  LPCVOID	pValueKeyUpper,
						  COMPARETAG	eCompareUpper,
						  BOOL		bDescending,
						  LPFNMAPBTREE	pfnMapItem,
						  LPVOID	pUserData );

static BOOL		fnEchoBTreePage		( LPVOID lpUserData,
						  OBJID oBTree,
						  OBJID oKey, OBJID oData,
						  OBJID oBTreePage,
						  int nIndex );

static BOOL		fnSearchBTreePage	( OBJID		oHeap,
						  LPPLOBBTREE	lpBTree,
						  LPCVOID	pValueKeyLower,
						  SHTYPETAG nTypeTagKeyLower,
						  COMPARETAG	eCompareLower,
						  LPCVOID	pValueKeyUpper,
						  SHTYPETAG nTypeTagKeyUpper,
						  COMPARETAG	eCompareUpper,
						  BOOL		bDescending,
						  LPBTREEMAPPER	lpMapper );

static void	fnBTreeMapStoreKey	( LPCVOID	pValueKey,
					  SHTYPETAG	pnTypeTagKey,
					  LPOBJID	poKey,
					  LPOBJID	ponTypeTagKey,
					  LPVOID	*ppBuffer );
static LPCVOID	fnBtreeMapGetKeyPtr	( LPOBJID	poKey,
					  OBJID		onTypeTagKey,
					  LPCVOID	pBuffer );
static void	fnBTreeMapUnstoreKey	( LPOBJID	poKey,
					  LPOBJID	ponTypeTagKey,
					  LPVOID	*ppBuffer );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
static time_t	timeGlobalStart		= 0;

void			fnInitializeBTreeModule		( void )
{
  PROCEDURE	( fnInitializeBTreeModule );

  time ( &timeGlobalStart );

  /* Make sure that the struct's components offsets match the sh-vector
     indices. If one of the following ASSERTs fails, the eshBTreeIdx...-
     constants have been modified without reflecting these modifications
     in the corresponding struct PLOBBTREE (or vice versa): */
  ASSERT ( Offset_matches_Index ( PLOBBTREE, oSelf,
				  Cooked2RawIndex ( eshBTreeIdxSelf ) ) );
  ASSERT ( Offset_matches_Index ( PLOBBTREE, oCompare,
				  Cooked2RawIndex ( eshBTreeIdxCompare ) ) );
  ASSERT ( Offset_matches_Index ( PLOBBTREE, onCount,
				  Cooked2RawIndex ( eshBTreeIdxCount ) ) );
  ASSERT ( Offset_matches_Index ( PLOBBTREE, obCached,
				  Cooked2RawIndex ( eshBTreeIdxCached ) ) );
  ASSERT ( Offset_matches_Index ( PLOBBTREE, onTimeStamp,
				  Cooked2RawIndex ( eshBTreeIdxTimeStamp ) ) );

  RegisterMethod ( eshBTreeTag, gfnInitializeInstance, mfnInitBTree );
  RegisterMethod ( eshBTreeTag, gfnPrintObjectDetails, mfnPrintBTree );
  RegisterMethod ( eshBTreeTag, gfnCount, mfnBTreeCount );
  RegisterMethod ( eshBTreeTag, gfnDestroy, mfnBTreeDestroy );

  RegisterMethod ( eshBTreePageTag, gfnInitializeInstance,
		   mfnInitBTreePage );
  RegisterMethod ( eshBTreePageTag, gfnPrintObjectDetails,
		   mfnPrintBTreePage );
  RegisterMethod ( eshBTreePageTag, gfnCount, mfnBTreePageCount );

  RegisterMethod ( eshBTreeMapperTag, gfnInitializeInstance,
		   mfnInitBTreeMapper );
  RegisterMethod ( eshBTreeMapperTag, gfnPrintObjectDetails,
		   mfnPrintBTreeMapper );
  RegisterMethod ( eshBTreeMapperTag, gfnDestroy,
		   mfnBTreeMapperDestroy );

  RETURN ( VOID );
} /* fnInitializeBTreeModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeBTreeModule	( void )
{
  PROCEDURE	( fnDeinitializeBTreeModule );

  RETURN ( VOID );
} /* fnDeinitializeBTreeModule */

/* -------------------------------------------------------------------------
| Initialization functions
 ------------------------------------------------------------------------- */
static BOOL		mfnInitBTree		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitBTree );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  ((LPPLOBBTREE)lpSHvector)->oSelf		= oObjId;
  ((LPPLOBBTREE)lpSHvector)->oCompare		= Fixnum2ObjId ( eshEqual );
  ((LPPLOBBTREE)lpSHvector)->onCount		= o0;
  ((LPPLOBBTREE)lpSHvector)->onTimeStamp	= o1;
  ((LPPLOBBTREE)lpSHvector)->onPages		= o0;
  ((LPPLOBBTREE)lpSHvector)->onGCcounter	=
    Fixnum2ObjId ( GetGCcounter () );
  ((LPPLOBBTREE)lpSHvector)->onPageSize		=
    Fixnum2ObjId ( nBtreeDefaultPageSize );
  RETURN ( (BOOL) TRUE );
} /* mfnInitBTree */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintBTree		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  LPCSTR	lpszCompare = (LPCSTR) NULL;
  char		szBuffer [ 80 ];
  int		nPageSize = 0, nPages = 0;

  PROCEDURE	( mfnPrintBTree );

  switch ( OBJID2FIXNUM ( ((LPPLOBBTREE)lpSHvector)->oCompare ) ) {
  case eshEq:
    lpszCompare	= "eq";
    break;
  case eshEql:
    lpszCompare	= "eql";
    break;
  case eshEqual:
    lpszCompare	= "equal";
    break;
  default:
    lpszCompare	= "???";
    break;
  }
  nPageSize	= OBJID2FIXNUM ( ((LPPLOBBTREE)lpSHvector)->onPageSize );
  nPages	= OBJID2FIXNUM ( ((LPPLOBBTREE)lpSHvector)->onPages );
  sprintf ( szBuffer, "%s %d/%d=%d*%d", lpszCompare,
	    OBJID2FIXNUM ( ((LPPLOBBTREE)lpSHvector)->onCount ),
	    nPages * nPageSize,
	    nPages, nPageSize );
  strncpy ( lpszBuffer, szBuffer, nBuffer );
  RETURN ( lpszBuffer );
} /* mfnPrintBTree */

/* ----------------------------------------------------------------------- */
static unsigned int	mfnBTreeCount		( OBJID oSelf )
{
  PROCEDURE	( mfnBTreeCount );

  RETURN ( fnBTreeCount ( oSelf ) );
} /* mfnBTreeCount */

/* ----------------------------------------------------------------------- */
static BOOL		mfnBTreeDestroy		( OBJID oSelf,
						  BOOL bKill )
{
  LPPLOBBTREE	lpBTree;

  PROCEDURE	( mfnBTreeDestroy );

  lpBTree	= fnGetBTree ( NULLOBJID, oSelf, (SHLOCK *) NULL );
  if ( ObjId2Fixnum ( lpBTree->onCount ) > 0 ) {
    CERROR (( "Ignore request to destroy the BTree object.",
	      "Request to destroy non-empty object %s.",
	      fnPrintObject ( oSelf, (LPSTR) NULL, 0 ) ));
    RETURN ( (BOOL) FALSE );
  }
  RETURN ( (BOOL) TRUE );
} /* mfnBTreeDestroy */

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitBTreePage	( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitBTreePage );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  ((LPBTREEPAGE)lpSHvector)->oSelf	= oObjId;
  ((LPBTREEPAGE)lpSHvector)->onCount	= o0;
  makunbound ( ((LPBTREEPAGE)lpSHvector)->oParentPage );
  makunbound ( ((LPBTREEPAGE)lpSHvector)->oParentIndex );
  RETURN ( (BOOL) TRUE );
} /* mfnInitBTreePage */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintBTreePage	( OBJID oSelf,
						  LPOBJID pSelf,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  int		i, j, n, nPageSize = 0;
  char		szKey [ 128 ], szBuffer [ 128 ];

  PROCEDURE	( mfnPrintBTreePage );

  n		= OBJID2FIXNUM ( ((LPBTREEPAGE)pSelf)->onCount );
  nPageSize	= fnBTreePageSize ( oSelf );
  if ( nPageSize <= 4 ) {
    /* 1996/10/21 HK: This method results in a rather long printed
       representation (too long for e.g. the LispWorks inspector): */
    for ( i = 0, j = 0; i < n && j < nBuffer; i++ ) {
      sprintf ( szBuffer, "%s%s",
		( i == 0 ) ? szEmpty : szSpace,
		PrintObject ( ((LPBTREEPAGE)pSelf)->Items [ i ].oKey,
			      szKey ) );
      strncpy ( & lpszBuffer [ j ], szBuffer, nBuffer - j );
      j	+= strlen ( szBuffer );
    }
  } else {
    /* 1996/10/21 HK: This method results in a rather short printed
       representation: */
    sprintf ( szBuffer, "%d/%d", n, nPageSize );
    strncpy ( lpszBuffer, szBuffer, nBuffer );
  }
  RETURN ( lpszBuffer );
} /* mfnPrintBTreePage */

/* ----------------------------------------------------------------------- */
static unsigned int	mfnBTreePageCount	( OBJID oSelf )
{
  PROCEDURE	( mfnBTreePageCount );

  RETURN ( fnBTreePageCount ( oSelf ) );
} /* mfnBTreePageCount */

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitBTreeMapper	( OBJID oSelf,
						  LPOBJID pSelf,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitBTreeMapper );

  mfnInitStandard ( oSelf, pSelf, lpClassInfo );

  ((LPBTREEMAPPER)pSelf)->oSelf			= oSelf;

  ((LPBTREEMAPPER)pSelf)->oKeyLower		= TypeTag2ObjId ( eshMinTag );
  ((LPBTREEMAPPER)pSelf)->onTypeTagKeyLower	= TypeTag2ObjId ( eshMinTag );
  ((LPBTREEMAPPER)pSelf)->onCompareLower	=
    Fixnum2ObjId ( eshGreaterEqual );

  ((LPBTREEMAPPER)pSelf)->oKeyUpper		= TypeTag2ObjId ( eshMaxTag );
  ((LPBTREEMAPPER)pSelf)->onTypeTagKeyUpper	= TypeTag2ObjId ( eshMaxTag );
  ((LPBTREEMAPPER)pSelf)->onCompareUpper	=
    Fixnum2ObjId ( eshLessEqual );

  ((LPBTREEMAPPER)pSelf)->onIncrement	= o1;
  ((LPBTREEMAPPER)pSelf)->onTimeStamp	= o1;

  RETURN ( (BOOL) TRUE );
} /* mfnInitBTreeMapper */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintBTreeMapper	( OBJID oObjId,
						  LPOBJID pSelf,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  static const char	szRefArrow []	= "->";

  PROCEDURE	( mfnPrintBTreeMapper );

  if ( boundp ( ((LPBTREEMAPPER)pSelf)->oBTree ) &&
       nBuffer > sizeof ( szRefArrow ) ) {
    strcpy ( lpszBuffer, szRefArrow );
    fnPrintObject ( ((LPBTREEMAPPER)pSelf)->oBTree,
		    & lpszBuffer [ sizeof ( szRefArrow ) - 1 ] ,
		    nBuffer - sizeof ( szRefArrow ) );
  }

  RETURN ( lpszBuffer );
} /* mfnPrintBTreeMapper */

/* ----------------------------------------------------------------------- */
static BOOL		mfnBTreeMapperDestroy	( OBJID oSelf,
						  BOOL bKill )
{
  LPBTREEMAPPER	pMapper;

  PROCEDURE	( mfnBTreeMapperDestroy );

  pMapper	= fnGetBTreeMapper ( NULLOBJID, oSelf, (SHLOCK *) NULL );
  ASSERT ( pMapper != NULL );

  fnBTreeMapUnstoreKey ( &pMapper->oKeyLower, &pMapper->onTypeTagKeyLower,
			 &pMapper->pValueKeyLower );
  fnBTreeMapUnstoreKey ( &pMapper->oKeyUpper, &pMapper->onTypeTagKeyUpper,
			 &pMapper->pValueKeyUpper );

  RETURN ( (BOOL) TRUE );
} /* mfnBTreeMapperDestroy */

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
typedef struct {
  int	nScanned;
  OBJID	oKeyLast;
  BOOL	bOutOfOrder;
}	CHECKBTREE, FAR * LPCHECKBTREE;

/* ----------------------------------------------------------------------- */
static BOOL		fnCheck			( LPVOID lpUserData,
						  OBJID oBTree,
						  OBJID oKey, OBJID oData,
						  OBJID oBTreePage,
						  int nIndex )
{
  PROCEDURE	( fnCheck );

  if ( ((LPCHECKBTREE)lpUserData)->nScanned > 0 ) {
    ((LPCHECKBTREE)lpUserData)->bOutOfOrder	= (BOOL)
      ( (int)
	((LPCHECKBTREE)lpUserData)->bOutOfOrder ||
	( ((LPCHECKBTREE)lpUserData)->oKeyLast > oKey ) );
  }
  ((LPCHECKBTREE)lpUserData)->oKeyLast	= oKey;
  ((LPCHECKBTREE)lpUserData)->nScanned++;
  RETURN ( (BOOL) ! ((LPCHECKBTREE)lpUserData)->bOutOfOrder );
} /* fnCheck */

/* ----------------------------------------------------------------------- */
static BOOL		fnReInsert		( LPVOID lpUserData,
						  OBJID oBTree,
						  OBJID oKey, OBJID oData,
						  OBJID oBTreePage,
						  int nIndex )
{
  PROCEDURE	( fnReInsert );

  fnBTreeInsertByObjId ( NULLOBJID, * (LPOBJID) lpUserData,
			 oKey, oData );
  RETURN ( (BOOL) TRUE );
} /* fnReInsert */

/* ----------------------------------------------------------------------- */
static void		fnReleaseAllPages	( OBJID oHeap,
						  LPBTREEPAGE lpPage )
{
  OBJID		oNext;
  int		i, n;

  PROCEDURE	( fnReleaseAllPages );

  if ( lpPage ) {
    oNext	= lpPage->oNext;
    fnReleaseAllPages  ( oHeap, fnGetBTreePage ( oNext ) );
    n		= ObjId2Fixnum ( lpPage->onCount );
    for ( i = 0; i < n; i++ ) {
      oNext	= lpPage->Items [ i ].oNext;
      fnReleaseAllPages  ( oHeap, fnGetBTreePage ( oNext ) );
    }
    fnUnlock ( oHeap, (SHLOCK)
	       ( (unsigned int) eshLockVectorRead |
		 (unsigned int) eshLockForce ),
	       lpPage->oSelf, -1, (SHLOCK *) NULL );
    fnUnlock ( oHeap, (SHLOCK)
	       ( (unsigned int) eshLockVectorWrite |
		 (unsigned int) eshLockForce ),
	       lpPage->oSelf, -1, (SHLOCK *) NULL );
    fnDestroyObject ( lpPage->oSelf, FALSE );
  }
  RETURN ( VOID );
} /* fnReleaseAllPages */

/* ----------------------------------------------------------------------- */
static void		fnReOrganize		( OBJID oHeap,
						  LPPLOBBTREE lpBTree )
{
  CHECKBTREE	Check;
  OBJID		oBTreeNew;
  LPPLOBBTREE	lpBTreeNew;

  PROCEDURE	( fnReOrganize );

  /* Map through the BTree and check if a reorganize is really
     necessary: */
  memset ( &Check, 0, sizeof ( Check ) );
  fnBTreeMapAll ( oHeap, lpBTree->oSelf, fnCheck, &Check );

  if ( Check.bOutOfOrder ) {
    /* Create a new BTree ... */
    oBTreeNew	=
      make_btree ( (COMPARETAG) ObjId2Fixnum ( lpBTree->oCompare ) );
    lpBTreeNew	= fnGetBTree ( NULLOBJID, oBTreeNew, (SHLOCK *) NULL );
    /* ... re-insert all items into the new BTree ... */
    fnBTreeMapAll ( oHeap, lpBTree->oSelf, fnReInsert, &oBTreeNew );
    if ( lpBTree->onCount != lpBTreeNew->onCount ) {
      CERROR (( "Use the reorganized BTree.",
	        "Reorganizing BTree %s after garbage collection failed;"
	        " expected to re-insert %d items, but re-inserted"
	        " %d items.",
	        fnPrintObject ( lpBTree->oSelf, (LPSTR) NULL, 0 ),
	        ObjId2Fixnum ( lpBTree->onCount ),
	        ObjId2Fixnum ( lpBTreeNew->onCount ) ));
    }
    /* ... free the 'old' BTree pages ... */
    fnReleaseAllPages ( oHeap, fnGetBTreePage ( lpBTree->oRoot ) );
    /* ... and move the 'new' BTree pages to the 'old' BTree: */
    *lpBTree	= *lpBTreeNew;
    makunbound ( lpBTreeNew->oRoot );
    lpBTreeNew->onCount	= o0;
    lpBTreeNew->onPages	= o0;
    fnDestroyObject ( oBTreeNew, FALSE );
  }
  RETURN ( VOID );
} /* fnReOrganize */

/* ----------------------------------------------------------------------- */
static LPPLOBBTREE	fnGetBTree		( OBJID oHeap,
						  OBJID oObjId,
						  SHLOCK * pnLock )
{
  SHLOCK	nLock	= (SHLOCK)
    ( (unsigned int) eshLockLevelNothing |
      (unsigned int) eshLockModeNothing );
  LPPLOBBTREE	lpBTree;

  PROCEDURE	( fnGetBTree );

  lpBTree	= (LPPLOBBTREE) SH_key_to_address ( oObjId );
  ASSERT ( lpBTree );
  if ( ! ASSERT_TYPE ( oObjId, lpBTree, eshBTreeTag ) ) {
    RETURN ( (LPPLOBBTREE) NULL );
  }
  /* 1998/07/03 HK: Hack: Assure that onTimeStamp contains always a
     fixnum. Needed since for old databases the slot might be
     unbound. */
  if ( ! fixnump ( lpBTree->onTimeStamp ) ) {
    lpBTree->onTimeStamp	= o1;
  }
  if ( boundp ( oHeap ) ) {
    nLock	=
      fnLockInsert ( oHeap, eshLockVectorRead, oObjId, -1, (PHPEEK) NULL );
  }
  if ( pnLock != NULL ) {
    *pnLock	= nLock;
  }
  if ( (int) nLock >= 0 ) {
    /* Check if the BTree should be re-organized because of a garbage
       collection done meanwhile: */
    if ( ObjId2Fixnum ( lpBTree->onGCcounter ) != GetGCcounter () &&
	 ObjId2Fixnum ( lpBTree->oCompare ) != eshEqual ) {
      lpBTree->onGCcounter	= Fixnum2ObjId ( GetGCcounter () );
      fnReOrganize ( oHeap, lpBTree );
    }
    RETURN ( lpBTree );
  }
  RETURN ( (LPPLOBBTREE) NULL );
} /* fnGetBTree */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnLockBTree		( OBJID oHeap,
						  LPPLOBBTREE lpBTree )
{
  SHLOCK	nLock	= (SHLOCK)
    ( (unsigned int) eshLockLevelNothing |
      (unsigned int) eshLockModeNothing );

  PROCEDURE	( fnLockBTree );

  if ( boundp ( oHeap ) ) {
    nLock	= fnLockInsert ( oHeap, eshLockVectorWrite,
				 lpBTree->oSelf, -1, (PHPEEK) NULL );
    if ( (int) nLock >= 0 ) {
      if ( ! gfnObjectStateChanged ( oHeap, lpBTree->oSelf ) )
	nLock	= eshLockFailed;
    }
  }
  RETURN ( nLock );
} /* fnLockBTree */

/* ----------------------------------------------------------------------- */
static OBJID		fnCreateBTreePage	( LPPLOBBTREE lpBTree )
{
  OBJID		oPageNew	= NULLOBJID;
  int		nPageSize	= 0;

  PROCEDURE	( fnCreateBTreePage );

  nPageSize	= ObjId2Fixnum ( lpBTree->onPageSize );
  oPageNew	=
    fnCreateObject ( (SHTYPETAG) eshBTreePageTag, nPageSize *
		     ( sizeof ( ((LPBTREEPAGE)NULL)->Items [ 0 ] ) /
		       nSizeOfPostoreWord ), NULLTYPETAG, 0 );

  RETURN ( oPageNew );
} /* fnCreateBTreePage */

/* ----------------------------------------------------------------------- */
static LPBTREEPAGE	fnGetBTreePage		( OBJID oObjId )
{
  LPBTREEPAGE	lpPage;

  PROCEDURE	( fnGetBTreePage );

  lpPage	= (LPBTREEPAGE) NULL;
  if ( boundp ( oObjId ) ) {
    lpPage	= (LPBTREEPAGE) SH_key_to_address ( oObjId );
    ASSERT ( lpPage != NULL );
    if ( ! ASSERT_TYPE ( oObjId, lpPage, eshBTreePageTag ) ) {
      RETURN ( (LPBTREEPAGE) NULL );
    }
  }
  RETURN ( lpPage );
} /* fnGetBTreePage */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnLockBTreePage		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage )
{
  SHLOCK	nLock	= (SHLOCK)
    ( (unsigned int) eshLockLevelNothing |
      (unsigned int) eshLockModeNothing );

  PROCEDURE	( fnLockBTreePage );

  if ( boundp ( oHeap ) ) {
    if ( lpBTree != NULL ) {
      nLock	= fnLockBTree ( oHeap, lpBTree );
    }
    if ( (int) nLock >= 0 ) {
      nLock	= fnLockInsert ( oHeap, eshLockVectorWrite,
				 lpPage->oSelf, -1, (PHPEEK) NULL );
    }
    if ( (int) nLock >= 0 ) {
      if ( ! gfnObjectStateChanged ( oHeap, lpPage->oSelf ) )
	nLock	= eshLockFailed;
    }
  }
  RETURN ( nLock );
} /* fnLockBTreePage */

/* ----------------------------------------------------------------------- */
static BOOL		fnSetPageSize		( LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  int nNewSize,
						  LPOBJID lpoUnderflow )
{
  int	i, nOldSize, nPageSize;
  BOOL	bUnderflow;

  PROCEDURE	( fnSetPageSize );
  ASSERT ( lpPage );
  nOldSize		= OBJID2FIXNUM ( lpPage->onCount );
  lpPage->onCount	= Fixnum2ObjId ( nNewSize );
  nPageSize		= OBJID2FIXNUM ( lpBTree->onPageSize );
  /* Check if there is a page underflow: */
  bUnderflow		= (BOOL) ( nNewSize < nPageSize / 2 );
  if ( nNewSize < nOldSize ) {
    for ( i = nNewSize; i < nOldSize; i++ ) {
      makunbound ( lpPage->Items [ i ].oKey );
      makunbound ( lpPage->Items [ i ].oData );
      fnSetPageNext ( lpPage, i, unbound );
    }
    /* If there is an underflow, put the object into the lpoUnderflow-list: */
    if ( bUnderflow ) {
      if ( lpoUnderflow ) {
	if ( ! boundp ( lpPage->oUnderflow ) ) {
	  lpPage->oUnderflow	= *lpoUnderflow;
	  *lpoUnderflow		= lpPage->oSelf;
	}
      } else {
	CERROR (( "Don't register the underflowed page.",
		  "Can't register underflowed page %s.",
		  fnPrintObject ( lpPage->oSelf, (LPSTR) NULL, 0 ) ));
      }
    }
  }
  RETURN ( bUnderflow );
} /* fnSetPageSize */

/* ----------------------------------------------------------------------- */
static void		fnSetPageNext		( LPBTREEPAGE lpPage,
						  int nIndex,
						  OBJID oNext )
{
  LPOBJID	lpoNext;
  LPBTREEPAGE	lpNextPage;

  PROCEDURE	( fnSetPageNext );

  ASSERT ( lpPage );
  if ( nIndex < 0 ) {
    nIndex	= -1;
    lpoNext	= & lpPage->oNext;
  } else {
    lpoNext	= & lpPage->Items [ nIndex ].oNext;
  }
  *lpoNext		= oNext;
  /* Put the parent reference into oNext: */
  if ( boundp ( oNext ) ) {
    lpNextPage		= fnGetBTreePage ( oNext );
    ASSERT ( lpNextPage != NULL );
    lpNextPage->oParentPage	= lpPage->oSelf;
    lpNextPage->oParentIndex	= Fixnum2ObjId ( nIndex );
  }

  RETURN ( VOID );
} /* fnSetPageNext */

/* ----------------------------------------------------------------------- */
static void		fnSetPageItem		( LPBTREEPAGE lpPageDest,
						  int nIndexDest,
						  LPBTREEPAGE lpPageSrce,
						  int nIndexSrce )
{
  PROCEDURE	( fnSetPageItem );

  lpPageDest->Items [ nIndexDest ]	= lpPageSrce->Items [ nIndexSrce ];
  fnSetPageNext ( lpPageDest, nIndexDest,
		  lpPageDest->Items [ nIndexDest ].oNext );

  RETURN ( VOID );
} /* fnSetPageItem */

/* ----------------------------------------------------------------------- */
static LPBTREEMAPPER	fnGetBTreeMapper	( OBJID oHeap,
						  OBJID oObjId,
						  SHLOCK * pnLock )
{
  SHLOCK	nLock	= (SHLOCK)
    ( (unsigned int) eshLockLevelNothing |
      (unsigned int) eshLockModeNothing );
  LPBTREEMAPPER	lpMapper;

  PROCEDURE	( fnGetBTreeMapper );

  lpMapper	= (LPBTREEMAPPER) NULL;
  if ( boundp ( oObjId ) ) {
    lpMapper	= (LPBTREEMAPPER) SH_key_to_address ( oObjId );
    ASSERT ( lpMapper != NULL );
    if ( ! ASSERT_TYPE ( oObjId, lpMapper, eshBTreeMapperTag ) ) {
      RETURN ( (LPBTREEMAPPER) NULL );
    }
    if ( lpMapper->timeMapper != timeGlobalStart &&
	 ( lpMapper->pValueKeyLower != NULL ||
	   lpMapper->pValueKeyUpper != NULL ) ) {
      makunbound ( lpMapper->oKeyLower );
      makunbound ( lpMapper->onTypeTagKeyLower );
      makunbound ( lpMapper->oKeyUpper );
      makunbound ( lpMapper->onTypeTagKeyUpper );
      lpMapper->pValueKeyLower	= NULL;
      lpMapper->pValueKeyUpper	= NULL;
      CERROR (( "Destroy the mapper object.",
		"Cannot access\n"
		"       %s\n"
		"       because it contained transient pointers of a\n"
		"       previous session, which are lost now.",
		fnPrintObject ( oObjId, (LPSTR) NULL, 0 ) ));
      fnDestroyObject ( oObjId, FALSE );
      lpMapper	= NULL;
    } else {
      /* 1998/07/03 HK: Hack: Assure that onTimeStamp contains always a
	 fixnum. Needed since for old databases the slot might be
	 unbound. */
      if ( ! fixnump ( lpMapper->onTimeStamp ) ) {
	lpMapper->onTimeStamp	= o1;
      }
      if ( boundp ( oHeap ) ) {
	nLock	=
	  fnLockInsert ( oHeap, eshLockVectorRead, oObjId, -1, (PHPEEK) NULL );
      }
      if ( (int) nLock < 0 ) {
	lpMapper	= (LPBTREEMAPPER) NULL;
      }
    }
  }
  if ( pnLock != NULL ) {
    *pnLock	= nLock;
  }
  RETURN ( lpMapper );
} /* fnGetBTreeMapper */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnLockBTreeMapper	( OBJID oHeap,
						  LPBTREEMAPPER lpMapper )
{
  SHLOCK	nLock	= (SHLOCK)
    ( (unsigned int) eshLockLevelNothing |
      (unsigned int) eshLockModeNothing );

  PROCEDURE	( fnLockBTreeMapper );

  if ( boundp ( oHeap ) ) {
    nLock	= fnLockInsert ( oHeap, eshLockVectorWrite,
				 lpMapper->oSelf, -1, (PHPEEK) NULL );
    if ( (int) nLock >= 0 ) {
      if ( ! gfnObjectStateChanged ( oHeap, lpMapper->oSelf ) )
	nLock	= eshLockFailed;
    }
  }
  RETURN ( nLock );
} /* fnLockBTreeMapper */

/* ----------------------------------------------------------------------- */
static COMPARETAG	fnCompare		( COMPARETAG nCompare,
						  LPCVOID pValue1,
						  SHTYPETAG nTypeTag1,
						  OBJID oObj2 )
{
  SHTYPETAG	nTypeTag2, nTypeTag;
  FIXNUM	nValue1, nValue2;
  COMPARETAG	nEqual, nNotEqual, nCompared;
  LPFNMETHOD	lpfnMethod;
  int		n1, n2, i, n;
  OBJID		oObj1 = NULLOBJID;
  psint		FAR * lpSHvector1 = NULL, FAR * lpSHvector2 = NULL;

  PROCEDURE	( fnCompare );

  nEqual	= nCompare;
  switch ( nCompare ) {
  case eshEq:
    nNotEqual	= eshNotEq;
    break;
  case eshEql:
    nNotEqual	= eshNotEql;
    break;
  case eshEqual:
    nNotEqual	= eshNotEqual;
    break;
  default:
    ERROR (( "Illegal compare mode %d.", nCompare ));
    RETURN ( eshNotEq );
  }

  switch ( nTypeTag1 ) {
  case eshShortObjIdTag:
    nTypeTag1	= typetagof ( SHORT2LONGOBJID ( * (LPOBJID) pValue1 ) );
    break;
  case eshObjIdTag:
    nTypeTag1	= typetagof ( * (LPOBJID) pValue1 );
    break;
  default:
    break;
  }
  nTypeTag2	= typetagof ( oObj2 );
  if ( immediatep ( nTypeTag1 ) && nTypeTag1 == nTypeTag2 &&
       * (LPOBJID) pValue1 == oObj2 ) {
    /* Same type & value, so objects are always eq, eql and equal: */
    RETURN ( nEqual );
  }

  /* Check for tags eshMinTag, eshMaxTag: */
  switch ( nTypeTag1 ) {
  case eshMinTag:
    RETURN ( eshLess );
  case eshMaxTag:
    RETURN ( eshGreater );
  default:
    break;
  }
  switch ( nTypeTag2 ) {
  case eshMinTag:
    RETURN ( eshGreater );
  case eshMaxTag:
    RETURN ( eshLess );
  default:
    break;
  }

  if ( nCompare == eshEq || nCompare == eshEql ) {
    /* The objects can't be eq or eql: */
    RETURN ( nNotEqual );
  }

  if ( nCompare == eshEqual ) {
    lpfnMethod	= _FindMethod ( nTypeTag1, gfnCompare );
    if ( lpfnMethod != NULL ) {
      RETURN ( (COMPARETAG) ( * lpfnMethod ) ( pValue1, nTypeTag1, oObj2 ) );
    }
    if ( nTypeTag1 != nTypeTag2 ) {
      RETURN ( nNotEqual );
    }
    oObj1	= * (LPOBJID) pValue1;
    if ( immediatep ( nTypeTag1 ) ) {
      /* Compare two immediate values: */
      nValue1	= fnObjId2Immediate ( oObj1, nTypeTag1 );
      nValue2	= fnObjId2Immediate ( oObj2, nTypeTag2 );
      if ( nValue1 < nValue2 ) {
	RETURN ( eshLess );
      }
      RETURN ( eshGreater );
    }
    ASSERT_ObjId_is_valid ( oObj1 );
    /* Compare both objects element for element; if a not-equal element
       is found, terminate with the found result. */
    lpSHvector1	= SH_key_to_address ( oObj1 );
    ASSERT ( lpSHvector1 != NULL );
    n1		= lpSHvector1 [ eshSHvectorIdxObjIds ];
    lpSHvector2	= SH_key_to_address ( oObj2 );
    ASSERT ( lpSHvector2 != NULL );
    n2		= lpSHvector2 [ eshSHvectorIdxObjIds ];
    n		= eshSHvectorIdxFirstObjId + MIN ( n1, n2 );
    /* Compare the references: */
    for ( i = eshSHvectorIdxFirstData; i < n; i++ ) {
      nTypeTag	= typetagof ( lpSHvector1 [ i ] );
      nCompared	= fnCompare ( nCompare, & lpSHvector1 [ i ], nTypeTag,
			      lpSHvector2 [ i ] );
      if ( nCompared != eshEqual )
	RETURN ( nCompared );
    }
    /* Objects are equal in all references ... */
    if ( n1 != n2 ) {
      /* ... but the number of references differ; decide by length: */
      RETURN ( ( n1 < n2 ) ? eshLess : eshGreater );
    }
    /* Now decide by value field: */
    n1	= lpSHvector1 [ eshSHvectorIdxSize ] -
      lpSHvector1 [ eshSHvectorIdxObjIds ];
    n2	= lpSHvector2 [ eshSHvectorIdxSize ] -
      lpSHvector2 [ eshSHvectorIdxObjIds ];
    n	= MIN ( n1, n2 ) - eshSHvectorIdxFirstObjId;
    if ( n > 0 ) {
      nCompared	= (COMPARETAG)
	memcmp ( & lpSHvector1 [ eshSHvectorIdxFirstObjId +
			       lpSHvector1 [ eshSHvectorIdxObjIds ] ],
		 & lpSHvector2 [ eshSHvectorIdxFirstObjId +
			       lpSHvector2 [ eshSHvectorIdxObjIds ] ],
		 n * sizeof ( psint ) );
      if ( (int) nCompared == 0 ) {
	RETURN ( nEqual );
      }
      if ( (int) nCompared < 0 ) {
	RETURN ( eshLess );
      }
      RETURN ( eshGreater );
    }
    /* The values are also equal; decide by length: */
    if ( n1 == n2 )
      RETURN ( nEqual );
    if  ( n1 < n2 )
      RETURN ( eshLess );
    RETURN ( eshGreater );
  }

  RETURN ( nNotEqual );
} /* fnCompare */

/* ----------------------------------------------------------------------- */
/* Binary search of start key in current page: */
static int		fnSearchKey		( LPPLOBBTREE	lpBTree,
						  LPBTREEPAGE	lpPage,
						  LPCVOID	pValueKeyLower,
						  SHTYPETAG
						  nTypeTagKeyLower,
						  LPINT		lpnIndexMiddle,
						  LPINT		lpnCompare )
{
  int		nIndexLeft, nIndexRight, nIndexMiddle;
  int		nCompare;

  PROCEDURE	( fnSearchKey );
  
  nIndexLeft	= 0;
  nIndexRight	= OBJID2FIXNUM ( lpPage->onCount ) - 1;
  ASSERT ( nIndexRight >= 0 );

  if ( nTypeTagKeyLower == eshMinTag ) {
    nIndexMiddle	= 0;
    nIndexRight		= -1;
    nCompare		= ( boundp ( lpPage->oNext ) ) ? -1 : 0;
  } else if ( nTypeTagKeyLower == eshMaxTag ) {
    nIndexMiddle	= nIndexRight;
    nCompare		=
      ( boundp ( lpPage->Items [ nIndexMiddle ].oNext ) ) ? 1 : 0;
  } else {
    do {
      nIndexMiddle	= ( nIndexLeft + nIndexRight ) / 2;
      nCompare		= fnKeyCmp ( lpBTree, pValueKeyLower, nTypeTagKeyLower,
				     lpPage->Items [ nIndexMiddle ].oKey );
      if ( nCompare <= 0 ) {
	nIndexRight	= nIndexMiddle - 1;
      }
      if ( nCompare >= 0 ) {
	nIndexLeft	= nIndexMiddle + 1;
      }
    } while ( nIndexRight >= nIndexLeft );
  }

  if ( lpnIndexMiddle != NULL ) {
    *lpnIndexMiddle	= nIndexMiddle;
  }
  if ( lpnCompare != NULL ) {
    *lpnCompare		= nCompare;
  }

  RETURN ( nIndexRight );
} /* fnSearchKey */

/* ----------------------------------------------------------------------- */
static int		fnKeyCmp		( LPPLOBBTREE lpBTree,
						  LPCVOID pValueKey1,
						  SHTYPETAG nTypeTagKey1,
						  OBJID oKey2 )
{
  static const char	szQuote []	= "\"";

  COMPARETAG		nCompare, nCompared;
  char			szKey2 [ 128 ], szBTree [ 128 ];
  OBJID			oKey1;

  PROCEDURE	( fnKeyCmp );

  ASSERT ( lpBTree != NULL );

  nCompare	= (COMPARETAG) ObjId2Fixnum ( lpBTree->oCompare );
  nCompared	= fnCompare ( nCompare, pValueKey1, nTypeTagKey1, oKey2 );

  switch ( nCompared ) {
  case eshEq:
  case eshEql:
  case eshEqual:
    RETURN (  0 );
  case eshLess:
    RETURN ( -1 );
  case eshGreater:
    RETURN (  1 );
  default:
    if ( nCompare == eshEq || nCompare == eshEql ) {
      switch ( nTypeTagKey1 ) {
      case eshDynCStringPtrTag:
	ERROR (( "Dynamic C string %s%s%s\n"
		 "       cannot be used as search key\n"
		 "       for %s,\n"
		 "       since dynamic strings have no persistent identity.",
		 szQuote, (LPSTR) pValueKey1, szQuote,
		 PrintObject ( lpBTree->oSelf, szBTree ) ));
	break;
      case eshDynCFloatPtrTag:
	ERROR (( "Dynamic C float %g cannot be used as search key\n"
		 "       for %s,\n"
		 "       since dynamic floats have no persistent identity.",
		 * (float FAR *) pValueKey1,
		 PrintObject ( lpBTree->oSelf, szBTree ) ));
	break;
      case eshDynCDoublePtrTag:
	ERROR (( "Dynamic C double float %g cannot be used as search key\n"
		 "       for %s,\n"
		 "       since dynamic doubles have no persistent identity.",
		 * (double FAR *) pValueKey1,
		 PrintObject ( lpBTree->oSelf, szBTree ) ));
	break;
      case eshDynCQuadruplePtrTag:
	ERROR (( "Dynamic C long double float %lg"
		 " cannot be used as search key\n"
		 "       for %s,\n"
		 "       since dynamic long doubles have no"
		 " persistent identity.",
		 * (long double FAR *) pValueKey1,
		 PrintObject ( lpBTree->oSelf, szBTree ) ));
	break;
      default:
	oKey1	= * (LPOBJID) pValueKey1;
	if ( oKey1 < oKey2 ) {
	  RETURN ( -1 );
	} else if ( oKey1 > oKey2 ) {
	  RETURN ( 1 );
	}
      }
    } else {
      char	szKey1 [ 32 ];
      LPCSTR	lpszQuote;
      LPSTR	lpszKey1;

      lpszQuote	= szQuote;
      switch ( nTypeTagKey1 ) {
      case eshDynCStringPtrTag:
	lpszKey1	= (LPSTR) pValueKey1;
	break;
      case eshDynCFloatPtrTag:
	sprintf ( szKey1, "%g", * (float FAR *) pValueKey1 );
	lpszKey1	= szKey1;
	lpszQuote	= szEmpty;
	break;
      case eshDynCDoublePtrTag:
	sprintf ( szKey1, "%g", * (double FAR *) pValueKey1 );
	lpszKey1	= szKey1;
	lpszQuote	= szEmpty;
	break;
      case eshDynCQuadruplePtrTag:
	sprintf ( szKey1, "%lg", * (long double FAR *) pValueKey1 );
	lpszKey1	= szKey1;
	lpszQuote	= szEmpty;
	break;
      default:
	oKey1		= * (LPOBJID) pValueKey1;
	lpszKey1	= fnPrintObject ( oKey1, (LPSTR) NULL, 0 );
	lpszQuote	= szEmpty;
	break;
      }
      ERROR (( "Illegal search key %s%s%s for\n"
	       "       object %s:\n"
	       "       compare failed with\n"
	       "       object %s.",
	       lpszQuote, lpszKey1, lpszQuote,
	       PrintObject ( lpBTree->oSelf, szBTree ),
	       PrintObject ( oKey2, szKey2 ) ));
    }
    break;
  }
  RETURN ( 0 );
} /* fnKeyCmp */

/* ----------------------------------------------------------------------- */
/* Fuege *lpInsert rechts von lpPage->Items [ nIndex ] ein: */
static SHLOCK		fnInsert		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  int nIndex,
						  LPBOOL lpbMove,
						  LPBTREEITEM lpInsert,
						  LPBTREEITEM lpMove )
{
  SHLOCK	nLockOld;
  register int	i, n;
  int		nPageSize, nPageSizeDiv2, nItems;
  OBJID		oPageNew;
  LPBTREEPAGE	lpPageNew;

  PROCEDURE		( fnInsert );

  nLockOld	= fnLockBTreePage ( oHeap, lpBTree, lpPage );
  if ( (int) nLockOld < 0 ) {
    RETURN ( nLockOld );
  }

  nItems	= OBJID2FIXNUM ( lpPage->onCount );
  nPageSize	= OBJID2FIXNUM ( lpBTree->onPageSize );
  nPageSizeDiv2	= nPageSize / 2;
  if ( nItems < nPageSize ) {
    fnSetPageSize ( lpBTree, lpPage, nItems + 1, (LPOBJID) NULL );
    *lpbMove		= (BOOL) FALSE;
    for ( i = nItems, n = nIndex + 2; i >= n; i-- ) {
      fnSetPageItem ( lpPage, i, lpPage, i - 1 );
    }
    lpPage->Items [ nIndex + 1 ]	= *lpInsert;
    fnSetPageNext ( lpPage, nIndex + 1, lpPage->Items [ nIndex + 1 ].oNext );
  } else {
    /* Seite *lpPage ist voll; teile sie auf und weise *lpMove das
       herausfallende Item zu: */
    oPageNew	= fnCreateBTreePage ( lpBTree );
    ASSERT ( oPageNew != NULLOBJID );
    lpPageNew	= (LPBTREEPAGE) SH_key_to_address ( oPageNew );
    ASSERT ( lpPageNew != NULL );
    INCOBJID ( lpBTree->onPages, 1 );
    if ( nIndex < nPageSizeDiv2 ) {
      if ( nIndex == nPageSizeDiv2 - 1 ) {
	*lpMove	= *lpInsert;
      } else {
	*lpMove	= lpPage->Items [ nPageSizeDiv2 - 1 ];
	for ( i = nPageSizeDiv2 - 1, n = nIndex + 2; i >= n; i-- ) {
	  fnSetPageItem ( lpPage, i, lpPage, i - 1 );
	}
	lpPage->Items [ nIndex + 1 ]	= *lpInsert;
	fnSetPageNext ( lpPage, nIndex + 1,
			lpPage->Items [ nIndex + 1 ].oNext );
      }
      for ( i = 0; i < nPageSizeDiv2; i++ ) {
	fnSetPageItem ( lpPageNew, i, lpPage, i + nPageSizeDiv2 );
      }
    } else {
      /* Fuege *lpInsert in die rechte Seite ein: */
      nIndex	-= nPageSizeDiv2;
      *lpMove	= lpPage->Items [ nPageSizeDiv2 ];
      for ( i = 0; i < nIndex; i++ ) {
	fnSetPageItem ( lpPageNew, i, lpPage, i + nPageSizeDiv2 + 1 );
      }
      lpPageNew->Items [ nIndex ]	= *lpInsert;
      fnSetPageNext ( lpPageNew, nIndex, lpPageNew->Items [ nIndex ].oNext );
      for ( i = nIndex + 1; i < nPageSizeDiv2; i++ ) {
	fnSetPageItem ( lpPageNew, i, lpPage, i + nPageSizeDiv2 );
      }
    }
    fnSetPageSize ( lpBTree, lpPage, nPageSizeDiv2, (LPOBJID) NULL );
    fnSetPageSize ( lpBTree, lpPageNew, nPageSizeDiv2, (LPOBJID) NULL ); 
    fnSetPageNext ( lpPageNew, -1, lpMove->oNext );
    lpMove->oNext	= oPageNew;
  }
  RETURN ( nLockOld );
} /* fnInsert */

/* ----------------------------------------------------------------------- */
static OBJID	fnMakePersistentData	( LPCVOID	pValueData,
					  SHTYPETAG	nTypeTagData )
{
  OBJID		oData = NULLOBJID;

  PROCEDURE	( fnMakePersistentData );

  switch ( nTypeTagData ) {
  case eshDynCStringPtrTag:
    oData	= make_string ( pValueData );
    break;
  case eshDynCFloatPtrTag:
    oData	= make_float ( * (float FAR *) pValueData );
    break;
  case eshDynCDoublePtrTag:
    oData	= make_double ( * (double FAR *) pValueData );
    break;
  case eshShortObjIdTag:
    oData	= Short2LongObjId ( * (SHORTOBJID *) pValueData );
    break;
  default:
    oData	= * (LPOBJID) pValueData;
    if ( ! immediatep ( oData ) ) {
      ASSERT_ObjId_is_valid ( oData );
    }
    break;
  }

  RETURN ( oData );
} /* fnMakePersistentData */

/* ----------------------------------------------------------------------- */
/* Suche Schluessel oKey, nTypeTagKey auf Seite *lpPage; ( ! *lpbMove ) */
static BTREERESULT	fnSearch		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPCVOID pValueKey,
						  SHTYPETAG nTypeTagKey,
						  LPOBJID lpoKeyFound,
						  LPOBJID lpoDataFound,
						  LPOBJID lpoDataInsert,
						  LPBOOL lpbMove,
						  LPBTREEITEM lpMove )
{
  int			nIndexRight, nIndexMiddle, nCompare;
  BTREERESULT		nResult;
  BTREEITEM		Move;
  OBJID			oKey = NULLOBJID, oNext;
  LPBTREEPAGE		lpPageNext;
  SHLOCK		nLockOld;

  PROCEDURE	( fnSearch );

  nResult	= btreeNotFound;
  if ( lpPage == NULL ) {
    /* Item ist nicht im Baum: */
    if ( lpoDataInsert ) {
      if ( markerp ( nTypeTagKey ) ) {
	char	szMarker [ 128 ];
	ERROR (( "Cannot insert key %s\n"
		 "       into %s",
		 PrintObject ( fnImmediate2ObjId ( nTypeTagKey, &nTypeTagKey ),
			       szMarker ),
		 fnPrintObject ( lpBTree->oSelf, (LPSTR) NULL, 0 ) ));
	RETURN ( (BTREERESULT) eshGeneralError );
      }
      *lpbMove	= (BOOL) TRUE;
      nResult	= btreeInserted;
      oKey	= fnMakePersistentData ( pValueKey, nTypeTagKey );
      if ( ObjId2Fixnum ( lpBTree->oCompare ) == eshEqual ) {
	make_readonly ( oKey );
      }
      lpMove->oKey	= oKey;
      lpMove->oData	= *lpoDataInsert;
      makunbound ( lpMove->oNext );
      if ( lpoKeyFound != NULL ) {
	makunbound ( *lpoKeyFound );
      }
      if ( lpoDataFound != NULL ) {
	makunbound ( *lpoDataFound );
      }
    }
  } else {
    nIndexRight	= fnSearchKey ( lpBTree, lpPage, pValueKey, nTypeTagKey,
				&nIndexMiddle, &nCompare );
    if ( nCompare == 0 ) {
      /* Gefunden: */
      if ( lpoDataInsert ) {
	nLockOld	=  fnLockBTreePage ( oHeap, lpBTree, lpPage );
	if ( (int) nLockOld < 0 ) {
	  RETURN ( (BTREERESULT) nLockOld );
	}
	nResult					= btreeUpdated;
	lpPage->Items [ nIndexMiddle ].oData	= *lpoDataInsert;
      } else {
	nResult					= btreeFound;
      }
      if ( lpoKeyFound ) {
	*lpoKeyFound	= lpPage->Items [ nIndexMiddle ].oKey;
      }
      if ( lpoDataFound ) {
	*lpoDataFound	= lpPage->Items [ nIndexMiddle ].oData;
      }
    } else {
      /* Item ist nicht in dieser Seite: */
      oNext		= ( nIndexRight < 0 ) ?
	lpPage->oNext : lpPage->Items [ nIndexRight ].oNext;
      lpPageNext	= fnGetBTreePage ( oNext );
      nResult		= fnSearch ( oHeap, lpBTree, lpPageNext,
				     pValueKey, nTypeTagKey,
				     lpoKeyFound, lpoDataFound,
				     lpoDataInsert, lpbMove, &Move );
      if ( (int) nResult < 0 ) {
	RETURN ( (BTREERESULT) nResult );
      }
      if ( *lpbMove ) {
	nLockOld	= fnInsert ( oHeap, lpBTree, lpPage, nIndexRight,
				     lpbMove, &Move, lpMove );
	if ( (int) nLockOld < 0 ) {
	  RETURN ( (BTREERESULT) nLockOld );
	}
      }
    }
  }
  RETURN ( nResult );
} /* fnSearch */

/* ----------------------------------------------------------------------- */
/* *lpPage = Seite mit Unterlauf, *lpPagePrev = Vorgaenger-Seite: */
static SHLOCK		fnUnderflow		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPBTREEPAGE lpPagePrev,
						  int nIndex,
						  LPOBJID lpoUnderflow,
						  LPBOOL lpbUnderflow )
{
  SHLOCK	nLockOld;
  OBJID		oPageNext;
  LPBTREEPAGE	lpPageNext;
  register int	i, n;
  int		nPageSize, nPageSizeDiv2, nMove, nItemsNext, nItemsPrev;

  PROCEDURE	( fnUnderflow );

  nLockOld	= fnLockBTreePage ( oHeap, lpBTree, lpPage );
  if ( (int) nLockOld < 0 ) {
    RETURN ( nLockOld );
  }
  fnLockBTreePage ( oHeap, (LPPLOBBTREE) NULL, lpPagePrev );

  nItemsPrev	= OBJID2FIXNUM ( lpPagePrev->onCount );
  nPageSize	= OBJID2FIXNUM ( lpBTree->onPageSize );
  nPageSizeDiv2	= nPageSize / 2;
  if ( nIndex < nItemsPrev - 1 ) {
    /* *lpPageNext := rechts von *lpPage */
    nIndex++;
    oPageNext	= lpPagePrev->Items [ nIndex ].oNext;
    lpPageNext	= fnGetBTreePage ( oPageNext );
    ASSERT ( lpPageNext != NULL );
    fnLockBTreePage ( oHeap, (LPPLOBBTREE) NULL, lpPageNext );
    nItemsNext	= OBJID2FIXNUM ( lpPageNext->onCount );
    nMove	= ( nItemsNext - nPageSizeDiv2 + 1 ) / 2;
    /* nMove == Anzahl Items auf benachbarter Seite *lpPageNext */
    fnSetPageItem ( lpPage, nPageSizeDiv2 - 1, lpPagePrev, nIndex );
    fnSetPageNext ( lpPage, nPageSizeDiv2 - 1, lpPageNext->oNext );
    if ( nMove > 0 ) {
      /* Bringe nMove Items von *lpPageNext nach *lpPage: */
      for ( i = 0, n = nMove - 1; i < n; i++ ) {
	fnSetPageItem ( lpPage, i + nPageSizeDiv2, lpPageNext, i );
      }
      fnSetPageItem ( lpPagePrev, nIndex, lpPageNext, n );
      fnSetPageNext ( lpPagePrev, nIndex, lpPageNext->oSelf );
      fnSetPageNext ( lpPageNext, -1, lpPageNext->Items [ n ].oNext );
      nItemsNext	-= nMove;
      for ( i = 0; i < nItemsNext; i++ ) {
	fnSetPageItem ( lpPageNext, i, lpPageNext, i + nMove );
      }
      fnSetPageSize ( lpBTree, lpPageNext, nItemsNext, (LPOBJID) NULL );
      fnSetPageSize ( lpBTree, lpPage, nPageSizeDiv2 + n, (LPOBJID) NULL );
      *lpbUnderflow	= (BOOL) FALSE;
    } else {
      /* Mische Seiten *lpPage und *lpPageNext */
#if defined(LOGGING)
      fprintf ( stderr, "%s(%d): Melting %s", __szProc__, __LINE__,
	        fnPrintObject ( lpPage->oSelf, (LPSTR) NULL, 0 ) );
      fprintf ( stderr, " and %s\n",
	        fnPrintObject ( lpPageNext->oSelf, (LPSTR) NULL, 0 ) );
#endif /* #if defined(LOGGING) */
      for ( i = 0; i < nPageSizeDiv2; i++ ) {
	fnSetPageItem ( lpPage, i + nPageSizeDiv2, lpPageNext, i );
      }
      for ( i = nIndex, n = nItemsPrev - 1; i < n; i++ ) {
	fnSetPageItem ( lpPagePrev, i, lpPagePrev, i + 1 );
      }
      /* HK 14.1.94: Mark next page as empty: */
      fnSetPageSize ( lpBTree, lpPageNext, 0, lpoUnderflow );
      fnSetPageSize ( lpBTree, lpPage, nPageSize, (LPOBJID) NULL );
      *lpbUnderflow	=
	fnSetPageSize ( lpBTree, lpPagePrev, n, lpoUnderflow );
    }
  } else {
    /* *lpPageNext := Seite links von *lpPage */
    oPageNext	=  ( nIndex <= 0 ) ?
      lpPagePrev->oNext : lpPagePrev->Items [ nIndex - 1 ].oNext;
    lpPageNext	=  fnGetBTreePage ( oPageNext );
    ASSERT ( lpPageNext != NULL );
    fnLockBTreePage ( oHeap, (LPPLOBBTREE) NULL, lpPageNext );
    nItemsNext	= OBJID2FIXNUM ( lpPageNext->onCount ) + 1;
    nMove	= ( nItemsNext - nPageSizeDiv2 ) / 2;
    if ( nMove > 0 ) {
      /* Bringe nMove Elemente von *lpPageNext nach *lpPage: */
      for ( i = nPageSizeDiv2 - 2; i >= 0; i-- ) {
	fnSetPageItem ( lpPage, i + nMove, lpPage, i );
      }
      fnSetPageItem ( lpPage, nMove - 1, lpPagePrev, nIndex );
      fnSetPageNext ( lpPage, nMove - 1, lpPage->oNext );
      nItemsNext	-= nMove;
      for ( i = nMove - 2; i >= 0; i-- ) {
	fnSetPageItem ( lpPage, i, lpPageNext, i + nItemsNext );
      }
      fnSetPageNext ( lpPage, -1, lpPageNext->Items [ nItemsNext - 1 ].oNext );
      fnSetPageItem ( lpPagePrev, nIndex, lpPageNext, nItemsNext - 1 );
      fnSetPageNext ( lpPagePrev, nIndex, lpPage->oSelf );
      fnSetPageSize ( lpBTree, lpPageNext, nItemsNext - 1, (LPOBJID) NULL );
      fnSetPageSize ( lpBTree, lpPage, nPageSizeDiv2 - 1 + nMove,
		      (LPOBJID) NULL );
      *lpbUnderflow	= (BOOL) FALSE;
    } else {
      /* Mische Seiten *lpPageNext und *lpPage: */
#if defined(LOGGING)
      fprintf ( stderr, "%s(%d): Melting %s", __szProc__, __LINE__,
	        fnPrintObject ( lpPageNext->oSelf, (LPSTR) NULL, 0 ) );
      fprintf ( stderr, " and %s\n",
	        fnPrintObject ( lpPage->oSelf, (LPSTR) NULL, 0 ) );
#endif /* #if defined(LOGGING) */
      fnSetPageItem ( lpPageNext, nItemsNext - 1, lpPagePrev, nIndex );
      fnSetPageNext ( lpPageNext, nItemsNext - 1, lpPage->oNext );
      for ( i = 0; i < nPageSizeDiv2 - 1; i++ ) {
	fnSetPageItem ( lpPageNext, i + nItemsNext, lpPage, i );
      }
      /* HK 14.1.94: Mark next page as empty: */
      fnSetPageSize ( lpBTree, lpPage, 0, lpoUnderflow );
      fnSetPageSize ( lpBTree, lpPageNext, nPageSize, (LPOBJID) NULL );
      *lpbUnderflow	=
	fnSetPageSize ( lpBTree, lpPagePrev, nItemsPrev - 1, lpoUnderflow );
    }
  }
  RETURN ( nLockOld );
} /* fnUnderflow */

/* ----------------------------------------------------------------------- */
static SHLOCK		fnDel			( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPBTREEPAGE lpPagePrev,
						  int nIndex,
						  LPOBJID lpoUnderflow,
						  LPBOOL lpbUnderflow )
{
  SHLOCK	nLockOld;
  OBJID		oPageNext;
  LPBTREEPAGE	lpPageNext;
  int		nItems;

  PROCEDURE	( fnDel );

  nItems	= OBJID2FIXNUM ( lpPage->onCount );
  oPageNext	= lpPage->Items [ nItems - 1 ].oNext;
  lpPageNext	= fnGetBTreePage ( oPageNext );
  if ( lpPageNext != NULL ) {
    nLockOld	= fnDel ( oHeap, lpBTree, lpPageNext, lpPagePrev, nIndex,
			  lpoUnderflow, lpbUnderflow );
    if ( *lpbUnderflow && (int) nLockOld >= 0 )
      nLockOld	= fnUnderflow ( oHeap, lpBTree, lpPageNext, lpPage, nItems - 1,
			        lpoUnderflow, lpbUnderflow );
  } else {
    nLockOld	= fnLockBTreePage ( oHeap, lpBTree, lpPage );
    if ( (int) nLockOld < 0 )
      RETURN ( nLockOld );
    fnLockBTreePage ( oHeap, (LPPLOBBTREE) NULL, lpPagePrev );
    fnSetPageNext ( lpPage, nItems - 1, lpPagePrev->Items [ nIndex ].oNext );
    fnSetPageItem ( lpPagePrev, nIndex, lpPage, nItems - 1 );
    nItems--;
    *lpbUnderflow	=
      fnSetPageSize ( lpBTree, lpPage, nItems, lpoUnderflow );
  }
  RETURN ( nLockOld );
} /* fnDel */

/* ----------------------------------------------------------------------- */
static BTREERESULT	fnDelete		( OBJID oHeap,
						  LPPLOBBTREE lpBTree,
						  LPBTREEPAGE lpPage,
						  LPCVOID pValueKey,
						  SHTYPETAG nTypeTagKey,
						  LPOBJID lpoUnderflow,
						  LPBOOL lpbUnderflow )
{
  BTREERESULT	nResult;
  SHLOCK	nLockOld;
  int		nItems, nIndexRight, nIndexMiddle;
  int		nCompare;
  register int	i;
  OBJID		oPageNext;
  LPBTREEPAGE	lpPageNext;

  PROCEDURE	( fnDelete );

  nResult		= btreeNotFound;
  if ( lpPage == NULL ) {
    *lpbUnderflow	= (BOOL) FALSE;
  } else {
    nItems	= OBJID2FIXNUM ( lpPage->onCount );
    nIndexRight	= fnSearchKey ( lpBTree, lpPage, pValueKey, nTypeTagKey,
				&nIndexMiddle, &nCompare );
    oPageNext	= ( nIndexRight < 0 ) ?
      lpPage->oNext : lpPage->Items [ nIndexRight ].oNext;
    lpPageNext	= fnGetBTreePage ( oPageNext );
    if ( nCompare == 0 ) {
      /* Gefunden; loesche lpPage->Items [ nIndexMiddle ]: */
      nResult	= btreeDeleted;
      nLockOld	= fnLockBTreePage ( oHeap, lpBTree, lpPage );
      if ( (int) nLockOld < 0 ) {
	RETURN ( (BTREERESULT) nLockOld );
      }
      if ( ObjId2Fixnum ( lpBTree->oCompare ) == eshEqual ) {
	make_readwrite ( lpPage->Items [ nIndexMiddle ].oKey );
      }
      if ( lpPageNext == NULL ) {
	/* *lpPage ist eine Endseite: */
	nItems--;
	for ( i = nIndexMiddle; i < nItems; i++ ) {
	  fnSetPageItem ( lpPage, i, lpPage, i + 1 );
	}
	*lpbUnderflow	=
	  fnSetPageSize ( lpBTree, lpPage, nItems, lpoUnderflow );
      } else {
	nLockOld	= fnDel ( oHeap, lpBTree, lpPageNext, lpPage,
				  nIndexMiddle, lpoUnderflow, lpbUnderflow );
	if ( *lpbUnderflow && (int) nLockOld >= 0 ) {
	  nLockOld	= fnUnderflow ( oHeap, lpBTree, lpPageNext, lpPage,
				        nIndexRight,
				        lpoUnderflow, lpbUnderflow );
	  if ( (int) nLockOld < 0 )
	    RETURN ( (BTREERESULT) nLockOld );
	}
      }
    } else {
      nResult	= fnDelete ( oHeap, lpBTree, lpPageNext,
			     pValueKey, nTypeTagKey,
			     lpoUnderflow, lpbUnderflow );
      if ( *lpbUnderflow && (int) nResult >= 0 ) {
	nLockOld	= fnUnderflow ( oHeap, lpBTree, lpPageNext, lpPage,
				        nIndexRight,
				        lpoUnderflow, lpbUnderflow );
	if ( (int) nLockOld < 0 )
	  RETURN ( (BTREERESULT) nLockOld );
      }
    }
  }
  RETURN ( nResult );
} /* fnDelete */

/* ----------------------------------------------------------------------- */
static int		fnDumpBTree		( OBJID oHeap,
						  FILE FAR * lpStream,
						  LPBTREEPAGE lpPage,
						  int nIndent,
						  LPINT lpnLine )
{
  int		nPrinted, i, n;
  OBJID		oNext;

  PROCEDURE	( fnDumpBTree );

  nPrinted	= 0;
  if ( lpStream != NULL && lpPage != NULL ) {
    if ( lpnLine ) {
      fprintf ( lpStream, "%4d: ", *lpnLine );
      (*lpnLine)++;
    }
    for ( i = 0; i < nIndent; i++ )
      fputc ( ' ', lpStream );
    n	= ObjId2Fixnum ( lpPage->onCount );
    fprintf ( lpStream, "%s\n",
	      fnPrintObject ( lpPage->oSelf, (LPSTR) NULL, 0 ) );
    oNext	= lpPage->oNext;
    nPrinted	+= n +
      fnDumpBTree ( oHeap, lpStream,
		    fnGetBTreePage ( oNext ),
		    nIndent + 1, lpnLine );
    for ( i = 0; i < n; i++ ) {
      oNext	= lpPage->Items [ i ].oNext;
      nPrinted	+= fnDumpBTree ( oHeap, lpStream,
				 fnGetBTreePage ( oNext ),
				 nIndent + 1, lpnLine );
    }
  }
  RETURN ( nPrinted );
} /* fnDumpBTree */

/* ----------------------------------------------------------------------- */
static FIXNUM	fnObjIds2Values	( FIXNUM nObjIds,
				  LPOBJID lpObjIds,
				  SHORTOBJID FAR * pnValues,
				  LPSHTYPETAG pnTypeTags )
{
  int		i;
  OBJID		oValue;
  SHTYPETAG	nTypeTag;

  PROCEDURE	( fnObjIds2Values );

  ASSERT ( pnValues != NULL );
  ASSERT ( pnTypeTags != NULL );
  for ( i = 0; i < nObjIds; i++ ) {
    oValue		= lpObjIds [ i ];
    nTypeTag		= typetagof ( oValue );
    pnValues [ i ]	= ( immediatep ( nTypeTag ) ) ?
      fnObjId2Immediate ( oValue, nTypeTag ) : LONG2SHORTOBJID ( oValue );
    pnTypeTags [ i ]	= nTypeTag;
  }

  RETURN ( nObjIds );
} /* fnObjIds2Values */

/* ----------------------------------------------------------------------- */
static BTREERESULT
fnServerBtreeDeleteByPtr	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjIdBTree,
				  LPCVOID	pValueKey,
				  SHTYPETAG	nTypeTagKey )
{
  BTREERESULT	eResult;
  OBJID		oHeap, oBTree, oKey;

  PROCEDURE	( fnServerBtreeDeleteByPtr );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  eResult	= fnBTreeDelete ( oHeap, oBTree, pValueKey, nTypeTagKey );

  RETURN ( eResult );
} /* fnServerBtreeDeleteByPtr */

/* ----------------------------------------------------------------------- */
static BTREERESULT
fnServerBtreeInsertByPtr	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjIdBTree,
				  LPCVOID	pValueKey,
				  SHTYPETAG	nTypeTagKey,
				  LPCVOID	pValueData,
				  SHTYPETAG	nTypeTagData )
{
  BTREERESULT	eResult;
  OBJID		oHeap, oBTree, oData;

  PROCEDURE	( fnServerBtreeInsertByPtr );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  oData		= fnMakePersistentData ( pValueData, nTypeTagData );
  eResult	= fnBTreeInsert ( oHeap, oBTree, pValueKey, nTypeTagKey,
				  oData );

  RETURN ( eResult );
} /* fnServerBtreeInsertByPtr */

/* ----------------------------------------------------------------------- */
static FIXNUM
fnServerBtreemapSearchByPtr	( SHORTOBJID	oShortObjIdMapper,
				  SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjIdBTree,
				  LPCVOID	pValueKeyLower,
				  SHTYPETAG	nTypeTagKeyLower,
				  COMPARETAG	eCompareLower,
				  LPCVOID	pValueKeyUpper,
				  SHTYPETAG	nTypeTagKeyUpper,
				  COMPARETAG	eCompareUpper,
				  BOOL		bDescending )
{
  FIXNUM	nMapped;
  OBJID		oHeap, oBTree, oMapper;

  PROCEDURE	( fnServerBtreemapSearchByPtr );

  oMapper	= Short2LongObjId ( oShortObjIdMapper );
  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= ( oShortObjIdBTree != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdBTree ) : NULLOBJID;
  nMapped	= fnBTreeMapSearch ( oMapper, oHeap, oBTree,
				     pValueKeyLower, nTypeTagKeyLower,
				     eCompareLower,
				     pValueKeyUpper, nTypeTagKeyUpper,
				     eCompareUpper, bDescending );

  RETURN ( nMapped );
} /* fnServerBtreemapSearchByPtr */

/* ----------------------------------------------------------------------- */
static FIXNUM
fnServerBtreemapFirstByPtr	( SHORTOBJID	*lpoShortObjIdMapper,
				  SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjIdBTree,
				  LPCVOID	pValueKeyLower,
				  SHTYPETAG	nTypeTagKeyLower,
				  COMPARETAG	eCompareLower,
				  LPCVOID	pValueKeyUpper,
				  SHTYPETAG	nTypeTagKeyUpper,
				  COMPARETAG	eCompareUpper,
				  BOOL		bDescending,
				  FIXNUM	nMap,
				  LPOBJID	*ppoKey,
				  LPOBJID	*ppoData,
				  VECTOR ( int, nMap )		pnValueKey,
				  VECTOR ( u_int, nMap )	pnTypeTagKey,
				  VECTOR ( int, nMap )		pnValueData,
				  VECTOR ( u_int, nMap )	pnTypeTagData )
{
  FIXNUM	nMapped;
  OBJID		oHeap, oBTree, oMapper;

  PROCEDURE	( fnServerBtreemapFirstByPtr );

  ASSERT ( ppoKey != NULL );
  ASSERT ( ppoData != NULL );

  *ppoKey	= (LPOBJID) NULL;
  *ppoData	= (LPOBJID) NULL;

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );

  *ppoKey	= (LPOBJID) Malloc ( nMap * sizeof ( **ppoKey ) );
  ASSERT ( *ppoKey != NULL );

  *ppoData	= (LPOBJID) Malloc ( nMap * sizeof ( **ppoData ) );
  ASSERT ( *ppoData != NULL );

  nMapped	= fnBTreeMapFirst ( &oMapper, oHeap, oBTree,
				    pValueKeyLower, nTypeTagKeyLower,
				    eCompareLower,
				    pValueKeyUpper, nTypeTagKeyUpper,
				    eCompareUpper, bDescending, nMap,
				    *ppoKey, *ppoData );

  if ( nMapped > 0 ) {
    fnObjIds2Values ( nMapped, *ppoKey,
		      (SHORTOBJID *) pnValueKey,
		      (LPSHTYPETAG) pnTypeTagKey );
    fnObjIds2Values ( nMapped, *ppoData,
		      (SHORTOBJID *) pnValueData,
		      (LPSHTYPETAG) pnTypeTagData );
    oMapper	= LONG2SHORTOBJID ( oMapper );
  } else {
    oMapper	= NULLOBJID;
  }

  if ( lpoShortObjIdMapper != NULL ) {
    *lpoShortObjIdMapper	= oMapper;
  }

  if ( *ppoKey != NULL ) {
    Free ( *ppoKey );
    *ppoKey	= (LPOBJID) NULL;
  }
  if ( *ppoData != NULL ) {
    Free ( *ppoData );
    *ppoData	= (LPOBJID) NULL;
  }

  RETURN ( nMapped );
} /* fnServerBtreemapFirstByPtr */

/* ----------------------------------------------------------------------- */
static FIXNUM
fnServerBtreeSearchByPtr	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjIdBTree,
				  LPCVOID	pValueKey,
				  SHTYPETAG	nTypeTagKey,
				  FIXNUM	*pnValueKey,
				  SHTYPETAG	*pnTypeTagKey,
				  FIXNUM	*pnValueData,
				  SHTYPETAG	*pnTypeTagData )
{
  OBJID		oHeap, oBTree;
  OBJID		oKeyFound = (OBJID) NULLOBJID, oData = (OBJID) NULLOBJID;
  BTREERESULT	eResult;

  PROCEDURE	( fnServerBtreeSearchByPtr );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );

  eResult	= fnBTreeSearch ( oHeap, oBTree, pValueKey, nTypeTagKey,
				  &oKeyFound, &oData );

  fnObjIds2Values ( 1, &oKeyFound, pnValueKey,  pnTypeTagKey );
  fnObjIds2Values ( 1, &oData, pnValueData, pnTypeTagData );

  RETURN ( eResult );
} /* fnServerBtreeSearchByPtr */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
OBJID		fnMakeBTree		( COMPARETAG	nTestMode )
{
  OBJID		oBTree;

  PROCEDURE	( fnMakeBTree );

  oBTree	=
    fnCreateObject ( (SHTYPETAG) eshBTreeTag, 0, NULLTYPETAG, 0 );
  fnBTreeTestMode ( oBTree, nTestMode );

  RETURN ( oBTree );
} /* fnMakeBTree */

/* ----------------------------------------------------------------------- */
static MAPMODE		fnMap			( OBJID		oHeap,
						  LPPLOBBTREE	pBTree,
						  LPBTREEPAGE	pPage,
						  LPINT		pnKeys,
						  LPOBJID	poKeyLower,
						  LPSHTYPETAG
						  pnTypeTagKeyLower,
						  LPCVOID	pValueKeyLower,
						  COMPARETAG	eCompareLower,
						  LPOBJID	poKeyUpper,
						  LPSHTYPETAG
						  pnTypeTagKeyUpper,
						  LPCVOID	pValueKeyUpper,
						  COMPARETAG	eCompareUpper,
						  BOOL		bDescending,
						  LPFNMAPBTREE	pfnMapItem,
						  LPVOID	pUserData )
{
  int		nItems, nIndexRight;
  int		nCompare, i, iIncr;
  OBJID		oKey, oNext, oData, onTimeStamp;
  LPOBJID	poKeyCurrent;
  LPCVOID	pValueKeyCurrent, pValueKeyLast;
  LPSHTYPETAG	pnTypeTagKeyCurrent;
  SHTYPETAG	nTypeTagKeyLast;
  COMPARETAG	eCompareCurrent, eCompareLast;
  LPBTREEPAGE	pPageNext;
  MAPMODE	nMapMode = mapmodeContinue;
  BOOL		bMapItem = FALSE;

  PROCEDURE	( fnMap );
  ASSERT ( eCompareLower == eshGreater || eCompareLower == eshGreaterEqual );
  ASSERT ( eCompareUpper == eshLess || eCompareUpper == eshLessEqual );

  if ( pPage == NULL ) {
    RETURN ( mapmodeContinue );
  }

  if ( bDescending ) {
    pValueKeyCurrent	= pValueKeyUpper;
    poKeyCurrent	= poKeyUpper;
    pnTypeTagKeyCurrent	= pnTypeTagKeyUpper;
    eCompareCurrent	= eCompareUpper;
    pValueKeyLast	= pValueKeyLower;
    nTypeTagKeyLast	= *pnTypeTagKeyLower;
    eCompareLast	= eCompareLower;
  } else {
    pValueKeyCurrent	= pValueKeyLower;
    poKeyCurrent	= poKeyLower;
    pnTypeTagKeyCurrent	= pnTypeTagKeyLower;
    eCompareCurrent	= eCompareLower;
    pValueKeyLast	= pValueKeyUpper;
    nTypeTagKeyLast	= *pnTypeTagKeyUpper;
    eCompareLast	= eCompareUpper;
  }

  /* Binary search of current iterated key in current page: */
  nIndexRight	= fnSearchKey ( pBTree, pPage,
				pValueKeyCurrent, *pnTypeTagKeyCurrent,
				(LPINT) NULL, &nCompare );

  /* nIndexRight now contains the index of the element < oKeyCurrent */
  if ( nCompare == 0 ) {
    /* Element was found: */
    nIndexRight++;
    bMapItem	= (BOOL) ( eCompareCurrent == eshLessEqual ||
			   eCompareCurrent == eshGreaterEqual );
  }

  nItems	= OBJID2FIXNUM ( pPage->onCount );
  iIncr		= ( bDescending ) ? -1 : 1;
  if ( bDescending && nIndexRight >= nItems ) {
    nIndexRight	= nItems - 1;
  }

  for ( i = nIndexRight; ( bDescending ) ? i >= -1 : i < nItems; i += iIncr ) {
    if ( i < 0 ) {
      oNext	= pPage->oNext;
      pPageNext	= fnGetBTreePage ( oNext );
      nMapMode	= fnMap ( oHeap, pBTree, pPageNext, pnKeys,
			  poKeyLower, pnTypeTagKeyLower,
			  pValueKeyLower, eCompareLower,
			  poKeyUpper, pnTypeTagKeyUpper,
			  pValueKeyUpper, eCompareUpper,
			  bDescending, pfnMapItem, pUserData );
      if ( nMapMode != mapmodeContinue ) {
	RETURN ( nMapMode );
      }
    } else {
      oKey	= pPage->Items [ i ].oKey;
      oData	= pPage->Items [ i ].oData;
      oNext	= pPage->Items [ i ].oNext;
      pPageNext	= fnGetBTreePage ( oNext );
      if ( bDescending ) {
	nMapMode	= fnMap ( oHeap, pBTree, pPageNext, pnKeys,
				  poKeyLower, pnTypeTagKeyLower,
				  pValueKeyLower, eCompareLower,
				  poKeyUpper, pnTypeTagKeyUpper,
				  pValueKeyUpper, eCompareUpper,
				  bDescending, pfnMapItem, pUserData );
	if ( nMapMode != mapmodeContinue ) {
	  RETURN ( nMapMode );
	}
      }
      if ( ! bMapItem ) {
	/* Check if current key is >= resp. > the start key: */
	nCompare	=
	  fnKeyCmp ( pBTree, pValueKeyCurrent, *pnTypeTagKeyCurrent, oKey );
	bMapItem	= (BOOL)
	  ( bDescending ) ?
	  ( ( eCompareUpper == eshLessEqual ) ?
	    ( nCompare >= 0 ) : ( nCompare > 0 ) ) :
	  ( ( eCompareLower == eshGreaterEqual ) ?
	    ( nCompare <= 0 ) : ( nCompare < 0 ) );
      }
      if ( bMapItem ) {
	/* Check if current key is == resp. >=  the last key: */
	nCompare	= fnKeyCmp ( pBTree, pValueKeyLast, nTypeTagKeyLast,
				     oKey );
	if ( ( bDescending ) ?
	     ( ( eCompareLower == eshGreaterEqual ) ?
	       ( nCompare > 0 ) : ( nCompare >= 0 ) ) :
	     ( ( eCompareUpper == eshLessEqual ) ?
	       ( nCompare < 0 ) : ( nCompare <= 0 ) ) ) {
	  RETURN ( mapmodeFinished );
	}
	if ( pnKeys != NULL ) {
	  (*pnKeys)++;
	}
	onTimeStamp	= pBTree->onTimeStamp;
	if ( ! ( * pfnMapItem ) ( pUserData, pBTree->oSelf,
				  oKey, oData, pPage->oSelf, i ) ) {
	  RETURN ( mapmodeFinished );
	}
	if ( onTimeStamp != pBTree->onTimeStamp ) {
	  /* The Btree has been changed during the iteration; do a
             restart. */
	  ASSERT ( poKeyCurrent != NULL );
	  *poKeyCurrent		= oKey;
	  ASSERT ( pnTypeTagKeyCurrent != NULL );
	  *pnTypeTagKeyCurrent	= typetagof ( oKey );
	  RETURN ( mapmodeRestart );
	}
      }
      if ( ! bDescending ) {
	nMapMode	= fnMap ( oHeap, pBTree, pPageNext, pnKeys,
				  poKeyLower, pnTypeTagKeyLower,
				  pValueKeyLower, eCompareLower,
				  poKeyUpper, pnTypeTagKeyUpper,
				  pValueKeyUpper, eCompareUpper,
				  bDescending, pfnMapItem, pUserData );
	if ( nMapMode != mapmodeContinue ) {
	  RETURN ( nMapMode );
	}
      }
    }
  }
  RETURN ( nMapMode );
} /* fnMap */

/* ----------------------------------------------------------------------- */
static BOOL		fnEchoBTreePage	( LPVOID	lpUserData,
					  OBJID		oBTree,
					  OBJID		oKey,
					  OBJID		oData,
					  OBJID		oBTreePage,
					  int		nIndex )
{
  PROCEDURE	( fnEchoBTreePage );

  ((LPBTREEMAPPER)lpUserData)->oBTreePage	= oBTreePage;
  ((LPBTREEMAPPER)lpUserData)->onIndex		= Fixnum2ObjId ( nIndex );
  ((LPBTREEMAPPER)lpUserData)->oKey		= oKey;
  ((LPBTREEMAPPER)lpUserData)->oData		= oData;

  RETURN ( (BOOL) FALSE );
} /* fnEchoBTreePage */

/* ----------------------------------------------------------------------- */
static BOOL		fnSearchBTreePage	( OBJID		oHeap,
						  LPPLOBBTREE	lpBTree,
						  LPCVOID	pValueKeyLower,
						  SHTYPETAG nTypeTagKeyLower,
						  COMPARETAG	eCompareLower,
						  LPCVOID	pValueKeyUpper,
						  SHTYPETAG nTypeTagKeyUpper,
						  COMPARETAG	eCompareUpper,
						  BOOL		bDescending,
						  LPBTREEMAPPER	lpMapper )
{
  int		nKeys		= 0;
  LPBTREEPAGE	lpPage;

  PROCEDURE	( fnSearchBTreePage );

  lpPage	= fnGetBTreePage ( lpBTree->oRoot );
  fnMap ( oHeap, lpBTree, lpPage, &nKeys,
	  NULL, &nTypeTagKeyLower, pValueKeyLower, eCompareLower,
	  NULL, &nTypeTagKeyUpper, pValueKeyUpper, eCompareUpper,
	  bDescending, fnEchoBTreePage, lpMapper );

  RETURN ( (BOOL) ( nKeys == 1 ) );
} /* fnSearchBTreePage */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT	fnBTreeClear	( OBJID oHeap,
					  OBJID oBTree )
{
  SHLOCK	nLockOld;
  LPPLOBBTREE	lpBTree;
  BTREERESULT	nResult;
  LPBTREEPAGE	lpPage;

  PROCEDURE	( fnBTreeClear );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpBTree	= fnGetBTree ( oHeap, oBTree, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (BTREERESULT) nLockOld );
  }
  /* If this ASSERT fails, there was maybe no active transaction on
     oHeap: */
  ASSERT ( lpBTree != NULL );
  if ( boundp ( lpBTree->oRoot ) ) {
    nResult	= btreeDeleted;
    nLockOld	= fnLockBTree ( oHeap, lpBTree );
    if ( (int) nLockOld < 0 ) {
      RETURN ( (BTREERESULT) nLockOld );
    }
    lpPage	= fnGetBTreePage ( lpBTree->oRoot );
    fnReleaseAllPages ( oHeap, lpPage );
    makunbound ( lpBTree->oRoot );
    lpBTree->onCount		= o0;
    lpBTree->onPages		= o0;
  } else {
    nResult			= btreeNotFound;
  }
  if ( oBTreeCountCache == oBTree )
    nBTreeCountCache		= 0;
  if ( oBTreeSearchByObjIdCache == oBTree )
    oBTreeSearchByObjIdCache	= NULLOBJID;
  RETURN ( nResult );
} /* fnBTreeClear */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT	fnBTreeDelete	( OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKey,
					  SHTYPETAG	nTypeTagKey )
{
  SHLOCK	nLockOld;
  OBJID		oUnderflow, oNext;
  LPPLOBBTREE	lpBTree;
  int		nPageSize = 0, nPageCount = 0;
  BTREERESULT	nResult;
  BOOL		bUnderflow;
  LPBTREEPAGE	lpPage;

  PROCEDURE	( fnBTreeDelete );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpBTree	= fnGetBTree ( oHeap, oBTree, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (BTREERESULT) nLockOld );
  }
  /* If this ASSERT fails, there was maybe no active transaction on
     oHeap: */
  ASSERT ( lpBTree != NULL );
  nPageSize	= ObjId2Fixnum ( lpBTree->onPageSize );
  makendmarker ( oUnderflow );
  bUnderflow	= (BOOL) FALSE;
  lpPage	= fnGetBTreePage ( lpBTree->oRoot );
  nResult	= fnDelete ( oHeap, lpBTree, lpPage, pValueKey, nTypeTagKey,
			     &oUnderflow, &bUnderflow );
  if ( (int) nResult < 0 ) {
    RETURN ( (BTREERESULT) nResult );
  }
  if ( bUnderflow && OBJID2FIXNUM ( lpPage->onCount ) <= 0 ) {
    /* Groesse der Wurzelseite wurde reduziert: */
    nLockOld	= fnLockBTree ( oHeap, lpBTree );
    if ( (int) nLockOld < 0 ) {
      RETURN ( (BTREERESULT) nLockOld );
    }
    lpBTree->oRoot		= lpPage->oNext;
    if ( boundp ( lpBTree->oRoot ) ) {
      /* Mark the new root page: */
      lpPage			= fnGetBTreePage ( lpBTree->oRoot );
      lpPage->oParentPage	= oBTree;
      makunbound ( lpPage->oParentIndex );
    }
  }
  /* Traverse the oUnderflow-list and free underflow'ed pages: */
  while ( ! endmarkerp ( oUnderflow ) ) {
    lpPage	= fnGetBTreePage ( oUnderflow );
    ASSERT ( lpPage != NULL );
    oNext	= lpPage->oUnderflow;
    makunbound ( lpPage->oUnderflow );
    nPageCount	= ObjId2Fixnum ( lpPage->onCount );
    if ( oUnderflow != lpBTree->oRoot && nPageCount < nPageSize / 2  ) {
      fnUnlock ( oHeap, (SHLOCK)
		 ( (unsigned int) eshLockVectorRead |
		   (unsigned int) eshLockForce ),
		 oUnderflow, -1, (SHLOCK *) NULL );
      fnUnlock ( oHeap, (SHLOCK)
		 ( (unsigned int) eshLockVectorWrite |
		   (unsigned int) eshLockForce ),
		 oUnderflow, -1, (SHLOCK *) NULL );
#if defined(LOGGING)
      fprintf ( stderr, "%s(%d): Deleting underflow'ed page %s.\n",
	        __szProc__, __LINE__,
	        fnPrintObject ( oUnderflow, (LPSTR) NULL, 0 ) );
#endif /* #if defined(LOGGING) */
      fnDestroyObject ( oUnderflow, FALSE );
      INCOBJID ( lpBTree->onPages, -1 );
#if defined(LOGGING)
    } else {
      fprintf ( stderr, "%s(%d): Found touched page %s.\n",
	        __szProc__, __LINE__,
	        fnPrintObject ( oUnderflow, (LPSTR) NULL, 0 ) );
#endif /* #if defined(LOGGING) */
    }
    oUnderflow	= oNext;
  }
  if ( nResult == btreeDeleted ) {
    nLockOld			= fnLockBTree ( oHeap, lpBTree );
    if ( (int) nLockOld < 0 ) {
      RETURN ( (BTREERESULT) nLockOld );
    }
    INCOBJID ( lpBTree->onCount, -1 );
    INCOBJID ( lpBTree->onTimeStamp, 1 );
  }
  if ( oBTreeCountCache == oBTree ) {
    nBTreeCountCache		= OBJID2FIXNUM ( lpBTree->onCount );
  }
  if ( oBTreeSearchByObjIdCache == oBTree ) {
    oBTreeSearchByObjIdCache	= NULLOBJID;
  }
  RETURN ( nResult );
} /* fnBTreeDelete */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT fnBTreeDeleteByObjId	( OBJID oHeap,
						  OBJID oBTree,
						  OBJID oKey )
{
  PROCEDURE	( fnBTreeDeleteByObjId );

  RETURN ( fnBTreeDelete ( oHeap, oBTree, &oKey, typetagof ( oKey ) ) );
} /* fnBTreeDelete */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT	fnBTreeInsert	( OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKey,
					  SHTYPETAG	nTypeTagKey,
					  OBJID		oData )
{
  SHLOCK	nLockOld;
  OBJID		oRootOld;
  LPPLOBBTREE	lpBTree;
  BTREERESULT	nResult;
  BOOL		bInsert;
  BTREEITEM	Move;
  LPBTREEPAGE	lpRoot;

  PROCEDURE	( fnBTreeInsert );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpBTree	= fnGetBTree ( oHeap, oBTree, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (BTREERESULT) nLockOld );
  }

  /* If this ASSERT fails, there was maybe no active transaction on
     oHeap: */
  ASSERT ( lpBTree != NULL );

  bInsert	= (BOOL) FALSE;
  lpRoot	= fnGetBTreePage ( lpBTree->oRoot );
  nResult	= fnSearch ( oHeap, lpBTree, lpRoot,
			     pValueKey, nTypeTagKey,
			     (LPOBJID) NULL, (LPOBJID) NULL, &oData,
			     &bInsert, &Move );
  if ( (int) nResult < 0 ) {
    RETURN ( (BTREERESULT) nResult );
  }

  if ( bInsert ) {
    /* Fuege neue Wurzelseite ein: */
    nLockOld		= fnLockBTree ( oHeap, lpBTree );
    if ( (int) nLockOld < 0 ) {
      RETURN ( (BTREERESULT) nLockOld );
    }
    oRootOld		= lpBTree->oRoot;
    lpBTree->oRoot	= fnCreateBTreePage ( lpBTree );
    INCOBJID ( lpBTree->onPages, 1 );
    lpRoot		= fnGetBTreePage ( lpBTree->oRoot );
    lpRoot->onCount	= o1;
    lpRoot->oParentPage	= oBTree;
    fnSetPageNext ( lpRoot, -1, oRootOld );
    lpRoot->Items [ 0 ]	= Move;
    fnSetPageNext ( lpRoot, 0, lpRoot->Items [ 0 ].oNext );
  }

  if ( nResult == btreeInserted ) {
    INCOBJID ( lpBTree->onCount, 1 );
    INCOBJID ( lpBTree->onTimeStamp, 1 );
  }

  if ( oBTreeCountCache == oBTree ) {
    nBTreeCountCache		= OBJID2FIXNUM ( lpBTree->onCount );
  }

  if ( oBTreeSearchByObjIdCache == oBTree ) {
    oBTreeSearchByObjIdCache	= NULLOBJID;
  }
  RETURN ( nResult );
} /* fnBTreeInsert */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT	fnBTreeInsertByObjId	( OBJID oHeap,
						  OBJID oBTree,
						  OBJID oKey,
						  OBJID oData )
{
  PROCEDURE	( fnBTreeInsertByObjId );

  RETURN ( fnBTreeInsert ( oHeap, oBTree, &oKey, typetagof ( oKey ), oData ) );
} /* fnBTreeInsertByObjId */

/* ----------------------------------------------------------------------- */
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
					  LPVOID	lpUserData )
{
  OBJID		oKeyLower = NULLOBJID, oKeyUpper = NULLOBJID;
  OBJID		oTypeTagKeyLower = NULLOBJID, oTypeTagKeyUpper = NULLOBJID;
  FIXNUM	nValueKeyLower, nValueKeyUpper;
  LPCVOID	pKeyLower, pKeyUpper;
  LPPLOBBTREE	lpBTree;
  int		nMapped, nKeys;
  SHLOCK	nLockOld;
  LPBTREEPAGE	lpPage;

  PROCEDURE	( fnBTreeMap );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  ASSERT ( lpfnMapItem != NULL );

  switch ( eCompareLower ) {
  case eshGreater: case eshGreaterEqual:
    break;
  default:
    ERROR (( szIllegalCompare, szLower,
	     fnCompareTag2String ( eCompareLower, FALSE ),
	     (int) eCompareLower,
	     fnCompareTag2String ( eshGreater, FALSE ), (int) eshGreater,
	     fnCompareTag2String ( eshGreaterEqual, FALSE ),
	     (int) eshGreaterEqual ));
    RETURN ( (int) eshGeneralError );
  }
  switch ( eCompareUpper ) {
  case eshLess: case eshLessEqual:
    break;
  default:
    ERROR (( szIllegalCompare, szUpper,
	     fnCompareTag2String ( eCompareUpper, FALSE ),
	     (int) eCompareLower,
	     fnCompareTag2String ( eshLess, FALSE ), (int) eshLess,
	     fnCompareTag2String ( eshLessEqual, FALSE ),
	     (int) eshLessEqual ));
    RETURN ( (int) eshGeneralError );
  }

  nKeys		= 0;
  lpBTree	= fnGetBTree ( oHeap, oBTree, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    RETURN ( nLockOld );
  }
  /* If this ASSERT fails, there was maybe no active transaction on
     oHeap: */
  ASSERT ( lpBTree != NULL );

  lpPage	= fnGetBTreePage ( lpBTree->oRoot );

  fnBTreeMapStoreKey ( pValueKeyLower, nTypeTagKeyLower, &oKeyLower,
		       &oTypeTagKeyLower, NULL );
  nTypeTagKeyLower	= ObjId2TypeTag ( oTypeTagKeyLower );
  pKeyLower		=
    fnBtreeMapGetKeyPtr ( &oKeyLower, oTypeTagKeyLower, pValueKeyLower );

  fnBTreeMapStoreKey ( pValueKeyUpper, nTypeTagKeyUpper, &oKeyUpper,
		       &oTypeTagKeyUpper, NULL );
  nTypeTagKeyUpper	= ObjId2TypeTag ( oTypeTagKeyUpper );
  pKeyUpper		=
    fnBtreeMapGetKeyPtr ( &oKeyUpper, oTypeTagKeyUpper, pValueKeyUpper );
  
#if defined(LOGGING)
  {
    char	szLower [ 256 ], szUpper [ 256 ];
    fprintf ( stderr,
	      "%s(%d): Start from key %s, type tag 0x%X\n"
	      "\t\tto %s, type tag 0x%X\n",
	      __szProc__, __LINE__,
	      PrintObject ( * (LPOBJID) pKeyLower, szLower ),
	      nTypeTagKeyLower,
	      PrintObject ( * (LPOBJID) pKeyUpper, szUpper ),
	      nTypeTagKeyUpper );
    fflush ( stderr );
  }
#endif /* #if defined(LOGGING) */

  while ( nMapped = fnMap ( oHeap, lpBTree, lpPage, &nKeys,
			    &oKeyLower, &nTypeTagKeyLower,
			    pKeyLower, eCompareLower,
			    &oKeyUpper, &nTypeTagKeyUpper,
			    pKeyUpper, eCompareUpper,
			    bDescending, lpfnMapItem,
			    lpUserData ) == mapmodeRestart ) {
#if defined(LOGGING)
    {
      char	szLower [ 256 ], szUpper [ 256 ];
      fprintf ( stderr,
		"%s(%d): Restart from key %s, type tag 0x%X\n"
		"\t\tto %s, type tag 0x%X\n",
		__szProc__, __LINE__,
		PrintObject ( * (LPOBJID) pKeyLower, szLower ),
		nTypeTagKeyLower,
		PrintObject ( * (LPOBJID) pKeyUpper, szUpper ),
		nTypeTagKeyUpper );
      fflush ( stderr );
    }
#endif /* #if defined(LOGGING) */
    if ( bDescending ) {
      eCompareUpper	= eshLess;
    } else {
      eCompareLower	= eshGreater;
    }
    /* Re-get the root page, since the btree might have been reorderd
       in the meantime. */
    lpPage	= fnGetBTreePage ( lpBTree->oRoot );
    pKeyLower	= &oKeyLower;
    pKeyUpper	= &oKeyUpper;
  }
  RETURN ( ( nMapped < 0 ) ? nMapped : nKeys );
} /* fnBTreeMap */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnBTreeMapAll		( OBJID		oHeap,
					  OBJID		oBTree,
					  LPFNMAPBTREE	pfnMapItem,
					  LPVOID	pUserData )
{
  OBJID	oMin	= TypeTag2ObjId ( eshMinTag );
  OBJID	oMax	= TypeTag2ObjId ( eshMaxTag );

  RETURN ( fnBTreeMap ( oHeap, oBTree,
			&oMin, eshMinTag, eshGreaterEqual,
			&oMax, eshMaxTag, eshLessEqual,
			FALSE, pfnMapItem, pUserData ) );
} /* fnBTreeMapAll */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnBTreeMapByObjId	( OBJID		oHeap,
					  OBJID		oBTree,
					  OBJID		oKeyLower,
					  COMPARETAG	eCompareLower,
					  OBJID		oKeyUpper,
					  COMPARETAG	eCompareUpper,
					  BOOL		bDescending,
					  LPFNMAPBTREE	lpfnMapItem,
					  LPVOID	lpUserData )
{
  PROCEDURE	( fnBTreeMapByObjId );
  RETURN ( fnBTreeMap ( oHeap, oBTree,
			&oKeyLower, typetagof ( oKeyLower ), eCompareLower,
			&oKeyUpper, typetagof ( oKeyUpper ), eCompareUpper,
			bDescending, lpfnMapItem, lpUserData ) );
} /* fnBTreeMapByObjId */

/* ----------------------------------------------------------------------- */
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
					  LPOBJID	lpoData )
{
  SHTYPETAG	nTypeTagKeyLower, nTypeTagKeyUpper;

  PROCEDURE	( fnBTreeMapFirstByObjId );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  nTypeTagKeyLower	= typetagof ( oKeyLower );
  nTypeTagKeyUpper	= typetagof ( oKeyUpper );

  RETURN ( fnBTreeMapFirst ( lpoMapper, oHeap, oBTree,
			     &oKeyLower, nTypeTagKeyLower, eCompareLower,
			     &oKeyUpper, nTypeTagKeyUpper, eCompareUpper,
			     bDescending, nMap, lpoKey, lpoData ) );
} /* fnBTreeMapFirstByObjId */

/* ----------------------------------------------------------------------- */
static void	fnBTreeMapStoreKey	( LPCVOID	pValueKey,
					  SHTYPETAG	nTypeTagKey,
					  LPOBJID	poKey,
					  LPOBJID	ponTypeTagKey,
					  LPVOID	*ppBuffer )
{
  OBJID		oKey = NULLOBJID, oTypeTagKey = NULLOBJID;
  LPVOID	pBuffer = NULL;
  LPCLASSINFO	lpClassInfo = NULL;
  unsigned int	nBytes = 0;

  PROCEDURE	( fnBTreeMapStoreKey );

  fnBTreeMapUnstoreKey ( poKey, ponTypeTagKey, ppBuffer );

  if ( nTypeTagKey == eshDynCStringPtrTag ) {
    if ( ppBuffer != NULL ) {
      nBytes	= strlen ( (LPSTR) pValueKey ) + 1;
      pBuffer	= Malloc ( nBytes );
      ASSERT ( pBuffer != NULL );
      strcpy ( (LPSTR) pBuffer, (LPSTR) pValueKey );
    }
  } else {
    lpClassInfo	= (LPCLASSINFO) FindClassInfo ( nTypeTagKey );
    /* If this assert fails, an unknown type tag was passed
       for the end key: */
    ASSERT ( lpClassInfo != NULL );
    if ( ( lpClassInfo->nTypeFlags & typeTransientP ) != 0 ) {
      nBytes	=
	( lpClassInfo->nFixSizeValue + nBitsPerByte - 1 ) / nBitsPerByte;
      pBuffer	= Malloc ( nBytes );
      ASSERT ( pBuffer != NULL );
      memcpy ( pBuffer, pValueKey, nBytes );
    } else {
      oKey	= fnImmediate2ObjId ( * (FIXNUM *) pValueKey, &nTypeTagKey );
    }
  }
  oTypeTagKey	= TypeTag2ObjId ( nTypeTagKey );

  if ( poKey != NULL ) {
    *poKey		= oKey;
  }
  if ( ponTypeTagKey != NULL ) {
    *ponTypeTagKey	= oTypeTagKey;
  }
  if ( ppBuffer != NULL ) {
    *ppBuffer		= pBuffer;
  }

  RETURN ( VOID );
} /* fnBTreeMapStoreKey */

/* ----------------------------------------------------------------------- */
static LPCVOID	fnBtreeMapGetKeyPtr	( LPOBJID	poKey,
					  OBJID		onTypeTagKey,
					  LPCVOID	pBuffer )
{
  LPCVOID	pValueKey = NULL;
  LPCLASSINFO	pClassInfo = NULL;

  PROCEDURE	( fnBtreeMapGetKeyPtr );

  pClassInfo	= (LPCLASSINFO)
    FindClassInfo ( ObjId2TypeTag ( onTypeTagKey ) );

  pValueKey	=
    ( ( pClassInfo->nTypeFlags & typeTransientP ) != 0 ) ?
    pBuffer : (LPVOID) poKey;

  RETURN ( pValueKey );
} /* fnBtreeMapGetKeyPtr */

/* ----------------------------------------------------------------------- */
static void	fnBTreeMapUnstoreKey	( LPOBJID	poKey,
					  LPOBJID	ponTypeTagKey,
					  LPVOID	*ppBuffer )
{
  PROCEDURE	( fnBTreeMapUnstoreKey );

  if ( poKey != NULL ) {
    makunbound ( *poKey );
  }
  if ( ponTypeTagKey != NULL ) {
    makunbound ( *ponTypeTagKey );
  }
  if ( ppBuffer != NULL && *ppBuffer != NULL ) {
    Free ( *ppBuffer );
    ppBuffer	= NULL;
  }

  RETURN ( VOID );
} /* fnBTreeMapUnstoreKey */

/* ----------------------------------------------------------------------- */
typedef enum {
  locateFailed,		/* Relocation failed: Key was not found in btree */
  locateUpToDate,	/* The location is up-to-date, no positioning */
  locateToStart,	/* Positioned to start key */
  locateToCurrent,	/* Positioned to current key */
  locateToNext		/* Positioned to next key */
}	MAPPERLOCATE, * PMAPPERLOCATE;

static MAPPERLOCATE	fnBTreeMapLocate	( LPBTREEMAPPER	pMapper,
						  int		nIncrement,
						  BOOL		bForce )
{
  MAPPERLOCATE	eLocate = locateUpToDate;
  LPCVOID	pValueKeyLower, pValueKeyUpper;
  SHTYPETAG	nTypeTagKeyLower, nTypeTagKeyUpper;
  COMPARETAG	eCompareLower, eCompareUpper;
  SHLOCK	nLockOld;
  LPPLOBBTREE	pBTree = (LPPLOBBTREE) NULL;
  LPBTREEPAGE	pPage = (LPBTREEPAGE) NULL;
  int		nIndex;
  BTREEMAPPER	Mapper;
  BOOL		bFound;

  PROCEDURE	( fnBTreeMapLocate );
  ASSERT ( pMapper != NULL );

  if ( ! boundp ( pMapper->oBTree ) ) {
    char	szMapper [ 128 ];
    ERROR (( "Object %s references no BTree.",
	     PrintObject ( pMapper->oSelf, szMapper ) ));
    RETURN ( (FIXNUM) eshGeneralError );
  }
					
  if ( ! bForce ) {
    pBTree	= fnGetBTree ( pMapper->oHeap, pMapper->oBTree, &nLockOld );
    if ( (int) nLockOld < 0 ) {
      RETURN ( (MAPPERLOCATE) nLockOld );
    }
    /* If this ASSERT fails, there was maybe no active transaction on
       oHeap: */
    ASSERT ( pBTree != NULL );
    if ( pMapper->onTimeStamp != pBTree->onTimeStamp ) {
      bForce	= TRUE;
    }
  }

  if ( ! bForce && boundp ( pMapper->oBTreePage ) &&
       boundp ( pMapper->onIndex ) ) {
    pPage	= fnGetBTreePage ( pMapper->oBTreePage );
    ASSERT ( pPage != NULL );
    nIndex	= ObjId2Fixnum ( pMapper->onIndex );
    if ( pPage->Items [ nIndex ].oKey != pMapper->oKey ) {
      bForce	= TRUE;
    } else if ( pPage->Items [ nIndex ].oData != pMapper->oData ) {
      nLockOld		= fnLockBTreeMapper ( pMapper->oHeap, pMapper );
      if ( (int) nLockOld < 0 ) {
	RETURN ( (FIXNUM) nLockOld );
      }
      pMapper->oData	= pPage->Items [ nIndex ].oData;
    }
  }

  if ( ! bForce && boundp ( pMapper->oBTreePage ) &&
       boundp ( pMapper->onIndex ) ) {
    RETURN ( eLocate );
  }

  /* Initialize to start and end key: */
  nTypeTagKeyLower	= (SHTYPETAG)
    ObjId2TypeTag ( pMapper->onTypeTagKeyLower );
  pValueKeyLower	= fnBtreeMapGetKeyPtr ( &pMapper->oKeyLower,
						pMapper->onTypeTagKeyLower,
						pMapper->pValueKeyLower );
  ASSERT ( pValueKeyLower != NULL );
  eCompareLower		= (COMPARETAG)
    ObjId2Fixnum ( pMapper->onCompareLower ); 

  nTypeTagKeyUpper	= (SHTYPETAG)
    ObjId2TypeTag ( pMapper->onTypeTagKeyUpper );
  pValueKeyUpper	= fnBtreeMapGetKeyPtr ( &pMapper->oKeyUpper,
						pMapper->onTypeTagKeyUpper,
						pMapper->pValueKeyUpper );
  ASSERT ( pValueKeyUpper != NULL );
  eCompareUpper		= (COMPARETAG)
    ObjId2Fixnum ( pMapper->onCompareUpper ); 

  if ( boundp ( pMapper->oKey ) ) {

    /* Use current key: */
    switch ( nIncrement ) {

    case -1:
      /* Descending & position to next element: */
      nTypeTagKeyUpper	= typetagof ( pMapper->oKey );
      pValueKeyUpper	= &pMapper->oKey;
      eCompareUpper	= eshLess;
      eLocate		= locateToNext;
      break;

    case 1:
      /* Ascending & position to next element: */
      nTypeTagKeyLower	= typetagof ( pMapper->oKey );
      pValueKeyLower	= &pMapper->oKey;
      eCompareLower	= eshGreater;
      eLocate		= locateToNext;
      break;

    default:
      /* Position to current key: */
      nTypeTagKeyLower	= typetagof ( pMapper->oKey );
      pValueKeyLower	= &pMapper->oKey;
      eCompareLower	= eshGreaterEqual;
      nTypeTagKeyUpper	= nTypeTagKeyLower;
      pValueKeyUpper	= pValueKeyLower;
      eCompareUpper	= eshLessEqual;
      eLocate		= locateToCurrent;
      break;
    }

  } else {
    eLocate	= locateToStart;
  }

  if ( pBTree == NULL ) {
    pBTree	= fnGetBTree ( pMapper->oHeap, pMapper->oBTree, &nLockOld );
    if ( (int) nLockOld < 0 ) {
      RETURN ( (MAPPERLOCATE) nLockOld );
    }
    /* If this ASSERT fails, there was maybe no active transaction on
       oHeap: */
    ASSERT ( pBTree != NULL );
  }
  memset ( &Mapper, 0, sizeof ( Mapper ) );
  bFound	=
    fnSearchBTreePage ( pMapper->oHeap, pBTree,
			pValueKeyLower, nTypeTagKeyLower, eCompareLower,
			pValueKeyUpper, nTypeTagKeyUpper, eCompareUpper,
			(BOOL) ( nIncrement < 0 ), &Mapper );
  if ( (int) bFound < 0 ) {
    RETURN ( (MAPPERLOCATE) bFound );
  }

  nLockOld		= fnLockBTreeMapper ( pMapper->oHeap, pMapper );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (FIXNUM) nLockOld );
  }
  if ( bFound ) {
    pMapper->onTimeStamp	= pBTree->onTimeStamp;
    pMapper->oBTreePage		= Mapper.oBTreePage;
    pMapper->onIndex		= Mapper.onIndex;
    pMapper->oKey		= Mapper.oKey;
    pMapper->oData		= Mapper.oData;
  } else {
    makunbound ( pMapper->oBTreePage );
    makunbound ( pMapper->onIndex );
    eLocate			= locateFailed;
  }

  RETURN ( eLocate );
} /* fnBTreeMapLocate */

/* ----------------------------------------------------------------------- */
static BOOL	fnBTreeMapDescending	( OBJID		oBTree,
					  LPBTREEPAGE	*ppPage,
					  LPINT		pnIndex )
{
  OBJID		oNext, oChild;

  PROCEDURE	( fnBTreeMapDescending );

  /* Continue iteration over the current page: */
  (*pnIndex)--;
  oNext	= ( *pnIndex < 0 ) ?
    (*ppPage)->oNext : (*ppPage)->Items [ *pnIndex ].oNext;

  if ( boundp ( oNext ) ) {
    /* Descend the BTree until a leaf page is reached: */
    do {
      *ppPage	= fnGetBTreePage ( oNext );
      ASSERT ( *ppPage != NULL );
      *pnIndex	= ObjId2Fixnum ( (*ppPage)->onCount ) - 1;
      oNext	= (*ppPage)->Items [ *pnIndex ].oNext;
    } while ( boundp ( oNext ) );
    /* Now, *ppPage points to a leaf page. */
  } else if ( *pnIndex < 0 ) {
    oChild	= (*ppPage)->oSelf;
    oNext	= (*ppPage)->oParentPage;
    /* Ascend until a not completely iterated parent page is
       reached: */
    while ( boundp ( oNext ) && oNext != oBTree ) {
      *pnIndex	= ObjId2Fixnum ( (*ppPage)->oParentIndex );
      *ppPage	= fnGetBTreePage ( oNext );
      ASSERT ( *ppPage != NULL );
      /* Check if the backward reference points to the pages
	 expected: */
      if ( *pnIndex < 0 ) {
	ASSERT ( (*ppPage)->oNext == oChild );
      } else {
	ASSERT ( (*ppPage)->Items [ *pnIndex ].oNext == oChild );
	break;
      }
      oChild	= oNext;
      oNext	= (*ppPage)->oParentPage;
    }
      if ( boundp ( oNext ) && oNext != oBTree ) {
	*ppPage	= fnGetBTreePage ( oNext );
	ASSERT ( *ppPage != NULL );
      } else {
	/* Finished with iteration, reached end of the btree: */
	*ppPage	= (LPBTREEPAGE) NULL;
      }
  }

  RETURN ( (BOOL) ( *ppPage != NULL ) );
} /* fnBTreeMapDescending */

/* ----------------------------------------------------------------------- */
static BOOL	fnBTreeMapAscending	( OBJID		oBTree,
					  LPBTREEPAGE	*ppPage,
					  LPINT		pnIndex )
{
  OBJID		oNext, oChild;
  int		nCount;

  PROCEDURE	( fnBTreeMapAscending );

  oNext	= (*ppPage)->Items [ *pnIndex ].oNext;
  if ( boundp ( oNext ) ) {
    /* Descend the BTree until a leaf page is reached: */
    do {
      *ppPage	= fnGetBTreePage ( oNext );
      ASSERT ( *ppPage != NULL );
      oNext	= (*ppPage)->oNext;
    } while ( boundp ( oNext ) );
    /* Now, *ppPage points to a leaf page. */
    *pnIndex		= 0;
  } else {
    /* Continue iteration over the current page: */
    (*pnIndex)++;
    nCount	= ObjId2Fixnum ( (*ppPage)->onCount );
    if ( *pnIndex >= nCount ) {
      /* Finished with this page, go on with the parent page: */
      oChild	= (*ppPage)->oSelf;
      oNext	= (*ppPage)->oParentPage;
      /* Ascend until a not completely iterated parent page is
	 reached: */
      while ( boundp ( oNext ) && oNext != oBTree ) {
	*pnIndex	= ObjId2Fixnum ( (*ppPage)->oParentIndex );
	*ppPage		= fnGetBTreePage ( oNext );
	ASSERT ( *ppPage != NULL );
	/* Check if the backward reference points to the pages
	   expected: */
	if ( *pnIndex < 0 ) {
	  ASSERT ( (*ppPage)->oNext == oChild );
	} else {
	  ASSERT ( (*ppPage)->Items [ *pnIndex ].oNext == oChild );
	}
	(*pnIndex)++;
	nCount	= ObjId2Fixnum ( (*ppPage)->onCount );
	if ( *pnIndex < nCount ) {
	  break;
	}
	oChild	= oNext;
	oNext	= (*ppPage)->oParentPage;
      }
      if ( boundp ( oNext ) && oNext != oBTree ) {
	*ppPage	= fnGetBTreePage ( oNext );
	ASSERT ( *ppPage != NULL );
      } else {
	/* Finished with iteration, reached end of the btree: */
	*ppPage	= (LPBTREEPAGE) NULL;
      }
    }
  }

  RETURN ( (BOOL) ( *ppPage != NULL ) );
} /* fnBTreeMapAscending */

/* ----------------------------------------------------------------------- */
static BOOL	fnBTreeMapIncrement	( LPBTREEMAPPER	pMapper,
					  int		nIncrement,
					  int		nIncrementIfStarted )
{
  BOOL		bDone = TRUE;
  LPPLOBBTREE	pBTree;
  LPBTREEPAGE	pPage = (LPBTREEPAGE) NULL;
  MAPPERLOCATE	eLocate;
  SHLOCK	nLockOld;
  LPCVOID	pValueKeyLast;
  OBJID		oKeyCurrent;
  SHTYPETAG	nTypeTagKeyLast;
  COMPARETAG	eCompareLast;
  int		nIndex, nCompare;

  PROCEDURE	( fnBTreeMapIncrement );
  ASSERT ( -1 <= nIncrement && nIncrement <= 1 );
  ASSERT ( -1 <= nIncrementIfStarted && nIncrementIfStarted <= 1 );

  eLocate	= fnBTreeMapLocate ( pMapper, nIncrement, FALSE );
  switch ( eLocate ) {

  case locateFailed:
    RETURN ( (BOOL) eshKeyNotFound );
    break;

  case locateToStart:
    nIncrement	= nIncrementIfStarted;
  case locateUpToDate:
  case locateToCurrent:
    pPage	= fnGetBTreePage ( pMapper->oBTreePage );
    ASSERT ( pPage != NULL );
    nIndex	= ObjId2Fixnum ( pMapper->onIndex );
    switch ( nIncrement ) {

    case -1: /* Descending */
      pValueKeyLast	=  fnBtreeMapGetKeyPtr ( &pMapper->oKeyLower,
						 pMapper->onTypeTagKeyLower,
						 pMapper->pValueKeyLower );
      ASSERT ( pValueKeyLast != NULL );
      nTypeTagKeyLast	= ObjId2TypeTag ( pMapper->onTypeTagKeyLower );
      eCompareLast	= (COMPARETAG)
	ObjId2Fixnum ( pMapper->onCompareLower );
      bDone		=
	fnBTreeMapDescending ( pMapper->oBTree, &pPage, &nIndex );
      break;

    case 1: /* Ascending */
      pValueKeyLast	=  fnBtreeMapGetKeyPtr ( &pMapper->oKeyUpper,
						 pMapper->onTypeTagKeyUpper,
						 pMapper->pValueKeyUpper );
      ASSERT ( pValueKeyLast != NULL );
      nTypeTagKeyLast	= ObjId2TypeTag ( pMapper->onTypeTagKeyUpper );
      eCompareLast	= (COMPARETAG)
	ObjId2Fixnum ( pMapper->onCompareUpper );
      bDone		=
	fnBTreeMapAscending ( pMapper->oBTree, &pPage, &nIndex );
      break;

    default:
      RETURN ( bDone );
      break;
    }
    if ( (int) bDone < 0 ) {
      RETURN ( bDone );
    }
    if ( pPage != NULL ) {
      pBTree	= fnGetBTree ( pMapper->oHeap, pMapper->oBTree, &nLockOld );
      if ( (int) nLockOld < 0 ) {
	RETURN ( nLockOld );
      }
      /* If this ASSERT fails, there was maybe no active transaction on
	 pMapper->oHeap: */
      ASSERT ( pBTree != NULL );
      /* Check if the last object is reached: */
      oKeyCurrent	= pPage->Items [ nIndex ].oKey;
      nCompare		=
	fnKeyCmp ( pBTree, pValueKeyLast, nTypeTagKeyLast, oKeyCurrent );
      if ( ( nIncrement < 0 ) ?
	   ( ( eCompareLast == eshGreaterEqual ) ?
	     ( nCompare > 0 ) : ( nCompare >= 0 ) ) :
	   ( ( eCompareLast == eshLessEqual ) ?
	     ( nCompare < 0 ) : ( nCompare <= 0 ) ) ) {
	bDone		= FALSE;
      } else {
	bDone		= TRUE;
	nLockOld	= fnLockBTreeMapper ( pMapper->oHeap, pMapper );
	if ( (int) nLockOld < 0 ) {
	  RETURN ( (FIXNUM) nLockOld );
	}
	pMapper->oBTreePage	= pPage->oSelf;
	pMapper->onIndex	= Fixnum2ObjId ( nIndex );
	pMapper->oKey		= oKeyCurrent;
	pMapper->oData		= pPage->Items [ nIndex ].oData;
      }
    }
    break;

  case locateToNext:
    bDone	= TRUE;
    break;

  default:
    bDone	= (BOOL) eLocate;
    break;
  }

  RETURN ( bDone );
} /* fnBTreeMapIncrement */

/* ----------------------------------------------------------------------- */
static BOOL	fnBTreeMapSeekSetOrEnd	( LPBTREEMAPPER	pMapper,
					  BOOL		bSeekEnd )
{
  BOOL		bDone = TRUE;
  SHLOCK	nLockOld;
  LPPLOBBTREE	pBTree = (LPPLOBBTREE) NULL;
  BTREEMAPPER	Mapper;
  LPVOID	pValueKeyLower, pValueKeyUpper;
  SHTYPETAG	nTypeTagKeyLower, nTypeTagKeyUpper;
  COMPARETAG	eCompareLower, eCompareUpper;

  PROCEDURE	( fnBTreeMapSeekSetOrEnd );

  pBTree	= fnGetBTree ( pMapper->oHeap, pMapper->oBTree, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (MAPPERLOCATE) nLockOld );
  }
  /* If this ASSERT fails, there was maybe no active transaction on
     oHeap: */
  ASSERT ( pBTree != NULL );

  /* Initialise to start and end key: */
  nTypeTagKeyLower	= (SHTYPETAG)
    ObjId2TypeTag ( pMapper->onTypeTagKeyLower );

  pValueKeyLower	= &pMapper->oKeyLower;

  eCompareLower		= (COMPARETAG)
    ObjId2Fixnum ( pMapper->onCompareLower ); 

  nTypeTagKeyUpper	= (SHTYPETAG)
    ObjId2TypeTag ( pMapper->onTypeTagKeyUpper );

  pValueKeyUpper	= &pMapper->oKeyUpper;

  eCompareUpper		= (COMPARETAG)
    ObjId2Fixnum ( pMapper->onCompareUpper );

  memset ( &Mapper, 0, sizeof ( Mapper ) );
  bDone	=
    fnSearchBTreePage ( pMapper->oHeap, pBTree,
			pValueKeyLower, nTypeTagKeyLower, eCompareLower,
			pValueKeyUpper, nTypeTagKeyUpper, eCompareUpper,
			bSeekEnd, &Mapper );
  if ( (int) bDone < 0 ) {
    RETURN ( bDone );
  }

  nLockOld		= fnLockBTreeMapper ( pMapper->oHeap, pMapper );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (FIXNUM) nLockOld );
  }
  if ( ! bDone ) {
    makunbound ( pMapper->oBTreePage );
    makunbound ( pMapper->onIndex );
    RETURN ( (BOOL) eshKeyNotFound );
  }

  pMapper->oBTreePage	= Mapper.oBTreePage;
  pMapper->onIndex	= Mapper.onIndex;
  pMapper->oKey		= Mapper.oKey;
  pMapper->oData	= Mapper.oData;

  RETURN ( bDone );
} /* fnBTreeMapSeekSetOrEnd */

/* ----------------------------------------------------------------------- */
FIXNUM DLLEXPORT fnBTreeMapSearch	( OBJID		oMapper,
					  OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKeyLower,
					  SHTYPETAG	nTypeTagKeyLower,
					  COMPARETAG	eCompareLower,
					  LPCVOID	pValueKeyUpper,
					  SHTYPETAG	nTypeTagKeyUpper,
					  COMPARETAG	eCompareUpper,
					  BOOL		bDescending )
{
  LPBTREEMAPPER	pMapper = NULL;
  SHLOCK	nLockOld;

  PROCEDURE	( fnBTreeMapSearch );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  ASSERT_ObjId_is_valid ( oMapper );

  switch ( eCompareLower ) {
  case eshGreater: case eshGreaterEqual:
    break;
  default:
    ERROR (( szIllegalCompare, szLower,
	     fnCompareTag2String ( eCompareLower, FALSE ),
	     (int) eCompareLower,
	     fnCompareTag2String ( eshGreater, FALSE ), (int) eshGreater,
	     fnCompareTag2String ( eshGreaterEqual, FALSE ),
	     (int) eshGreaterEqual ));
    RETURN ( (int) eshGeneralError );
  }
  switch ( eCompareUpper ) {
  case eshLess: case eshLessEqual:
    break;
  default:
    ERROR (( szIllegalCompare, szUpper,
	     fnCompareTag2String ( eCompareUpper, FALSE ),
	     (int) eCompareLower,
	     fnCompareTag2String ( eshLess, FALSE ), (int) eshLess,
	     fnCompareTag2String ( eshLessEqual, FALSE ),
	     (int) eshLessEqual ));
    RETURN ( (int) eshGeneralError );
  }

  pMapper	= fnGetBTreeMapper ( oHeap, oMapper, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    RETURN ( nLockOld );
  }
  ASSERT ( pMapper != NULL );

  if ( boundp ( oHeap ) ) {
    ASSERT_ObjId_is_valid ( oHeap );
    pMapper->oHeap		= oHeap;
  }

  nLockOld			= fnLockBTreeMapper ( oHeap, pMapper );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (FIXNUM) nLockOld );
  }

  if ( boundp ( oBTree ) ) {
    ASSERT_ObjId_is_valid ( oBTree );
    pMapper->oBTree		= oBTree;
  }

  fnBTreeMapStoreKey ( pValueKeyLower, nTypeTagKeyLower,
		       &pMapper->oKeyLower, &pMapper->onTypeTagKeyLower,
		       &pMapper->pValueKeyLower );
  pMapper->onCompareLower	= Fixnum2ObjId ( eCompareLower );

  fnBTreeMapStoreKey ( pValueKeyUpper, nTypeTagKeyUpper,
		       &pMapper->oKeyUpper, &pMapper->onTypeTagKeyUpper,
		       &pMapper->pValueKeyUpper );
  pMapper->onCompareUpper	= Fixnum2ObjId ( eCompareUpper );

  pMapper->onIncrement		= ( bDescending ) ? oM1 : o1;

  makunbound ( pMapper->oBTreePage );
  makunbound ( pMapper->onIndex );

  makunbound ( pMapper->oKey );
  makunbound ( pMapper->oData );

  RETURN ( 0 );
} /* fnBTreeMapSearch */

/* ----------------------------------------------------------------------- */
FIXNUM DLLEXPORT fnBTreeMapSeek		( OBJID		oMapper,
					  FIXNUM	nIncrement,
					  SEEK		eOrigin,
					  LPOBJID	poKey,
					  LPOBJID	poData )
{
  LPBTREEMAPPER	pMapper;
  FIXNUM	i, nIncr, nSeeked = 0, nSign, bDone = 0;

  PROCEDURE	( fnBTreeMapSeek );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  ASSERT_ObjId_is_valid ( oMapper );

  pMapper		=
    fnGetBTreeMapper ( NULLOBJID, oMapper, (SHLOCK *) NULL );
  ASSERT ( pMapper != NULL );

  switch ( eOrigin ) {
  case seekSet:
    bDone	= (FIXNUM) fnBTreeMapSeekSetOrEnd ( pMapper, FALSE );
    if ( (int) bDone <= 0 ) {
      RETURN ( bDone );
    }
    break;
  case seekEnd:
    bDone	= (FIXNUM) fnBTreeMapSeekSetOrEnd ( pMapper, TRUE );
    if ( (int) bDone <= 0 ) {
      RETURN ( bDone );
    }
    break;
  case seekCur:
    break;
  default:
    ERROR (( "Illegal seek origin %d.", (int) eOrigin ));
    RETURN ( (FIXNUM) eshGeneralError );
  }

  if ( nIncrement != 0 ) {
    nSign	= SGN ( nIncrement );
    nIncr	= ABS ( nIncrement );
    for ( i = 0, bDone = TRUE; i < nIncr && bDone > 0; i++ ) {
      bDone	= (FIXNUM) fnBTreeMapIncrement ( pMapper, nSign, 0 );
      if ( bDone < 0 ) {
	RETURN ( bDone );
      }
      if ( bDone > 0 ) {
	nSeeked++;
      } else {
	break;
      }
    }
  } else {
    bDone	= (FIXNUM) fnBTreeMapIncrement ( pMapper, 0, 0 );
    if ( bDone < 0 ) {
      RETURN ( bDone );
    }
  }

  if ( poKey != NULL ) {
    *poKey	= pMapper->oKey;
  }
  if ( poData != NULL ) {
    *poData	= pMapper->oData;
  }

  RETURN ( ( bDone < 0 ) ? (FIXNUM) bDone : nSeeked );
} /* fnBTreeMapSeek */

/* ----------------------------------------------------------------------- */
FIXNUM DLLEXPORT fnBTreeMapSeekSet	( OBJID		oMapper,
					  FIXNUM	nIncrement,
					  SEEK		eOrigin,
					  OBJID		oData )
{
  LPBTREEMAPPER	pMapper;
  FIXNUM	nReturnCode;
  SHLOCK	nLockOld;
  LPPLOBBTREE	pBTree = (LPPLOBBTREE) NULL;
  LPBTREEPAGE	pPage = (LPBTREEPAGE) NULL;
  int		nIndex;

  PROCEDURE	( fnBTreeMapSeekSet );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  nReturnCode	= fnBTreeMapSeek ( oMapper, nIncrement, eOrigin, NULL, NULL );
  if ( nReturnCode >= 0 ) {
    pMapper		=
      fnGetBTreeMapper ( NULLOBJID, oMapper, (SHLOCK *) NULL );
    ASSERT ( pMapper != NULL );
    if ( boundp ( pMapper->oBTreePage ) && boundp ( pMapper->onIndex ) ) {
      pBTree	= fnGetBTree ( pMapper->oHeap, pMapper->oBTree, &nLockOld );
      if ( (int) nLockOld < 0 ) {
	RETURN ( (FIXNUM) nLockOld );
      }
      /* If this ASSERT fails, there was maybe no active transaction on
	 oHeap: */
      ASSERT ( pBTree != NULL );
      pPage	= fnGetBTreePage ( pMapper->oBTreePage );
      ASSERT ( pPage != NULL );
      nLockOld	= fnLockBTreePage ( pMapper->oHeap, pBTree, pPage );
      if ( (int) nLockOld < 0 ) {
	RETURN ( (FIXNUM) nLockOld );
      }
      nIndex		= ObjId2Fixnum ( pMapper->onIndex );
      pPage->Items [ nIndex ].oData	= oData;
      pMapper->oData	= oData;
    } else {
      nReturnCode	= eshKeyNotFound;
    }
  }
  RETURN ( nReturnCode );
} /* fnBTreeMapSeekSet */

/* ----------------------------------------------------------------------- */
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
					  LPOBJID	lpoData )
{
  FIXNUM	nMapped = 0;
  OBJID		oMapper = NULLOBJID;
  LPBTREEMAPPER	pMapper;
  SHLOCK	nLockOld;
  int		nIncrement;

  PROCEDURE	( fnBTreeMapFirst );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  ASSERT_ObjId_is_valid ( oBTree );

  oMapper	=
    fnCreateObject ( (SHTYPETAG) eshBTreeMapperTag, 0, NULLTYPETAG, 0 );
  ASSERT ( boundp ( oMapper ) );
  pMapper	= fnGetBTreeMapper ( oHeap, oMapper, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    fnDestroyObject ( oMapper, FALSE );
    RETURN ( nLockOld );
  }
  ASSERT ( pMapper != NULL );
  SETFLAGBIT ( pMapper->Header.oTypeTag, flagTemporary, (BOOL) TRUE );
  pMapper->timeMapper	= timeGlobalStart;

  nMapped	=
    fnBTreeMapSearch ( oMapper, oHeap, oBTree,
		       pValueKeyLower, nTypeTagKeyLower, eCompareLower,
		       pValueKeyUpper, nTypeTagKeyUpper, eCompareUpper,
		       bDescending );
  if ( nMapped < 0 ) {
    RETURN ( nMapped );
  }

  ASSERT ( nMap > 0 );
  ASSERT ( lpoKey != NULL );
  ASSERT ( lpoData != NULL );
  nIncrement	= ( bDescending ) ? -1 : 1;
  nMapped	= fnBTreeMapIncrement ( pMapper, nIncrement, 0 );
  if ( nMapped >= 0 ) {
    if ( nMapped > 0 ) {
      nMapped			= 0;
      lpoKey [ nMapped ]	= pMapper->oKey;
      lpoData [ nMapped++ ]	= pMapper->oData;
      if ( nMapped < nMap ) {
	nMapped			= fnBTreeMapNext ( oMapper, nMap - 1,
						   & lpoKey [ 1 ],
						   & lpoData [ 1 ] );
	if ( nMapped < 0 ) {
	  oMapper		= fnBTreeMapLast ( oMapper );
	  RETURN ( nMapped );
	}
	nMapped++;
      }
    } else {
      oMapper		= fnBTreeMapLast ( oMapper );
      RETURN ( nMapped );
    }
  } else {
    oMapper	= fnBTreeMapLast ( oMapper );
    RETURN ( nMapped );
  }
  if ( lpoMapper != NULL ) {
    *lpoMapper	= oMapper;
  }
  ASSERT ( nMapped >= 0 );
  RETURN ( nMapped );
} /* fnBTreeMapFirst */

/* ----------------------------------------------------------------------- */
FIXNUM DLLEXPORT fnBTreeMapNext		( OBJID oMapper,
					  FIXNUM nMap,
					  LPOBJID lpoKey,
					  LPOBJID lpoData )
{
  int		nIncrement;
  FIXNUM	bDone, nMapped = 0;
  LPBTREEMAPPER	pMapper;

  PROCEDURE	( fnBTreeMapNext );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  ASSERT_ObjId_is_valid ( oMapper );

  /* A buffer of length 1 is needed at most: */
  ASSERT ( nMap > 0 );
  ASSERT ( lpoKey != NULL );
  ASSERT ( lpoData != NULL );

  if ( typetagof ( oMapper ) != eshBTreeMapperTag ) {
    RETURN ( 0 );
  }
  pMapper		=
    fnGetBTreeMapper ( NULLOBJID, oMapper, (SHLOCK *) NULL );
  ASSERT ( pMapper != NULL );
  
  nIncrement	= ObjId2Fixnum ( pMapper->onIncrement );

  while ( nMapped < nMap ) {
    bDone	= fnBTreeMapIncrement ( pMapper, nIncrement, nIncrement );
    if ( bDone < 0 ) {
      oMapper	= fnBTreeMapLast ( oMapper );
      RETURN ( bDone );
    }
    if ( bDone ) {
      lpoKey [ nMapped ]	= pMapper->oKey;
      lpoData [ nMapped++ ]	= pMapper->oData;
    } else {
      /* Finished with iteration. */
      oMapper	= fnBTreeMapLast ( oMapper );
      break;
    }
  }

  RETURN ( nMapped );
} /* fnBTreeMapNext */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnBTreeMapLast		( OBJID oMapper )
{
  LPBTREEMAPPER	pMapper;

  PROCEDURE	( fnBTreeMapLast );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  if ( ! ObjId_is_valid ( oMapper ) ) {
    RETURN ( NULLOBJID );
  }

  if ( typetagof ( oMapper ) != eshBTreeMapperTag ) {
    RETURN ( NULLOBJID );
  }

  pMapper	= fnGetBTreeMapper ( NULLOBJID, oMapper, (SHLOCK *) NULL );

  if ( GETFLAGBIT ( pMapper->Header.oTypeTag, flagTemporary ) ) {
    fnUnlock ( pMapper->oHeap, (SHLOCK)
	       ( (unsigned int) eshLockVectorRead |
		 (unsigned int) eshLockForce ),
	       oMapper, -1, (SHLOCK *) NULL );
    fnUnlock ( pMapper->oHeap, (SHLOCK)
	       ( (unsigned int) eshLockVectorWrite |
		 (unsigned int) eshLockForce ),
	       oMapper, -1, (SHLOCK *) NULL );
    fnDestroyObject ( oMapper, FALSE );
    oMapper	= NULLOBJID;
  }

  RETURN ( oMapper );
} /* fnBTreeMapLast */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT	fnBTreeSearch	( OBJID		oHeap,
					  OBJID		oBTree,
					  LPCVOID	pValueKey,
					  SHTYPETAG	nTypeTagKey,
					  LPOBJID	lpoKey,
					  LPOBJID	lpoData )
{
  LPPLOBBTREE	pBTree;
  BOOL		bInsert;
  BTREEITEM	Move;
  LPBTREEPAGE	lpPage;
  SHLOCK	nLockOld;

  PROCEDURE	( fnBTreeSearch );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  pBTree	= fnGetBTree ( oHeap, oBTree, &nLockOld );
  if ( (int) nLockOld < 0 ) {
    RETURN ( (BTREERESULT) nLockOld );
  }
  /* If this ASSERT fails, there was maybe no active transaction on
     oHeap: */
  ASSERT ( pBTree != NULL );
  lpPage	= fnGetBTreePage ( pBTree->oRoot );
  if ( lpoData ) {
    makunbound ( *lpoData );
  }
  bInsert	= (BOOL) FALSE;
  RETURN ( fnSearch ( oHeap, pBTree, lpPage,
		      pValueKey, nTypeTagKey,
		      lpoKey, lpoData, (LPOBJID) NULL, &bInsert, &Move ) );
} /* fnBTreeSearch */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT	fnBTreeSearchByObjId	( OBJID oHeap,
						  OBJID oBTree,
						  OBJID oKey,
						  LPOBJID lpoKey,
						  LPOBJID lpoData )
{
  OBJID		oKeyFound, oData;

  PROCEDURE	( fnBTreeSearchByObjId );

  nBTreeSearchByObjIdResultCache	=
    fnBTreeSearch ( oHeap, oBTree, &oKey, typetagof ( oKey ),
		    &oKeyFound, &oData );
  oBTreeSearchByObjIdCache		= oBTree;
  oBTreeSearchByObjIdKeyCache		= oKey;
  oBTreeSearchByObjIdKeyFoundCache	= oKeyFound;
  oBTreeSearchByObjIdDataCache		= oData;
  if ( lpoKey != NULL ) {
    *lpoKey				= oKeyFound;
  }
  if ( lpoData != NULL ) {
    *lpoData				= oData;
  }
  RETURN ( nBTreeSearchByObjIdResultCache );
} /* fnBTreeSearchByObjId */

/* ----------------------------------------------------------------------- */
COMPARETAG DLLEXPORT	fnBTreeTestMode	( OBJID oBTree,
					  COMPARETAG nNewTestMode )
{
  LPPLOBBTREE	pBTree;
  COMPARETAG	nOldTestMode;

  PROCEDURE	( fnBTreeTestMode );
  INITIALIZEPLOB;

  pBTree	= fnGetBTree ( NULLOBJID, oBTree, (SHLOCK *) NULL );
  nOldTestMode	= (COMPARETAG) ObjId2Fixnum ( pBTree->oCompare );
  if ( nNewTestMode != eshGetTestMode && pBTree->onCount == o0 ) {
    pBTree->oCompare	= Fixnum2ObjId ( nNewTestMode );
  }
  RETURN ( nOldTestMode );
} /* fnBTreeTestMode */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnBTreeRoot		( OBJID oBTree )
{
  LPPLOBBTREE	pBTree;

  PROCEDURE	( fnBTreeRoot );
  INITIALIZEPLOB;

  pBTree	= fnGetBTree ( NULLOBJID, oBTree, (SHLOCK *) NULL );
  if ( ! pBTree )
    RETURN ( NULLOBJID );
  RETURN ( pBTree->oRoot );
} /* fnBTreeRoot */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnBTreeCount		( OBJID oBTree )
{
  LPPLOBBTREE	pBTree		= (LPPLOBBTREE) NULL;

  PROCEDURE	( fnBTreeCount );
  INITIALIZEPLOB;

  pBTree		= fnGetBTree ( NULLOBJID, oBTree, (SHLOCK *) NULL );
  if ( pBTree == NULL ) {
    RETURN ( 0 );
  }
  nBTreeCountCache	= ObjId2Fixnum ( pBTree->onCount );
  oBTreeCountCache	= oBTree;
  RETURN ( nBTreeCountCache );
} /* fnBTreeCount */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnBTreeSize		( OBJID oBTree )
{
  LPPLOBBTREE	pBTree		= (LPPLOBBTREE) NULL;
  int		nPages = 0, nPageSize = 0;

  PROCEDURE	( fnBTreeSize );
  INITIALIZEPLOB;

  pBTree	= fnGetBTree ( NULLOBJID, oBTree, (SHLOCK *) NULL );
  if ( pBTree == NULL ) {
    RETURN ( 0 );
  }
  nPages	= ObjId2Fixnum ( pBTree->onPages );
  nPageSize	= ObjId2Fixnum ( pBTree->onPageSize );
  RETURN ( nPages * nPageSize );
} /* fnBTreeSize */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnBTreeSetPageSize	( OBJID oBTree,
					  int nNewPageSize )
{
  LPPLOBBTREE	pBTree		= (LPPLOBBTREE) NULL;
  int		nOldSize;

  PROCEDURE	( fnBTreeSetPageSize );
  INITIALIZEPLOB;

  pBTree	= fnGetBTree ( NULLOBJID, oBTree, (SHLOCK *) NULL );
  if ( pBTree == NULL ) {
    RETURN ( (int) eshGeneralError );
  }
  nOldSize	= ObjId2Fixnum ( pBTree->onPageSize );
  if ( nNewPageSize >= 0 ) {
    if ( nNewPageSize < nBTreeMinPageSize ) {
      ERROR (( "New page size %d for\n"
	       "       %s must be at least %d",
	       nNewPageSize,
	       fnPrintObject ( oBTree, (LPSTR) NULL, 0 ),
	       nBTreeMinPageSize ));
      RETURN ( (int) eshGeneralError );
    }
    if ( nNewPageSize > nBTreeMaxPageSize ) {
      ERROR (( "New page size %d for\n"
	       "       %s must not exceed %d",
	       nNewPageSize,
	       fnPrintObject ( oBTree, (LPSTR) NULL, 0 ),
	       nBTreeMaxPageSize ));
      RETURN ( (int) eshGeneralError );
    }
    if ( nNewPageSize % 2 != 0) {
      ERROR (( "New page size %d for\n"
	       "       %s must be even.",
	       nNewPageSize,
	       fnPrintObject ( oBTree, (LPSTR) NULL, 0 ) ));
      RETURN ( (int) eshGeneralError );
    }
    if ( boundp ( pBTree->oRoot ) ) {
      ERROR (( "Cannot set page size %d to\n"
	       "       non-empty %s",
	       nNewPageSize, fnPrintObject ( oBTree, (LPSTR) NULL, 0 ) ));
      RETURN ( (int) eshGeneralError );
    }
    pBTree->onPageSize	= Fixnum2ObjId ( nNewPageSize );
  }

  RETURN ( nOldSize );
} /* fnBTreeSetPageSize */

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnClientBtreeClear, "c-sh-btree-clear",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) )
{
  BTREERESULT	eResult;
  OBJID		oHeap, oBTree;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  eResult	= fnBTreeClear ( oHeap, oBTree );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnClientBtreeClear );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientBtreeCount, "c-sh-btree-count",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) )
{
  BTREERESULT	eResult;
  FIXNUM	nCount;
  OBJID		oBTree;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  nCount	= BTreeCount ( oBTree );

  UnstoreSession ();
  RETURN ( nCount );
} EndFunction ( fnClientBtreeCount );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
	        fnServerBtreeDelete, "c-sh-btree-delete",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( FIXNUM, value_in, nValueKey )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagKey ) ) )
{
  BTREERESULT	eResult;
  OBJID		oHeap, oBTree, oKey;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  oKey		= fnImmediate2ObjId ( nValueKey, &nTypeTagKey );
  eResult	= fnBTreeDelete ( oHeap, oBTree, &oKey, nTypeTagKey );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeDelete );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnServerBtreeDeleteByFloat, "c-sh-btree-delete-by-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( SINGLE_FLOAT, value_in, fKey ) ) )
{
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	= fnServerBtreeDeleteByPtr ( oShortObjIdHeap, oShortObjIdBTree,
					     &fKey, eshDynCFloatPtrTag );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeDeleteByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnServerBtreeDeleteByDouble, "c-sh-btree-delete-by-double",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fKey ) ) )
{
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	= fnServerBtreeDeleteByPtr ( oShortObjIdHeap, oShortObjIdBTree,
					     &fKey, eshDynCDoublePtrTag );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeDeleteByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		 fnServerBtreeDeleteByString, "c-sh-btree-delete-by-string",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( CONST_STRING, vector_in, szKey ) ) )
{
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	= fnServerBtreeDeleteByPtr ( oShortObjIdHeap, oShortObjIdBTree,
					     szKey, eshDynCStringPtrTag );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeDeleteByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
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
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  BTREERESULT	eResult;
  OBJID		oHeap, oBTree, oKey, oData;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  oKey		= fnImmediate2ObjId ( nValueKey, &nTypeTagKey );
  oData		= fnImmediate2ObjId ( nValueData, &nTypeTagData );
  eResult	= fnBTreeInsert ( oHeap, oBTree, &oKey, nTypeTagKey, oData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeInsert );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnServerBtreeInsertByFloat, "c-sh-btree-insert-by-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( SINGLE_FLOAT, value_in, fKey )
		  and
		  argument ( FIXNUM, value_in, nValueData )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	= fnServerBtreeInsertByPtr ( oShortObjIdHeap, oShortObjIdBTree,
					     &fKey, eshDynCFloatPtrTag,
					     &nValueData, nTypeTagData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeInsertByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnServerBtreeInsertByDouble, "c-sh-btree-insert-by-double",
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
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	= fnServerBtreeInsertByPtr ( oShortObjIdHeap, oShortObjIdBTree,
					     &fKey, eshDynCDoublePtrTag,
					     &nValueData, nTypeTagData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeInsertByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnServerBtreeInsertByString, "c-sh-btree-insert-by-string",
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
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	= fnServerBtreeInsertByPtr ( oShortObjIdHeap, oShortObjIdBTree,
					     szKey, eshDynCStringPtrTag,
					     &nValueData, nTypeTagData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeInsertByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
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
		  argument ( BOOL, value_in, bDescending ) ) )
{
  FIXNUM	nMapped;
  OBJID		oHeap, oBTree, oMapper;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oMapper	= Short2LongObjId ( oShortObjIdMapper );
  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= ( oShortObjIdBTree != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdBTree ) : NULLOBJID;
  nMapped	= fnBTreeMapSearch ( oMapper, oHeap, oBTree,
				     &nValueKeyLower, nTypeTagKeyLower,
				     eCompareLower,
				     &nValueKeyUpper, nTypeTagKeyUpper,
				     eCompareUpper, bDescending );

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapSearch );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
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
		  argument ( BOOL, value_in, bDescending ) ) )
{
  FIXNUM	nMapped;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  nMapped	=
    fnServerBtreemapSearchByPtr ( oShortObjIdMapper,
				  oShortObjIdHeap, oShortObjIdBTree,
				  &fKeyLower, nTypeTagKeyLower, eCompareLower,
				  &fKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				  bDescending );

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapSearchByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
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
		  argument ( BOOL, value_in, bDescending ) ) )
{
  FIXNUM	nMapped;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  nMapped	=
    fnServerBtreemapSearchByPtr ( oShortObjIdMapper,
				  oShortObjIdHeap, oShortObjIdBTree,
				  &fKeyLower, nTypeTagKeyLower, eCompareLower,
				  &fKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				  bDescending );

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapSearchByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
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
		  argument ( BOOL, value_in, bDescending ) ) )
{
  FIXNUM	nMapped;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  nMapped	=
    fnServerBtreemapSearchByPtr ( oShortObjIdMapper, oShortObjIdHeap,
				  oShortObjIdBTree,
				  szKeyLower, nTypeTagKeyLower, eCompareLower,
				  szKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				  bDescending );

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapSearchByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
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
		  argument ( SHTYPETAG, value_out, pnTypeTagData ) ) )
{
  FIXNUM	nSeeked;
  OBJID		oMapper, oKey, oData;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oMapper	= Short2LongObjId ( oShortObjIdMapper );
  makunbound ( oKey );
  makunbound ( oData );
  nSeeked	= fnBTreeMapSeek ( oMapper, nIncrement, eOrigin,
				   &oKey, &oData );

  fnObjIds2Values ( 1, &oKey, (SHORTOBJID *) pnValueKey,
		    (LPSHTYPETAG) pnTypeTagKey );
  fnObjIds2Values ( 1, &oData, (SHORTOBJID *) pnValueData,
		    (LPSHTYPETAG) pnTypeTagData );

  UnstoreSession ();
  RETURN ( nSeeked );
} EndFunction ( fnServerBtreemapSeek );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
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
		  argument ( SHTYPETAG, value_in, nTypeTagData ) ) )
{
  FIXNUM	nSeeked;
  OBJID		oMapper, oData;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oMapper	= Short2LongObjId ( oShortObjIdMapper );
  oData		= fnImmediate2ObjId ( nValueData, &nTypeTagData );
  nSeeked	= fnBTreeMapSeekSet ( oMapper, nIncrement, eOrigin, oData );

  UnstoreSession ();
  RETURN ( nSeeked );
} EndFunction ( fnServerBtreemapSeekSet );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
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
			     vector_out, pnTypeTagData ) ) )
{
  OBJID		oHeap, oBTree, oMapper;
  FIXNUM	nMapped;
  LPOBJID	lpoKey = (LPOBJID) NULL, lpoData = (LPOBJID) NULL;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( lpoKey != NULL ) {
	Free ( lpoKey );
	lpoKey	= (LPOBJID) NULL;
      }
      if ( lpoData != NULL ) {
	Free ( lpoData );
	lpoData	= (LPOBJID) NULL;
      }
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );

  lpoKey	= (LPOBJID) Malloc ( nMap * sizeof ( *lpoKey ) );
  ASSERT ( lpoKey != NULL );

  lpoData	= (LPOBJID) Malloc ( nMap * sizeof ( *lpoData ) );
  ASSERT ( lpoData != NULL );

  nMapped	= fnBTreeMapFirst ( &oMapper, oHeap, oBTree,
				    &nValueKeyLower, nTypeTagKeyLower,
				    eCompareLower,
				    &nValueKeyUpper, nTypeTagKeyUpper,
				    eCompareUpper,
				    bDescending, nMap, lpoKey, lpoData );
  if ( nMapped > 0 ) {
    fnObjIds2Values ( nMapped, lpoKey,
		      (SHORTOBJID *) pnValueKey,
		      (LPSHTYPETAG) pnTypeTagKey );
    fnObjIds2Values ( nMapped, lpoData,
		      (SHORTOBJID *) pnValueData,
		      (LPSHTYPETAG) pnTypeTagData );
    oMapper	= LONG2SHORTOBJID ( oMapper );
  } else {
    oMapper	= NULLOBJID;
  }
  if ( lpoShortObjIdMapper != NULL ) {
    *lpoShortObjIdMapper	= oMapper;
  }
  if ( lpoKey != NULL ) {
    Free ( lpoKey );
    lpoKey	= (LPOBJID) NULL;
  }
  if ( lpoData != NULL ) {
    Free ( lpoData );
    lpoData	= (LPOBJID) NULL;
  }

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapFirst );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
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
		  argument ( VECTOR ( int, nMap ), vector_out, pnValueKey )
		  and
		  argument ( VECTOR ( u_int, nMap ), vector_out, pnTypeTagKey )
		  and
		  argument ( VECTOR ( int, nMap ), vector_out, pnValueData )
		  and
		  argument ( VECTOR ( u_int, nMap ),
			     vector_out, pnTypeTagData ) ) )
{
  LPOBJID	lpoKey = (LPOBJID) NULL, lpoData = (LPOBJID) NULL;
  FIXNUM	nMapped;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( lpoKey != NULL ) {
	Free ( lpoKey );
	lpoKey	= (LPOBJID) NULL;
      }
      if ( lpoData != NULL ) {
	Free ( lpoData );
	lpoData	= (LPOBJID) NULL;
      }
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  nMapped	=
    fnServerBtreemapFirstByPtr ( lpoShortObjIdMapper,
				 oShortObjIdHeap, oShortObjIdBTree,
				 &fKeyLower, nTypeTagKeyLower, eCompareLower,
				 &fKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				 bDescending, nMap,
				 &lpoKey, &lpoData,
				 pnValueKey, pnTypeTagKey,
				 pnValueData, pnTypeTagData );

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapFirstByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
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
			     vector_out, pnTypeTagData ) ) )
{
  LPOBJID	lpoKey = (LPOBJID) NULL, lpoData = (LPOBJID) NULL;
  FIXNUM	nMapped;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( lpoKey != NULL ) {
	Free ( lpoKey );
	lpoKey	= (LPOBJID) NULL;
      }
      if ( lpoData != NULL ) {
	Free ( lpoData );
	lpoData	= (LPOBJID) NULL;
      }
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  nMapped	=
    fnServerBtreemapFirstByPtr ( lpoShortObjIdMapper,
				 oShortObjIdHeap, oShortObjIdBTree,
				 &fKeyLower, nTypeTagKeyLower, eCompareLower,
				 &fKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				 bDescending, nMap,
				 &lpoKey, &lpoData,
				 pnValueKey, pnTypeTagKey,
				 pnValueData, pnTypeTagData );
    
  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapFirstByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
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
			     vector_out, pnTypeTagData ) ) )
{
  LPOBJID	lpoKey = (LPOBJID) NULL, lpoData = (LPOBJID) NULL;
  FIXNUM	nMapped;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( lpoKey != NULL ) {
	Free ( lpoKey );
	lpoKey	= (LPOBJID) NULL;
      }
      if ( lpoData != NULL ) {
	Free ( lpoData );
	lpoData	= (LPOBJID) NULL;
      }
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  nMapped	=
    fnServerBtreemapFirstByPtr ( lpoShortObjIdMapper,
				 oShortObjIdHeap, oShortObjIdBTree,
				 szKeyLower, nTypeTagKeyLower, eCompareLower,
				 szKeyUpper, nTypeTagKeyUpper, eCompareUpper,
				 bDescending, nMap,
				 &lpoKey, &lpoData,
				 pnValueKey, pnTypeTagKey,
				 pnValueData, pnTypeTagData );

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnServerBtreemapFirstByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreemapNext, "c-sh-btree-map-next",
		( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( FIXNUM, value_in, nMap )
		  and	/* ouput arguments: */
		  argument ( VECTOR ( int, nMap ), vector_out, pnValueKey )
		  and
		  argument ( VECTOR ( u_int, nMap ), vector_out, pnTypeTagKey )
		  and
		  argument ( VECTOR ( int, nMap ), vector_out, pnValueData )
		  and
		  argument ( VECTOR ( u_int, nMap ), vector_out,
			     pnTypeTagData ) ) )
{
  OBJID		oMapper;
  FIXNUM	nMapped;
  LPOBJID	lpoKey	= (LPOBJID) NULL, lpoData = (LPOBJID) NULL;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( lpoKey != NULL ) {
	Free ( lpoKey );
	lpoKey	= (LPOBJID) NULL;
      }
      if ( lpoData != NULL ) {
	Free ( lpoData );
	lpoData	= (LPOBJID) NULL;
      }
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oMapper	= Short2LongObjId ( oShortObjIdMapper );
  lpoKey	= (LPOBJID) Malloc ( nMap * sizeof ( *lpoKey ) );
  ASSERT ( lpoKey != NULL );
  lpoData	= (LPOBJID) Malloc ( nMap * sizeof ( *lpoData ) );
  ASSERT ( lpoData != NULL );
  nMapped	= fnBTreeMapNext ( oMapper, nMap, lpoKey, lpoData );
  if ( nMapped > 0 ) {
    fnObjIds2Values ( nMapped, lpoKey,
		      (SHORTOBJID *) pnValueKey,
		      (LPSHTYPETAG) pnTypeTagKey );
    fnObjIds2Values ( nMapped, lpoData,
		      (SHORTOBJID *) pnValueData,
		      (LPSHTYPETAG) pnTypeTagData );
  }
  if ( lpoKey != NULL ) {
    Free ( lpoKey );
    lpoKey	= (LPOBJID) NULL;
  }
  if ( lpoData != NULL ) {
    Free ( lpoData );
    lpoData	= (LPOBJID) NULL;
  }

  UnstoreSession ();
  RETURN ( nMapped );
} EndFunction ( fnClientBtreemapNext );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientBtreemapLast, "c-sh-btree-map-last",
		( argument ( SHORTOBJID, value_in, oShortObjIdMapper )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  OBJID		oMapper;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oMapper	= Short2LongObjId ( oShortObjIdMapper );
  oMapper	= fnBTreeMapLast ( oMapper );
  oMapper	= ( boundp ( oMapper ) ) ?
    LONG2SHORTOBJID ( oMapper ) : NULLOBJID;

  UnstoreSession ();
  RETURN ( oMapper );
} EndFunction ( fnClientBtreemapLast );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientBtreePrint, "c-sh-btree-print",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( NUMERICSTDSTREAM, value_in, nStdStream ) ) )
{
  FIXNUM	nDumped;
  OBJID		oHeap, oBTree;
  LPPLOBBTREE	pBTree;
  FILE		FAR * lpStream;
  int		nLine;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  pBTree	= fnGetBTree ( NULLOBJID, oBTree, (SHLOCK *) NULL );
  lpStream	= ( nStdStream == eshStdOut ) ? stdout : stderr;
  nLine		= 1;
  nDumped	= fnDumpBTree ( oHeap, lpStream,
				fnGetBTreePage ( pBTree->oRoot ),
				0, &nLine );

  UnstoreSession ();
  RETURN ( nDumped );
} EndFunction ( fnClientBtreePrint );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
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
		  argument ( SHTYPETAG, value_out, pnTypeTagData ) ) )
{
  OBJID		oHeap, oBTree, oKey, oKeyFound, oData;
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  oKey		= fnImmediate2ObjId ( nValueKey, &nTypeTagKey );

  eResult	= fnBTreeSearch ( oHeap, oBTree, &oKey, nTypeTagKey,
				  &oKeyFound, &oData );

  fnObjIds2Values ( 1, &oKeyFound, pnValueKey,  pnTypeTagKey );
  fnObjIds2Values ( 1, &oData, pnValueData, pnTypeTagData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeSearch );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
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
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagData ) ) )
{
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	=
    fnServerBtreeSearchByPtr ( oShortObjIdHeap, oShortObjIdBTree,
			       &fKey, nTypeTagKey,
			       pnValueKey, pnTypeTagKey,
			       pnValueData, pnTypeTagData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeSearchByFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
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
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagData ) ) )
{
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	=
    fnServerBtreeSearchByPtr ( oShortObjIdHeap, oShortObjIdBTree,
			       &fKey, nTypeTagKey,
			       pnValueKey, pnTypeTagKey,
			       pnValueData, pnTypeTagData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeSearchByDouble );

/* ----------------------------------------------------------------------- */
BeginFunction ( BTREERESULT,
		fnServerBtreeSearchByString,
		"c-sh-btree-search-by-string",
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
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagKey )
		  and
		  argument ( FIXNUM, value_out, pnValueData )
		  and
		  argument ( SHTYPETAG, value_out,
			     pnTypeTagData ) ) )
{
  BTREERESULT	eResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BTREERESULT) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  eResult	=
    fnServerBtreeSearchByPtr ( oShortObjIdHeap, oShortObjIdBTree,
			       szKey, nTypeTagKey,
			       pnValueKey, pnTypeTagKey,
			       pnValueData, pnTypeTagData );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnServerBtreeSearchByString );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientBtreeRoot, "c-sh-btree-root",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) )
{
  OBJID		oBTree, oRoot;
  SHORTOBJID	oShortRoot;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (SHORTOBJID) 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  oRoot		= fnBTreeRoot ( oBTree );
  oShortRoot	= ( oRoot == NULLOBJID ) ?
    (SHORTOBJID) NULLOBJID : Long2ShortObjId ( oRoot );

  UnstoreSession ();
  RETURN ( oShortRoot );
} EndFunction ( fnClientBtreeRoot );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientBtreeSize, "c-sh-btree-size",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree ) ) )
{
  FIXNUM	nSize;
  OBJID		oBTree;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  nSize		= fnBTreeSize ( oBTree );

  UnstoreSession ();
  RETURN ( nSize );
} EndFunction ( fnClientBtreeSize );

/* ----------------------------------------------------------------------- */
BeginFunction ( COMPARETAG,
	        fnClientBtreeTestMode, "c-sh-btree-test-mode",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( COMPARETAG, value_in, nNewTestMode ) ) )
{
  COMPARETAG	eResult;
  OBJID		oBTree;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (COMPARETAG) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  eResult	= fnBTreeTestMode ( oBTree, nNewTestMode );

  UnstoreSession ();
  RETURN ( eResult );
} EndFunction ( fnClientBtreeTestMode );

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnBTreePageParent	( OBJID oBTreePage )
{
  LPBTREEPAGE	pBTreePage;

  PROCEDURE	( fnBTreePageParent );
  INITIALIZEPLOB;

  pBTreePage	= fnGetBTreePage ( oBTreePage );
  if ( ! pBTreePage )
    RETURN ( NULLOBJID );
  RETURN ( pBTreePage->oParentPage );
} /* fnBTreePageParent */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnBTreePageCount	( OBJID oBTreePage )
{
  LPBTREEPAGE	pBTreePage = (LPBTREEPAGE) NULL;

  PROCEDURE	( fnBTreePageCount );
  INITIALIZEPLOB;

  pBTreePage		= fnGetBTreePage ( oBTreePage );
  if ( pBTreePage == NULL ) {
    RETURN ( 0 );
  }
  nBTreePageCountCache	= ObjId2Fixnum ( pBTreePage->onCount );
  oBTreePageCountCache	= oBTreePage;
  RETURN ( nBTreePageCountCache );
} /* fnBTreePageCount */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnBTreePageSize		( OBJID oBTreePage )
{
  int		nSlots = 0, nBTreePageSize = 0;
  LPBTREEPAGE	pBTreePage = (LPBTREEPAGE) NULL;

  PROCEDURE	( fnBTreePageSize );
  INITIALIZEPLOB;

  pBTreePage		= fnGetBTreePage ( oBTreePage );
  if ( pBTreePage == NULL ) {
    RETURN ( 0 );
  }

  nSlots		=
    pBTreePage->Header.PostoreHeader.nObjIds + eshSHvectorIdxFirstObjId;
  nBTreePageSize	=
    ( nSlots -
      ( sizeof ( BTREEPAGE ) - sizeof ( ((LPBTREEPAGE)NULL)->Items ) ) /
      nSizeOfPostoreWord ) /
    ( sizeof ( ((LPBTREEPAGE)NULL)->Items [ 0 ] ) / nSizeOfPostoreWord );

  RETURN ( nBTreePageSize );
} /* fnBTreePageSize */

/* ----------------------------------------------------------------------- */
LPBTREEITEM DLLEXPORT	fnBTreePageItem		( OBJID oBTreePage,
						  int nIndex )
{
  LPBTREEPAGE	pBTreePage	= (LPBTREEPAGE) NULL;

  PROCEDURE	( fnBTreePageItem );
  INITIALIZEPLOB;

  pBTreePage	= fnGetBTreePage ( oBTreePage );
  if ( pBTreePage == NULL ) {
    RETURN ( (LPBTREEITEM) NULL );
  }
  if ( nIndex < 0 || nIndex >= ObjId2Fixnum ( pBTreePage->onCount ) ) {
    ERROR (( szCantIndex, fnPrintObject ( oBTreePage, (LPSTR) NULL, 0 ) ));
    RETURN ( (LPBTREEITEM) NULL );
  }
  RETURN ( & pBTreePage->Items [ nIndex ] );
} /* fnBTreePageItem */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreePageSize, "c-sh-btree-set-page-size",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTree )
		  and
		  argument ( FIXNUM, value_in, nNewPageSize ) ) )
{
  FIXNUM	nOldPageSize = 0;
  OBJID		oBTree;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (int) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTree	= Short2LongObjId ( oShortObjIdBTree );
  nOldPageSize	= fnBTreeSetPageSize ( oBTree, nNewPageSize );

  UnstoreSession ();
  RETURN ( nOldPageSize );
} EndFunction ( SH_btree_page_size );

/* ----------------------------------------------------------------------- */
BeginFunction ( OBJID,
		fnClientBtreepageParent, "c-sh-btreepage-parent",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTreePage ) ) )
{
  OBJID		oBTreePage, oParent;
  SHORTOBJID	oShortParent;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTreePage	= Short2LongObjId ( oShortObjIdBTreePage );
  oParent	= fnBTreePageParent ( oBTreePage );
  oShortParent	= ( oParent == NULLOBJID ) ?
    (SHORTOBJID) NULLOBJID : Long2ShortObjId ( oParent );

  UnstoreSession ();
  RETURN ( oShortParent );
} EndFunction ( fnClientBtreepageParent );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreepageCount, "c-sh-btreepage-count",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTreePage ) ) )
{
  FIXNUM	nCount;
  OBJID		oBTreePage;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTreePage	= Short2LongObjId ( oShortObjIdBTreePage );
  nCount	= fnBTreePageCount ( oBTreePage );

  UnstoreSession ();
  RETURN ( nCount );
} EndFunction ( fnClientBtreepageCount );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientBtreepageGetSize, "c-sh-btreepage-size",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdBTreePage ) ) )
{
  FIXNUM	nSize;
  OBJID		oBTreePage;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBTreePage	= Short2LongObjId ( oShortObjIdBTreePage );
  nSize		= fnBTreePageSize ( oBTreePage );

  UnstoreSession ();
  RETURN ( nSize );
} EndFunction ( fnClientBtreepageGetSize );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
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
			     poNext ) ) )
{
  OBJID		oBTreePage, oHeap, oKey, oData, oNext;
  int		i, n, nCount, nCopied;
  LPBTREEPAGE	pBTreePage;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  ASSERT ( nIndex >= -1 );
  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oBTreePage	= Short2LongObjId ( oShortObjIdBTreePage );
  pBTreePage	= fnGetBTreePage ( oBTreePage );
  ASSERT ( pBTreePage != NULL );
  nCount	= fnBTreePageCount ( oBTreePage );
  n		= ( nIndex + nItems >= nCount ) ?
    nCount : nIndex + nItems;
  for ( i = nIndex, nCopied = 0; i < n; i++ ) {
    if ( i < 0 ) {
      makunbound ( oKey );
      makunbound ( oData );
      oNext	= pBTreePage->oNext;
    } else {
      oKey	= pBTreePage->Items [ i ].oKey;
      oData	= pBTreePage->Items [ i ].oData;
      oNext	= pBTreePage->Items [ i ].oNext;
    }
    fnObjIds2Values ( 1, &oKey,
		      (SHORTOBJID *) & pnValueKey [ nCopied ],
		      (LPSHTYPETAG) & pnTypeTagKey [ nCopied ] );
    fnObjIds2Values ( 1, &oData,
		      (SHORTOBJID *) & pnValueData [ nCopied ],
		      (LPSHTYPETAG) & pnTypeTagData [ nCopied ] );
    poNext [ nCopied ]	= ( boundp ( oNext ) ) ?
      Long2ShortObjId ( oNext ) : (SHORTOBJID) NULLOBJID;
    nCopied++;
  }

  UnstoreSession ();
  RETURN ( nCopied );
} EndFunction ( fnClientBtreepageItem );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
