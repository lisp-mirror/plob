/* -------------------------------------------------------------------------
| Module	splobtype.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94 Derived from c-plob.c
| Description	PLOB various types
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

#include	<limits.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<ctype.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobtype.h"
#include	"splobnumber.h"
#include	"splobsequ.h"
#include	"splobstruct.h"
#include	"splobbtree.h"
#include	"splobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
OBJID			__oTypeTagOf__		= NULLOBJID;
OBJID			oTypeTagCache		= NULLOBJID;
SHTYPETAG		nTypeTagCache		= (SHTYPETAG) NULLTYPETAG;

/* -------------------------------------------------------------------------
| static types and constants
 ------------------------------------------------------------------------- */
static const char	szNullObjId []		=
UNREADABLE_OBJECT_PREFIX "null-objid" UNREADABLE_OBJECT_SUFFIX;

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */

/* Object initialization methods: */

/* Sample prototype for mfnInit...-functions:
static BOOL	mfnInit	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
*/

/* Object print methods: */
static LPSTR	mfnPrintSymbol	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintFunction( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );

/* Object name methods: */
static LPSTR		mfnSymbolNameOf		( OBJID oSelf,
						  LPINT lpnName );
static LPSTR		mfnFunctionNameOf	( OBJID oSelf,
						  LPINT lpnName );

/* Object equal methods: */
static COMPARETAG	mfnSymbolCompare	( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );
static COMPARETAG	mfnCStringCompare	( LPVOID	pSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeTypeModule		( void )
{
  PROCEDURE	( fnInitializeTypeModule );

  /* Register methods: */
  RegisterMethod ( eshSymbolTag, gfnInitializeInstance,
		   mfnInitStandard );
  RegisterMethod ( eshSymbolTag, gfnPrintObjectDetails,
		   mfnPrintSymbol );
  RegisterMethod ( eshSymbolTag, gfnCompare, mfnSymbolCompare );
  RegisterMethod ( eshSymbolTag, gfnNameOf, mfnSymbolNameOf );

  RegisterMethod ( eshFunctionTag, gfnInitializeInstance,
		   mfnInitStandard );
  RegisterMethod ( eshFunctionTag, gfnPrintObjectDetails,
		   mfnPrintFunction );
  RegisterMethod ( eshFunctionTag, gfnNameOf, mfnFunctionNameOf );

  RegisterMethod ( eshTLatterTag, gfnInitializeInstance,
		   mfnInitStandard );

  RegisterMethod ( eshDynCStringPtrTag, gfnCompare, mfnCStringCompare );

  RETURN ( VOID );
} /* fnInitializeTypeModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeTypeModule	( void )
{
  PROCEDURE	( fnDeinitializeTypeModule );

  RETURN ( VOID );
} /* fnDeinitializeTypeModule */

/* ----------------------------------------------------------------------- */
SHTYPETAG DLLEXPORT	fnTypeTagOf		( OBJID oObjId )
{
  SHTYPETAG	nTypeTag = (SHTYPETAG) NULLTYPETAG;
  psint		FAR * lpSHvector;

  PROCEDURE	( fnTypeTagOf );
  INITIALIZEPLOB;

  switch ( oObjId & nTagMask ) {
  case eshObjIdTag:
    if ( ObjId_is_valid ( oObjId ) ) {
      lpSHvector	= SH_key_to_address ( oObjId );
      ASSERT ( lpSHvector != NULL );
      if ( typetagp ( lpSHvector [ eshSHvectorIdxTypeTag ] ) ) {
	nTypeTag	=
	  ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] );
	ASSERT ( nTypeTag != eshObjIdTag );
      } else {
	ERROR (( "Encountered short-objid 0x%X (%d)"
		 " with invalid type tag 0x%X (%d)",
		 LONG2SHORTOBJID ( oObjId ),
		 LONG2SHORTOBJID ( oObjId ),
		 lpSHvector [ eshSHvectorIdxTypeTag ],
		 lpSHvector [ eshSHvectorIdxTypeTag ] ));
      }
    } else {
      nTypeTag	= eshObjIdTag;
    }
    break;
  case eshFixnumTag: case eshFixnumTag | ( 1 << nFixnumBitOffset ):
    nTypeTag	= eshFixnumTag;
    break;
  case eshMarkerTag:
    nTypeTag	= (SHTYPETAG) oObjId;
    break;
  default:
    nTypeTag	= (SHTYPETAG)
      ( (unsigned int) oObjId & (unsigned int) nTagMask );
    break;
  }
  if ( oObjId != NULLOBJID && nTypeTag != NULLTYPETAG ) {
    oTypeTagCache	= oObjId;
    nTypeTagCache	= nTypeTag;
  }
  RETURN ( nTypeTag );
} /* fnTypeTagOf */

/* ----------------------------------------------------------------------- */
void DLLEXPORT		fnUnexpectedTypeTag	( LPCSTR lpszFile,
						  LPCSTR lpszProc,
						  int nLine,
						  OBJID oObjId,
						  psint nIndex,
						  SHTYPETAG nTypeTagExpected )
{
  static const char	szUnexpectedTypeTag []	=
    "Object %s\n"
    "       has class %s%s,\n"
    "       not the expected class %s.";

  psint		FAR * lpSHvector, nRawIndex;
  SHTYPETAG	nTypeTagObjId;
  LPCLASSINFO	lpClassInfo;
  char		szClassNameObjId [ 80 ], szClassNameExpected [ 80 ];
  char		szIndex [ 80 ];

  PROCEDURE	( fnUnexpectedTypeTag );
  INITIALIZEPLOB;

  szIndex [ 0 ]	= '\0';
  nTypeTagObjId	= typetagof ( oObjId );

  if ( ! immediatep ( oObjId ) && nIndex >= 0 ) {
    ASSERT_ObjId_is_valid ( oObjId );
    lpSHvector	= SH_key_to_address ( oObjId );
    ASSERT ( lpSHvector );
    nRawIndex	= Cooked2RawIndex ( nIndex );
    ASSERT ( 0 <= nRawIndex && nRawIndex < eshSHvectorIdxFirstObjId +
	     lpSHvector [ eshSHvectorIdxObjIds ] );
    nTypeTagObjId	= typetagof ( lpSHvector [ nRawIndex ] );
    sprintf ( szIndex, " at slot location %d", nIndex );
  }

  _ERROR ( lpszFile, lpszProc, nLine,
	   ( szUnexpectedTypeTag,
	     fnPrintObject ( oObjId, (LPSTR) NULL, 0 ),
	     PrintObject ( TypeTag2ObjId ( nTypeTagObjId ),
			   szClassNameObjId ),
	     szIndex, PrintObject ( TypeTag2ObjId ( nTypeTagExpected ),
				    szClassNameExpected ) ) );
  RETURN ( VOID );
} /* fnUnexpectedTypeTag */

/* -------------------------------------------------------------------------
| Object initialization methods
 ------------------------------------------------------------------------- */
BOOL		mfnInitStandard	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  register int	i, nSlots;

  PROCEDURE	( mfnInitStandard );

  nSlots	= lpSHvector [ eshSHvectorIdxObjIds ];

  /* Set all values to 0: */
  i	= lpSHvector [ eshSHvectorIdxSize ] - nSlots -
    eshSHvectorIdxFirstObjId;
  if ( i > 0 ) {
    memset ( & lpSHvector [ nSlots + eshSHvectorIdxFirstObjId ],
	     0, i * sizeof ( psint ) );
  }

  /* Set all objids to eshUnboundTag: */
  for ( i = eshSHvectorIdxFirstData - eshSHvectorIdxFirstObjId,
        lpSHvector += eshSHvectorIdxFirstData;
	i < nSlots; i++, lpSHvector++ ) {
    makunbound ( *lpSHvector );
  }

  RETURN ( TRUE );
} /* mfnInitStandard */

/* -------------------------------------------------------------------------
| Object print methods
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintSymbol	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  PROCEDURE	( mfnPrintSymbol );

  RETURN ( fnPrintSymbol ( oObjId, lpszBuffer, nBuffer ) );
} /* mfnPrintSymbol */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintFunction( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  LPSTR		lpsName;
  int		nName;

  PROCEDURE	( mfnPrintFunction );
  lpsName	= mfnFunctionNameOf ( oObjId, &nName );
  if ( lpsName )
    strncpy ( lpszBuffer, lpsName, MIN ( nBuffer, nName ) );

  RETURN ( lpszBuffer );
} /* mfnPrintFunction */

/* -------------------------------------------------------------------------
| Object name methods
 ------------------------------------------------------------------------- */
static LPSTR		mfnSymbolNameOf		( OBJID oSelf,
						  LPINT lpnName )
{
  LPOBJID	lpSymbol;
  OBJID		oPackage, oName;
  LPOBJID	lpPackage;

  PROCEDURE	( mfnSymbolNameOf );

  lpSymbol	= SH_key_to_address ( oSelf );
  ASSERT ( lpSymbol != NULL );

  if ( ! boundp ( oGlobalPackageDescr ) ) {
    /* Try to find the package description object: */
    oPackage	= lpSymbol [ Cooked2RawIndex ( eshSymbolIdxPackage ) ];
    if ( structurep ( oPackage ) ) {
      lpPackage			= SH_key_to_address ( oPackage );
      oGlobalPackageDescr	=
	lpPackage [ Cooked2RawIndex ( eshStructIdxDesc ) ];
      LocateStructDescr ( oPackage );
    }
  }

  oName		= lpSymbol [ Cooked2RawIndex ( eshSymbolIdxName ) ];
  if ( boundp ( oName ) ) {
    RETURN ( gfnNameOf ( oName, lpnName ) );
  }
  RETURN ( (LPSTR) NULL );
} /* mfnSymbolNameOf */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnFunctionNameOf	( OBJID oSelf,
						  LPINT lpnName )
{
  LPOBJID	lpFunction;
  OBJID		oName;

  PROCEDURE	( mfnFunctionNameOf );

  lpFunction	= SH_key_to_address ( oSelf );
  oName		= lpFunction [ Cooked2RawIndex ( eshFunctionIdxName ) ];
  if ( boundp ( oName ) ) {
    RETURN ( gfnNameOf ( oName, lpnName ) );
  }
  RETURN ( (LPSTR) NULL );
} /* mfnFunctionNameOf */

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */

COMPARETAG DLLEXPORT	fnNameCompare	( LPCSTR	lpsSelf,
					  int		nSelf,
					  OBJID		oCompare,
					  BOOL		bIgnoreCase )
{
  LPSTR		lpsCompare;
  int		nCompare, nCmp;

  PROCEDURE	( fnNameCompare );

  lpsCompare	= gfnNameOf ( oCompare, &nCompare );
  if ( lpsSelf == NULL || lpsCompare == NULL ) {
    RETURN ( ( lpsSelf == lpsCompare ) ? eshEq : eshNotEqual );
  }

  nCmp	= ( bIgnoreCase ) ? 
    strnnicmp ( lpsSelf, nSelf, lpsCompare, nCompare ) :
    strnncmp ( lpsSelf, nSelf, lpsCompare, nCompare );

  if ( nCmp < 0 ) {
    RETURN ( eshLess );
  }

  if ( nCmp > 0 ) {
    RETURN ( eshGreater );
  }

  RETURN ( eshEqual );
} /* fnNameCompare */

/* -------------------------------------------------------------------------
| Compare methods
 ------------------------------------------------------------------------- */
static COMPARETAG	mfnSymbolCompare	( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  LPCSTR	psName = NULL;
  int		nName = 0;

  PROCEDURE	( mfnSymbolCompare );

  psName	= mfnSymbolNameOf ( * (LPOBJID) poSelf, &nName );

  RETURN ( fnNameCompare ( psName, nName, oCompare, FALSE ) );
} /* mfnSymbolCompare */

/* ----------------------------------------------------------------------- */
static COMPARETAG	mfnCStringCompare	( LPVOID	pSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  PROCEDURE	( mfnCStringCompare );
  RETURN ( fnNameCompare ( (LPCSTR) pSelf, strlen ( pSelf ),
			   oCompare, FALSE ) );
} /* mfnCStringCompare */

/* ----------------------------------------------------------------------- */
BeginFunction ( CONST_STRING,
		fnServerDbTypeTagName, "c-sh-type-string",
		( argument ( SHTYPETAG, value_in, nTypeTag ) ) )
{
  CONST_STRING	pszTypename;

  INITIALIZEPLOB;
  if ( oGlobalSession == NULLOBJID ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( szEmpty );
    }
  }
  pszTypename	= fnTypeString ( nTypeTag );

  UnstoreSession ();
  RETURN ( pszTypename );
} EndFunction ( fnServerDbTypeTagName );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnServerObjectPrettyPrint, "c-sh-pprint-objid",
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
  FIXNUM	nLength;
  OBJID		oObjId;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( lpszBuffer && nBuffer > 0 ) {
	*lpszBuffer	= '\0';
      }
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= fnImmediate2ObjId ( oShortObjId, &nTypeTag );
  fnPrintObject ( oObjId, lpszBuffer, nBuffer );

  nLength	= strlen ( lpszBuffer );

  UnstoreSession ();
  RETURN ( nLength );
} EndFunction ( fnServerObjectPrettyPrint );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnShortPrintSymbol, "c-sh-pprint-symbol",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortSymbol )
		  and
		  argument ( STRING ( nBuffer ),
			     vector_out, lpszBuffer )
		  and
		  argument ( FIXNUM, value_in, nBuffer ) ) )
{
  FIXNUM	nLength;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( lpszBuffer && nBuffer > 0 ) {
	*lpszBuffer	= '\0';
      }
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  fnPrintSymbol ( SHORT2LONGOBJID ( oShortSymbol ), lpszBuffer, nBuffer );
  nLength	= strlen ( lpszBuffer );

  UnstoreSession ();
  RETURN ( nLength );
} EndFunction ( fnShortPrintSymbol );

/* ----------------------------------------------------------------------- */
LPSTR DLLEXPORT	fnPrintObject		( OBJID oSelf,
					  LPSTR lpszBuffer,
					  size_t nBuffer )
{
  static const char	szFormatPrefix []	=
    UNREADABLE_OBJECT_PREFIX "%s ";
  static const char	szFormatSuffix []	=
    "%sshort-objid=%d" UNREADABLE_OBJECT_SUFFIX;

  /* 1997/09/26 HK: Multi threading restriction */
  static char	szBuffer [ 2048 ];

  int		i;
  char		szPrefix [ 80 ], szSuffix [ 80 ];
  LPOBJID	lpSHvector;
  LPCSTR	lpszBlank;
  LPCLASSINFO	lpClassInfo;
  SHTYPETAG	nTypeTag;
  LPFNMETHOD	lpfnMethod;

  PROCEDURE	( fnPrintObject );

  INITIALIZEPLOB;

  if ( lpszBuffer == (LPSTR) NULL || nBuffer == 0 ) {
    lpszBuffer	= szBuffer;
    nBuffer	= sizeof ( szBuffer );
  }

  if ( oSelf == NULLOBJID ) {
    strncpy ( lpszBuffer, szNullObjId, nBuffer );
    RETURN ( lpszBuffer );
  }

  nTypeTag	= typetagof ( oSelf );

  if ( immediatep ( oSelf ) ) {
    /* Print immediate object: */
    fnPrintImmediateObject ( fnObjId2Immediate ( oSelf, nTypeTag ),
			     nTypeTag, lpszBuffer, nBuffer );
    RETURN ( lpszBuffer );
  }

  /* Print non-immediate object: */
  lpSHvector	= (LPOBJID) NULL;
  lpszBlank	= szEmpty;
  if ( StableHeap_is_open && ObjId_is_valid ( oSelf ) ) {
    lpSHvector	= SH_key_to_address ( oSelf );
  }

  sprintf ( szPrefix, szFormatPrefix, fnTypeString ( nTypeTag ) );
  strncpy ( lpszBuffer, szPrefix, nBuffer );
  i		= strlen ( szPrefix );

  if ( i < nBuffer && lpSHvector != NULL ) {
    lpClassInfo	= (LPCLASSINFO) FindClassInfo ( nTypeTag );
    lpfnMethod	= _FindMethod ( nTypeTag, gfnPrintObjectDetails );
    if ( lpClassInfo != NULL ) {
      if ( lpfnMethod != NULL ) {
	( *lpfnMethod ) ( oSelf, lpSHvector, lpClassInfo,
			  & lpszBuffer [ i ], nBuffer - i );
      } else if ( lpClassInfo->lpszFormat ) {
	sprintf ( &lpszBuffer [ i ], lpClassInfo->lpszFormat, oSelf );
      }
    }
    if ( lpszBuffer [ i ] != '\0' && lpszBuffer [ i ] != ' ' ) {
      lpszBlank	= szSpace;
    }
    i		+= strlen ( & lpszBuffer [ i ] );
  }
  if ( i < nBuffer ) {
    sprintf ( szSuffix, szFormatSuffix, lpszBlank, LONG2SHORTOBJID ( oSelf ) );
    strncpy ( & lpszBuffer [ i ], szSuffix, nBuffer - i );
  }
  RETURN ( lpszBuffer );
} /* fnPrintObject */

/* ----------------------------------------------------------------------- */
int		fnTypeAddBuiltInClasses	( OBJID	oHeap )
{
  int		nAdded = 0, nMapped;
  OBJID		oLispRoot = NULLOBJID, oClasses = NULLOBJID;
  OBJID		oMapper = NULLOBJID, oKey, oData, oPackage;
  BOOL		bMapped;
  LPCLASSINFO	pClassInfo;
  char		szUpperName [ 128 ];

  PROCEDURE	( fnTypeAddBuiltInClasses );
  INITIALIZEPLOB;

  oLispRoot	= fnReadLispRoot ();

  if ( boundp ( oLispRoot ) ) {
    oClasses	=
      structure_slotref ( oLispRoot, eshLispRootIdxSymbolClassTable );
  }

  if ( boundp ( oClasses ) ) {
    ASSERT_ObjId_is_valid ( oClasses );
    /* Iterate on the class' btree, remove all current built in classes: */
    for ( nMapped =
	    fnBTreeMapFirstByObjId ( &oMapper, NULLOBJID, oClasses,
				     minmarker, eshGreaterEqual,
				     maxmarker, eshLessEqual,
				     FALSE, 1, &oKey, &oData );
	  nMapped > 0;
	  nMapped = fnBTreeMapNext ( oMapper, 1, &oKey, &oData ) ) {
      if ( typetagp ( oData ) ) {
	fnBTreeDeleteByObjId ( NULLOBJID, oClasses, oKey );
      }
    }
    for ( bMapped = fnClassInfoFirst ( (LPCLASSTAG) NULL, (LPCSTR *) NULL,
				       (LPVOID *) &pClassInfo );
	  bMapped;
	  bMapped = fnClassInfoNext ( (LPCLASSTAG) NULL, (LPCSTR *) NULL,
				      (LPVOID *) &pClassInfo ) ) {
      if ( pClassInfo->lpszPackageName != (LPCSTR) NULL &&
	   pClassInfo->lpszPackageName [ 0 ] != '\0' &&
	   ( pClassInfo->nTypeTag == eshMarkerTag ||
	     ! markerp ( pClassInfo->nTypeTag ) ) ) {
	strncpy ( szUpperName, pClassInfo->lpszPackageName,
		  sizeof ( szUpperName ) );
	STRUPR ( szUpperName );
	oPackage	= fnCreatePackage ( NULLOBJID, szUpperName );
	strncpy ( szUpperName, pClassInfo->lpszTypeName,
		  sizeof ( szUpperName ) );
	STRUPR ( szUpperName );
	oKey	= fnCreateSymbol ( NULLOBJID, oPackage, szUpperName );
	oData	= TypeTag2ObjId ( pClassInfo->nTypeTag );
	if ( BTreeSearchByObjId ( NULLOBJID, oClasses, oKey, NULL, NULL ) ==
	     btreeNotFound ) {
	  fnBTreeInsertByObjId ( NULLOBJID, oClasses, oKey, oData );
	}
      }
    }
  }

  RETURN ( nAdded );
} /* fnTypeAddBuiltInClasses */

/* ----------------------------------------------------------------------- */
LPSTR			fnBarifyString	( LPSTR lpszToBarify,
					  int nBarify )
{
  int		nLength, j;
  char		c;
  BOOL		bAddBars;

  PROCEDURE	( fnBarifyString );
  INITIALIZEPLOB;

  ASSERT ( lpszToBarify );
  if ( *lpszToBarify ) {
    for ( nLength = 0, bAddBars = FALSE;
	  nLength < nBarify && ( c = lpszToBarify [ nLength ] );
	  nLength++ ) {
      bAddBars	= (BOOL)
	( bAddBars ||  ( c <= ' ' ) || ( c == '\'' ) || ( c == '"' ) ||
	  islower ( c ) );
    }
  } else {
    nLength	= 0;
    bAddBars	= TRUE;
  }
  if ( bAddBars ) {
    if ( nLength < nBarify - 2 ) {
      for ( j = nLength; j > 0; j-- )
	lpszToBarify [ j ]		= lpszToBarify [ j - 1 ];
      lpszToBarify [ 0 ]		= '|';
      lpszToBarify [ nLength + 1 ]	= '|';
      lpszToBarify [ nLength + 2 ]	= '\0';
    }
  } else {
    STRLWR ( lpszToBarify );
  }
  RETURN ( lpszToBarify );
} /* fnBarifyString */

/* ----------------------------------------------------------------------- */
LPSTR DLLEXPORT	fnPrintSymbol		( OBJID oSymbol,
					  LPSTR lpszBuffer,
					  size_t nBuffer )
{
  static char	szBuffer [ 2048 ];

  LPOBJID	lpSymbol;
  OBJID		oName, oPackage;
  LPOBJID	lpPackage;
  LPSTR		lpszPackage, lpsName;
  int		n, i;

  PROCEDURE	( fnPrintSymbol );
  INITIALIZEPLOB;

  if ( lpszBuffer == (LPSTR) NULL || nBuffer == 0 ) {
    lpszBuffer	= szBuffer;
    nBuffer	= sizeof ( szBuffer );
  }

  if ( oSymbol == NULLOBJID ) {
    strncpy ( lpszBuffer, szNullObjId, nBuffer );
    RETURN ( lpszBuffer );
  }

  if ( ! symbolp ( oSymbol ) ) {
    sprintf ( lpszBuffer,
	      UNREADABLE_OBJECT_PREFIX 
	      "invalid symbol short-objid=%d"
	      UNREADABLE_OBJECT_SUFFIX,
	      LONG2SHORTOBJID ( oSymbol ) );
    RETURN ( lpszBuffer );
  }

  lpSymbol	= SH_key_to_address ( oSymbol );
  lpszPackage	= (LPSTR) NULL;
  oPackage	= lpSymbol [ Cooked2RawIndex ( eshSymbolIdxPackage ) ];
  i		= 0;
  memset ( lpszBuffer, 0, nBuffer );
  if ( structurep ( oPackage ) ) {
    lpPackage		= SH_key_to_address ( oPackage );
    oGlobalPackageDescr	= lpPackage [ Cooked2RawIndex ( eshStructIdxDesc ) ];
    oName		= lpPackage [ Cooked2RawIndex ( eshPackageIdxName ) ];
    lpszPackage		= gfnNameOf ( oName, &n );
    if ( lpszPackage ) {
      strncpy ( lpszBuffer, lpszPackage, MIN ( n, nBuffer ) );
      if ( STRICMP ( lpszBuffer, szKEYWORD ) == 0 ) {
	i			= 0;
	memset ( lpszBuffer, 0, nBuffer );
      } else {
	fnBarifyString ( lpszBuffer, nBuffer );
	i			= strlen ( lpszBuffer );
	if ( i < nBuffer )
	  lpszBuffer [ i++ ]	= ':';
      }
    }
  } else {
    lpszBuffer [ i++ ]	= '#';
  }
  if ( i < nBuffer )
    lpszBuffer [ i++ ]	= ':';
  oName			= lpSymbol [ Cooked2RawIndex ( eshSymbolIdxName ) ];
  lpsName		= gfnNameOf ( oName, &n );
  if ( lpsName ) {
    n	= MIN ( n, nBuffer - i - 1 );
    if ( n > 0 ) {
      strncpy ( & lpszBuffer [ i ], lpsName, n );
      fnBarifyString ( & lpszBuffer [ i ], nBuffer - i - 1 );
      i	+= strlen ( & lpszBuffer [ i ] );
    }
  }
  if ( i < nBuffer ) {
    lpszBuffer [ i ]	= '\0';
  }

  RETURN ( lpszBuffer );
} /* fnPrintSymbol */

/* ----------------------------------------------------------------------- */
FIXNUM			mfnStandardValues	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  PROCEDURE	( mfnStandardValues );
  INITIALIZEPLOB;

  /* Standard method returns 1 value. */
  RETURN ( 1 );
} /* mfnStandardValues */

/* ----------------------------------------------------------------------- */
SHTYPETAG		mfnStandardValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  PROCEDURE	( mfnStandardValueTypeTag );
  INITIALIZEPLOB;

  RETURN ( pClassInfo->nTypeTag );
} /* mfnStandardValueTypeTag */

/* -------------------------------------------------------------------------
| Generic functions
 ------------------------------------------------------------------------- */
FIXNUM DLLEXPORT	gfnValues	( OBJID		oSelf )
{
  FIXNUM	nValues = 0;
  LPFNMETHOD	pfnMethod;
  SHTYPETAG	eSelfTypeTag;
  LPCLASSINFO	pClassInfo;

  PROCEDURE	( gfnValues );
  INITIALIZEPLOB;

  eSelfTypeTag	= typetagof ( oSelf );
  pfnMethod	= (LPFNMETHOD) FindMethod ( eSelfTypeTag );
  if ( pfnMethod != NULL ) {
    pClassInfo	= (LPCLASSINFO) FindClassInfo ( eSelfTypeTag );
    nValues	= (FIXNUM) ( *pfnMethod ) ( oSelf, pClassInfo );
  }

  RETURN ( nValues );
} /* gfnValues */

/* ----------------------------------------------------------------------- */
SHTYPETAG DLLEXPORT	gfnValueTypeTag	( OBJID		oSelf )
{
  SHTYPETAG	eValueTypeTag = (SHTYPETAG) NULLTYPETAG;
  LPFNMETHOD	pfnMethod;
  SHTYPETAG	eSelfTypeTag;
  LPCLASSINFO	pClassInfo;

  PROCEDURE	( gfnValueTypeTag );
  INITIALIZEPLOB;

  eSelfTypeTag	= typetagof ( oSelf );
  pfnMethod	= (LPFNMETHOD) FindMethod ( eSelfTypeTag );
  if ( pfnMethod != NULL ) {
    pClassInfo		= (LPCLASSINFO) FindClassInfo ( eSelfTypeTag );
    eValueTypeTag	= (SHTYPETAG) ( *pfnMethod ) ( oSelf, pClassInfo );
  }

  RETURN ( eValueTypeTag );
} /* gfnValueTypeTag */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT	gfnInitializeInstance	( OBJID oSelf )
{
  BOOL		bInitialized;
  LPCLASSINFO	lpClassInfo;
  LPFNMETHOD	lpfnMethod;
  LPOBJID	lpSHvector;

  PROCEDURE	( gfnInitializeInstance );

  bInitialized	= TRUE;
  lpClassInfo	= (LPCLASSINFO) FindClassInfo ( typetagof ( oSelf ) );
  if ( lpClassInfo != NULL ) {
    lpfnMethod	= FindMethod ( lpClassInfo->nTypeTag );
    if ( lpfnMethod != NULL ) {
      lpSHvector	= (LPOBJID) SH_key_to_address ( oSelf );
      ASSERT ( lpSHvector != NULL );
      bInitialized	= (BOOL)
	( *lpfnMethod ) ( oSelf, lpSHvector, lpClassInfo );
    }
  }
  RETURN ( bInitialized );
} /* gfnInitializeInstance */

/* ----------------------------------------------------------------------- */
LPSTR DLLEXPORT	gfnPrintObjectDetails	( OBJID oSelf,
					  LPSTR lpszBuffer,
					  size_t nBuffer )
{
  LPSTR		lpszDetails;
  LPCLASSINFO	lpClassInfo;
  LPFNPMETHOD	lpfnMethod;
  LPOBJID	lpSHvector;

  PROCEDURE	( gfnPrintObjectDetails );
  INITIALIZEPLOB;

  ASSERT ( lpszBuffer != NULL );
  *lpszBuffer	= '\0';
  lpszDetails	= lpszBuffer;
  lpClassInfo	= (LPCLASSINFO) FindClassInfo ( typetagof ( oSelf ) );
  if ( lpClassInfo != NULL ) {
    lpfnMethod	= (LPFNPMETHOD) FindMethod ( lpClassInfo->nTypeTag );
    if ( lpfnMethod != NULL ) {
      lpSHvector	= (LPOBJID) SH_key_to_address ( oSelf );
      ASSERT ( lpSHvector );
      lpszDetails	= (LPSTR)
	( *lpfnMethod ) ( oSelf, lpSHvector, lpClassInfo,
			  lpszBuffer, nBuffer );
    }
  }
  RETURN ( lpszDetails );
} /* gfnPrintObjectDetails */

/* ----------------------------------------------------------------------- */
unsigned int DLLEXPORT	gfnCount	( OBJID oSelf )
{
  unsigned int	nCount	= 0;
  LPFNMETHOD	lpfnMethod;

  PROCEDURE	( gfnCount );
  INITIALIZEPLOB;

  lpfnMethod	= FindMethod ( typetagof ( oSelf ) );
  if ( lpfnMethod != NULL ) {
    nCount	= (unsigned int) ( *lpfnMethod ) ( oSelf );
  }
  RETURN ( nCount );
} /* gfnCount */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		gfnDestroy	( OBJID oSelf, 
					  BOOL bKill )
{
  LPFNMETHOD	lpfnMethod;

  PROCEDURE	( gfnDestroy );
  INITIALIZEPLOB;

  lpfnMethod	= FindMethod ( typetagof ( oSelf ) );
  if  ( lpfnMethod != NULL )
    RETURN ( (BOOL) ( *lpfnMethod ) ( oSelf, bKill ) );
  RETURN ( TRUE );
} /* gfnDestroy */

/* ----------------------------------------------------------------------- */
COMPARETAG DLLEXPORT	gfnCompare	( LPVOID	pSelf,
					  SHTYPETAG	eTypeTagSelf,
					  OBJID		oCompare )
{
  COMPARETAG	eCompared = eshNotEqual;
  SHTYPETAG	compareTypeTag;
  LPFNMETHOD	pfnMethod;

  PROCEDURE	( gfnCompare );
  INITIALIZEPLOB;

  compareTypeTag	= typetagof ( oCompare );
  if ( eTypeTagSelf == compareTypeTag && * (LPOBJID) pSelf == oCompare ) {
    eCompared	= eshEq;
  } else {
    pfnMethod		= FindMethod ( eTypeTagSelf );
    if ( pfnMethod != NULL ) {
      eCompared	= (COMPARETAG)
	( *pfnMethod ) ( pSelf, eTypeTagSelf, oCompare );
    }
  }

  RETURN ( eCompared );
}

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		gfnEqual	( OBJID oSelf,
					  OBJID oCompare )
{
  BOOL		isEqual = FALSE;
  SHTYPETAG	selfTypeTag;
  LPFNMETHOD	pfnMethod;

  PROCEDURE	( gfnEqual );
  INITIALIZEPLOB;

  if ( oSelf == oCompare ) {
    RETURN ( TRUE );
  }

  selfTypeTag	= typetagof ( oSelf );
  pfnMethod	= FindMethod ( selfTypeTag );
  if ( pfnMethod == NULL ) {
    /* Try gfnCompare. */
    switch ( gfnCompare ( &oSelf, selfTypeTag, oCompare ) ) {
    case eshEqual: case eshEql: case eshEq:
      isEqual	= TRUE;
    default:
      isEqual	= FALSE;
    }
  } else {
    isEqual	= (BOOL) ( *pfnMethod ) ( oSelf, oCompare );
  }
  RETURN ( isEqual );
} /* gfnEqual */

/* ----------------------------------------------------------------------- */
void DLLEXPORT		gfnFlush	( OBJID oSelf )
{
  LPFNMETHOD	lpfnMethod;

  PROCEDURE	( gfnFlush );
  INITIALIZEPLOB;

  lpfnMethod	= FindMethod ( typetagof ( oSelf ) );
  if ( lpfnMethod != NULL ) {
    ( *lpfnMethod ) ( oSelf );
  }
  RETURN ( VOID );
} /* gfnFlush */

/* ----------------------------------------------------------------------- */
LPSTR DLLEXPORT		gfnNameOf	( OBJID oSelf,
					  LPINT lpnName )
{
  LPFNPMETHOD	lpfnMethod;

  PROCEDURE	( gfnNameOf );
  INITIALIZEPLOB;

  lpfnMethod	= (LPFNPMETHOD) FindMethod ( typetagof ( oSelf ) );
  if  ( lpfnMethod != NULL )
    RETURN ( (LPSTR) ( *lpfnMethod ) ( oSelf, lpnName ) );
  RETURN ( (LPSTR) NULL );
} /* gfnNameOf */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	gfnLockAcquired	( OBJID oSelf,
					  OBJID oLocked,
					  SHLOCK nLockOld,
					  int nLockCount,
					  SHLOCK nLockNew )
{
  LPFNPMETHOD	lpfnMethod;

  PROCEDURE	( gfnLockAcquired );
  INITIALIZEPLOB;

  lpfnMethod	= (LPFNPMETHOD) FindMethod ( typetagof ( oSelf ) );
  if  ( lpfnMethod != NULL ) {
    RETURN ( (LPCSTR)  ( *lpfnMethod ) ( oSelf, oLocked, nLockOld,
					 nLockCount, nLockNew ) );
  }
  RETURN ( (LPCSTR) NULL );
} /* gfnLockAcquired */

/* ----------------------------------------------------------------------- */
void DLLEXPORT		gfnLockReleased	( OBJID oSelf,
					  OBJID oLocked,
					  SHLOCK nLockOld,
					  int nLockCount,
					  SHLOCK nLockNew )
{
  LPFNMETHOD	lpfnMethod;

  PROCEDURE	( gfnLockReleased );
  INITIALIZEPLOB;

  lpfnMethod	= FindMethod ( typetagof ( oSelf ) );
  if  ( lpfnMethod != NULL ) {
    ( *lpfnMethod ) ( oSelf, oLocked, nLockOld, nLockCount, nLockNew );
  }
  RETURN ( VOID );
} /* gfnLockReleased */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		gfnObjectStateChanged	( OBJID oSelf,
						  OBJID oLocked )
{
  LPFNMETHOD	lpfnMethod;

  PROCEDURE	( gfnObjectStateChanged );
  INITIALIZEPLOB;

  lpfnMethod	= FindMethod ( typetagof ( oSelf ) );
  if  ( lpfnMethod != NULL )
    RETURN ( (BOOL) ( *lpfnMethod ) ( oSelf, oLocked ) );
  RETURN ( TRUE );
} /* gfnObjectStateChanged */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
