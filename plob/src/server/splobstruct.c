/* -------------------------------------------------------------------------
| Module	splobstruct.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		9.3.94
| Description	
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
const char		szFormatSlots []	= "slots=%d/%d";
const char		szFormatVersNum []	= "%d.%02.2d";
const char		szFormatErrorDescr []	=
"The object %s\n"
"       is no proper %s description object.\n"
"       Reason: %s.";
const char		szStructure []		= "structure";
const char		szInvalidTypeTag []	= "invalid type tag";
const char		szNotSelfRef []		= "not self-referencing";
const char		szNSlotsNoFixnum []	=
"number of slots is no fixnum";

OBJID			oGlobalPackageDescr	= NULLOBJID;
OBJID			oGlobalStructDescr	= NULLOBJID;
OBJID			oGlobalStructSlotDescr	= NULLOBJID;

OBJID			oGlobalPkgCommonLisp	= NULLOBJID;
OBJID			oGlobalSymNil		= NULLOBJID;
OBJID			oGlobalSymT		= NULLOBJID;

OBJID			oGlobalPkgKeyword	= NULLOBJID;
OBJID			oGlobalSymKeywordRead	= NULLOBJID;
OBJID			oGlobalSymKeywordWrite	= NULLOBJID;
OBJID			oGlobalSymKeywordReadWrite	= NULLOBJID;

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */
static LPSTR		fnAdd1Name		( LPSTR lpsName,
						  int nName,
						  LPSTR lpszBuffer,
						  int nBuffer,
						  LPINT lpnIndex,
						  BOOL bBarify,
						  BOOL bAddLeadingBlank );
static LPSTR	mfnPrintStructure( OBJID oObjId, LPOBJID lpSHvector,
				   LPCLASSINFO lpClassInfo,
				   LPSTR lpszBuffer, size_t nBuffer );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeStructModule	( void )
{

  PROCEDURE	( fnInitializeStructModule );

  RegisterMethod ( eshStructureTag, gfnInitializeInstance,
		   mfnInitStandard );
  RegisterMethod ( eshStructureTag, gfnPrintObjectDetails,
		   mfnPrintStructure );

  RETURN ( VOID );
} /* fnInitializeStructModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeStructModule	( void )
{
  PROCEDURE	( fnDeinitializeStructModule );

  RETURN ( VOID );
} /* fnDeinitializeStructModule */

/* -------------------------------------------------------------------------
| Object print methods
 ------------------------------------------------------------------------- */
static LPSTR		fnAdd1Name		( LPSTR lpsName,
						  int nName,
						  LPSTR lpszBuffer,
						  int nBuffer,
						  LPINT lpnIndex,
						  BOOL bBarify,
						  BOOL bAddLeadingBlank )
{
  PROCEDURE	( fnAdd1Name );

  if ( lpsName ) {
    *lpnIndex	= strlen ( lpszBuffer );
    if ( bAddLeadingBlank && *lpnIndex > 0 && *lpnIndex < nBuffer )
      lpszBuffer [ (*lpnIndex)++ ]	= ' ';
    strncpy ( & lpszBuffer [ *lpnIndex ], lpsName,
	     MIN ( nBuffer - *lpnIndex - 1, nName ) );
    if ( bBarify )
      fnBarifyString ( & lpszBuffer [ *lpnIndex ], nBuffer - *lpnIndex - 1 );
    *lpnIndex	+= strlen ( & lpszBuffer [ *lpnIndex ] );
  }
  RETURN ( lpsName );
} /* fnAdd1Name */

/* ----------------------------------------------------------------------- */
LPSTR			fnAddName		( OBJID oName,
						  LPSTR lpszBuffer,
						  int nBuffer,
						  LPINT lpnIndex,
						  BOOL bAddLeadingBlank )
{
  LPSTR		lpsName;
  int		n;
  OBJID		oFirst, oSecond;

  PROCEDURE	( fnAddName );

  lpsName	= gfnNameOf ( oName, &n );
  if ( lpsName != NULL ) {
    fnAdd1Name ( lpsName, n, lpszBuffer, nBuffer, lpnIndex,
		 TRUE, bAddLeadingBlank );
  } else if ( consp ( oName ) ) {
    *lpnIndex	= strlen ( lpszBuffer );
    if ( bAddLeadingBlank && *lpnIndex > 0 && *lpnIndex < nBuffer )
      lpszBuffer [ (*lpnIndex)++ ]	= ' ';
    if ( *lpnIndex < nBuffer )
      lpszBuffer [ (*lpnIndex)++ ]	= '(';
    oFirst	= car ( oName );
    lpsName	= gfnNameOf ( oFirst, &n );
    fnAdd1Name ( lpsName, n, lpszBuffer, nBuffer, lpnIndex, TRUE, FALSE );
    oSecond	= cdr ( oName );
    if ( consp ( oSecond ) ) {
      oSecond	= car ( oSecond );
      if ( symbolp ( oSecond ) ) {
	lpsName	= fnPrintSymbol ( oSecond, (LPSTR) NULL, 0 );
	n	= strlen ( lpsName );
	fnAdd1Name ( lpsName, n, lpszBuffer, nBuffer, lpnIndex, FALSE, TRUE );
      } else {
	lpsName	= gfnNameOf ( oSecond, &n );
	fnAdd1Name ( lpsName, n, lpszBuffer, nBuffer, lpnIndex, TRUE, TRUE );
      }
    }
    if ( *lpnIndex < nBuffer )
      lpszBuffer [ (*lpnIndex)++ ]	= ')';
    if ( *lpnIndex < nBuffer )
      lpszBuffer [ (*lpnIndex)++ ]	= '\0';
  }
  RETURN ( lpsName );
} /* fnAddName */

/* ----------------------------------------------------------------------- */
void			fnLocateStructDescr	( OBJID oSelf )
{
  int		i;
  OBJID		oI;
  LPOBJID	lpI;

  PROCEDURE	( fnLocateStructDescr );

  if ( ! structurep ( oSelf ) )
    RETURN ( VOID );

  /* Search for the structure description object: Dereference the
     structure-description field until it points onto itself; this
     object is the structure-description: */
  if ( ! boundp ( oGlobalStructDescr ) ) {
    for ( i = 0, oI = oSelf, lpI = SH_key_to_address ( oSelf ); i < 4; i++ ) {
      if ( lpI [ Cooked2RawIndex ( eshStructIdxDesc ) ] == oI ) {
	/* Found the structure description: */
	oGlobalStructDescr	= oI;
	break;
      }
      oI	= lpI [ Cooked2RawIndex ( eshStructIdxDesc ) ];
      if ( OBJID2TYPETAG ( lpI [ eshSHvectorIdxTypeTag ] ) ==
	  eshStructureTag && ObjId_is_valid ( oI ) ) {
	lpI	= SH_key_to_address ( oI );
      } else {
	break;
      }
    }
  }

  if ( ! boundp ( oGlobalStructSlotDescr ) &&
       boundp ( oGlobalStructDescr ) ) {
    /* Look for the structure slot description vector; if found,
       then look for the structure slot description object: */
    lpI	= SH_key_to_address ( oGlobalStructDescr );
    oI	= lpI [ Cooked2RawIndex ( eshStructDescrIdxSlots ) ];
    if ( vectorp ( oI ) ) {
      oI	= vector_svref ( oI, 0 );
      if ( structurep ( oI ) ) {
	lpI	= SH_key_to_address ( oI );
	oI	= lpI [ Cooked2RawIndex ( eshStructIdxDesc ) ];
	if ( structurep ( oI ) ) {
	  lpI	= SH_key_to_address ( oI );
	  if ( lpI [ Cooked2RawIndex ( eshStructIdxDesc ) ] ==
	      oGlobalStructDescr ) {
	    oGlobalStructSlotDescr	= oI;
	  }
	}
      }
    }
  }
  RETURN ( VOID );
} /* fnLocateStructDescr */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintStructure( OBJID oObjId, LPOBJID lpSHvector,
				   LPCLASSINFO lpClassInfo,
				   LPSTR lpszBuffer, size_t nBuffer )
{
  int		i, n;
  OBJID		oName, oDesc, oInternals, oExternals;
  LPSTR		lpsName;
  LPOBJID	lpDesc, lpInternals, lpExternals;
  char		szBuffer [ 1024 ];

  PROCEDURE	( mfnPrintStructure );

  LocateStructDescr ( oObjId );
  memset ( szBuffer, 0, sizeof ( szBuffer ) );
  oDesc		= lpSHvector [ Cooked2RawIndex ( eshStructIdxDesc ) ];
  lpDesc	= ( ObjId_is_valid ( oDesc ) ) ?
    (LPOBJID) SH_key_to_address ( oDesc ) : (LPOBJID) NULL;
  oName		= ( lpDesc ) ?
    lpDesc [ Cooked2RawIndex ( eshStructDescrIdxName ) ] : NULLOBJID;
  lpsName	= gfnNameOf ( oName, &n );
  if ( lpsName ) {
    strncpy ( szBuffer, lpsName, MIN ( sizeof ( szBuffer ) - 1, n ) );
    fnBarifyString ( szBuffer, sizeof ( szBuffer ) - 1 );
  }

  i	= 0;
  if ( boundp ( oGlobalStructDescr ) && oDesc == oGlobalStructDescr ) {
    /* oSelf is a structure description object; add the structure name,
       version number and number of slots: */
    fnAddName ( lpSHvector [ Cooked2RawIndex ( eshStructDescrIdxName ) ],
	        szBuffer, sizeof ( szBuffer ), &i, TRUE );
    if ( fixnump ( lpSHvector [ Cooked2RawIndex
			        ( eshStructDescrIdxVersion ) ] ) &&
	 i < sizeof ( szBuffer ) - 10 ) {
      if ( i > 0 )
	szBuffer [ i++ ]	= ' ';
      sprintf ( & szBuffer [ i ], szFormatVersNum,
	        OBJID2FIXNUM ( lpSHvector
			       [ Cooked2RawIndex
				 ( eshStructDescrIdxVersion ) ] ) / 100,
	        OBJID2FIXNUM ( lpSHvector
			       [ Cooked2RawIndex
				 ( eshStructDescrIdxVersion ) ] ) % 100 );
      i	+= strlen ( & szBuffer [ i ] );
    }
    if ( i < sizeof ( szBuffer ) - 22 &&
	 fixnump ( lpSHvector [ Cooked2RawIndex
			        ( eshStructDescrIdxPNSlots ) ] ) &&
	 fixnump ( lpSHvector [ Cooked2RawIndex
			        ( eshStructDescrIdxNSlots ) ] ) ) {
      if ( i > 0 )
	szBuffer [ i++ ]	= ' ';
      sprintf ( & szBuffer [ i ], szFormatSlots,
	        OBJID2FIXNUM ( lpSHvector
			       [ Cooked2RawIndex
				 ( eshStructDescrIdxPNSlots ) ] ),
	        OBJID2FIXNUM ( lpSHvector
			       [ Cooked2RawIndex
				 ( eshStructDescrIdxNSlots ) ] ) );
    }
  } else if ( boundp ( oGlobalPackageDescr ) &&
	      oDesc == oGlobalPackageDescr ) {
    /* oSelf is a package object; add the package name ... */
    if ( fnAddName ( lpSHvector [ Cooked2RawIndex ( eshPackageIdxName ) ],
		     szBuffer, sizeof ( szBuffer ), &i, TRUE ) &&
	 i < sizeof ( szBuffer ) )
      szBuffer [ i++ ]	= ' ';
    if ( i < sizeof ( szBuffer ) - 16 ) {
      /* ... and the number of intern and extern symbols: */
      oInternals	=
	lpSHvector [ Cooked2RawIndex ( eshPackageIdxInternals ) ];
      oExternals	=
	lpSHvector [ Cooked2RawIndex ( eshPackageIdxExternals ) ];
      if ( btreep ( oInternals ) && btreep ( oExternals ) ) {
	lpInternals	= SH_key_to_address ( oInternals );
	lpExternals	= SH_key_to_address ( oExternals );
	sprintf ( & szBuffer [ i ], "%d/%d",
		  OBJID2FIXNUM ( lpInternals [ Cooked2RawIndex
					       ( eshBTreeIdxCount ) ] ),
		  OBJID2FIXNUM ( lpExternals [ Cooked2RawIndex
					       ( eshBTreeIdxCount ) ] ) );
      } else {
	szBuffer [ --i ]	= '\0';
      }
    } else {
      szBuffer [ --i ]	= '\0';
    }
  } else if ( boundp ( oGlobalStructSlotDescr ) &&
	      oDesc == oGlobalStructSlotDescr ) {
    /* oSelf is a structure slot description object; add the slot name
       and slot extent: */
    if ( fnAddName ( lpSHvector [ Cooked2RawIndex
				  ( eshStructSlotDescrIdxName ) ],
		     szBuffer, sizeof ( szBuffer ), &i, TRUE ) &&
	 i < sizeof ( szBuffer ) )
      szBuffer [ i++ ]	= ' ';
    if ( i < sizeof ( szBuffer ) - 8 ) {
      strncpy ( & szBuffer [ i ], "extent=", nBuffer - i );
      i	+= strlen ( & szBuffer [ i ] );
      fnAddName ( lpSHvector [ Cooked2RawIndex
			       ( eshStructSlotDescrIdxExtent ) ],
		  szBuffer, sizeof ( szBuffer ), &i, FALSE );
    }
  } else if ( lpDesc &&
	      structurep ( lpDesc [ Cooked2RawIndex
				    ( eshStructDescrIdxNextGen ) ] ) ) {
    /* oSelf is a 'normal' structure; add the version number iff
       the structure description is not the most actual structure
       description:*/
    i	= strlen ( szBuffer );
    if ( i < sizeof ( szBuffer ) - 8 ) {
      if ( i > 0 )
	szBuffer [ i++ ]	= ' ';
      sprintf ( & szBuffer [ i ], szFormatVersNum,
	        OBJID2FIXNUM ( lpDesc
			       [ Cooked2RawIndex
				 ( eshStructDescrIdxVersion ) ] ) / 100,
	        OBJID2FIXNUM ( lpDesc
			       [ Cooked2RawIndex
				 ( eshStructDescrIdxVersion ) ] ) % 100 );
    }
  }

  strncpy ( lpszBuffer, szBuffer, nBuffer );
  if ( nBuffer > 0 )
    lpszBuffer [ nBuffer - 1 ]	= '\0';
  RETURN ( lpszBuffer );
} /* mfnPrintStructure */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
OBJID		fnFindClass	( OBJID		oSymbolClassName )
{
  OBJID		oLispRoot = NULLOBJID, oClasses = NULLOBJID;
  OBJID		oClass = unbound;

  PROCEDURE	( fnFindClass );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oLispRoot	= fnReadLispRoot ();
  if ( boundp ( oLispRoot ) ) {
    oClasses	=
      structure_slotref ( oLispRoot, eshLispRootIdxSymbolClassTable );
  }
  if ( boundp ( oClasses ) ) {
    BTreeSearchByObjId ( NULLOBJID, oClasses, oSymbolClassName,
			 NULL, &oClass );
  }

  RETURN ( oClass );
} /* fnFindClass */

/* ----------------------------------------------------------------------- */
static OBJID	fnLocatePackage	( LPCSTR	pszPackageName,
				  LPOBJID	poPackageTable )
{
  OBJID		oLispRoot, oNamePackageTable, oPackage = unbound;

  PROCEDURE	( fnLocatePackage );
  ASSERT ( pszPackageName != (LPCSTR) NULL );

  if ( poPackageTable != NULL ) {
    *poPackageTable	= unbound;
  }
  oLispRoot	= fnReadLispRoot ();
  if ( boundp ( oLispRoot ) ) {
    oNamePackageTable	=
      structure_slotref ( oLispRoot, eshLispRootIdxNamePkgTable );
    if ( boundp ( oNamePackageTable ) ) {
      if ( poPackageTable != NULL ) {
	*poPackageTable	= oNamePackageTable;
      }
      fnBTreeSearch ( NULLOBJID, oNamePackageTable, pszPackageName,
		      eshDynCStringPtrTag, (LPOBJID) NULL, &oPackage );
    }
  }
  RETURN ( oPackage );
} /* fnLocatePackage */

/* ----------------------------------------------------------------------- */
OBJID		fnFindPackage	( LPCSTR	pszPackageName )
{
  PROCEDURE	( fnFindPackage );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  RETURN ( fnLocatePackage ( pszPackageName, (LPOBJID) NULL ) );
} /* fnFindPackage */

/* ----------------------------------------------------------------------- */
OBJID		fnCreatePackage	( OBJID		oHeap,
				  LPCSTR	pszPackageName )
{
  OBJID		oClassPackageName = unbound, oClassPackage = unbound;
  OBJID		oPackage = unbound, oPackageTable = unbound, oName;

  PROCEDURE	( fnCreatePackage );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oPackage	= fnLocatePackage ( pszPackageName, &oPackageTable );
  if ( ! boundp ( oPackage ) && boundp ( oPackageTable ) ) {
    oPackage		= fnLocatePackage ( szCOMMONLISP, (LPOBJID) NULL );
    if ( boundp ( oPackage ) ) {
      oClassPackageName	= fnFindSymbol ( oPackage, szPACKAGE );
    }
    if ( boundp ( oClassPackageName ) ) {
      oClassPackage	= fnFindClass ( oClassPackageName );
    }
    if ( boundp ( oClassPackage ) ) {
      /* Create the package and put it into the package table: */
      oName		= make_string ( pszPackageName );
      oPackage		=
	fnCreateObject ( (SHTYPETAG) eshStructureTag, 0, NULLTYPETAG, 0 );
      structure_slotref ( oPackage, eshStructIdxDesc )	= oClassPackage;
      package_name ( oPackage )		= oName;
      package_internals ( oPackage )	= oPackage;
      fnBTreeInsertByObjId ( oHeap, oPackageTable, oName, oPackage );
    }
  }

  RETURN ( oPackage );
} /* fnCreatePackage */

/* ----------------------------------------------------------------------- */
static OBJID	fnLocateSymbol	( OBJID		oPackage,
				  LPCSTR	pszSymbolName,
				  LPOBJID	poInternalTable )
{
  OBJID		oTable = unbound, oSymbol = unbound;

  PROCEDURE	( fnLocateSymbol );

  ASSERT ( pszSymbolName != (LPCSTR) NULL );

  if ( poInternalTable != NULL ) {
    *poInternalTable	= unbound;
  }
  if ( boundp ( oPackage ) ) {
    ASSERT ( structurep ( oPackage ) );
    oTable	= structure_slotref ( oPackage, eshPackageIdxInternals );
    if ( boundp ( oTable ) ) {
      if ( poInternalTable != NULL ) {
	*poInternalTable	= oTable;
      }
    } else {
      oTable	= structure_slotref ( oPackage, eshPackageIdxExternals );
    }
    if ( boundp ( oTable ) ) {
      fnBTreeSearch ( NULLOBJID, oTable, pszSymbolName,
		      eshDynCStringPtrTag, (LPOBJID) NULL, &oSymbol );
    }
  }
  RETURN ( oSymbol );
} /* fnLocateSymbol */

/* ----------------------------------------------------------------------- */
OBJID		fnFindSymbol	( OBJID		oPackage,
				  LPCSTR	pszSymbolName )
{
  PROCEDURE	( fnFindSymbol );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  RETURN ( fnLocateSymbol ( oPackage, pszSymbolName, (LPOBJID) NULL ) );
} /* fnFindSymbol */

/* ----------------------------------------------------------------------- */
OBJID		fnCreateSymbol	( OBJID		oHeap,
				  OBJID		oPackage,
				  LPCSTR	pszSymbolName )
{
  OBJID		oSymbol = unbound, oInternalTable = unbound, oName;

  PROCEDURE	( fnCreateSymbol );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oSymbol	= fnLocateSymbol ( oPackage, pszSymbolName, &oInternalTable );
  if ( ! boundp ( oSymbol ) && boundp ( oInternalTable ) ) {
    /* Create the symbol and put it into the internal table: */
    oSymbol	= fnCreateObject ( eshSymbolTag, 0, NULLTYPETAG, 0 );
    oName	= make_string ( pszSymbolName );
    symbol_name ( oSymbol )	= oName;
    symbol_package ( oSymbol )	= oPackage;
    fnBTreeInsertByObjId ( oHeap, oInternalTable, oName, oSymbol );
  }

  RETURN ( oSymbol );
} /* fnCreateSymbol */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerDbCreateStructures, "c-sh-create-structures",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in,
			     oShortObjIdStructDescr )
		  and
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_out, pObjIds )
		  and
		  argument ( u_int, value_out, pnSlots ) ) )
{
  int		nCreated = 0;
  OBJID		oStructure, oStructDescr, oDependent, oSlots;
  int		nSlots;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oStructDescr	= Short2LongObjId ( oShortObjIdStructDescr );
  if ( ! structurep ( oStructDescr ) ) {
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oStructDescr, (LPSTR) NULL, 0 ),
	     szStructure, szInvalidTypeTag ));
    UnstoreSession ();
    RETURN ( 0 );
  }

  LocateStructDescr ( oStructDescr );
  if ( structure_slotref ( oStructDescr, eshStructIdxDesc ) !=
       oGlobalStructDescr ) {
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oStructDescr, (LPSTR) NULL, 0 ),
	     szStructure, szNotSelfRef ));
    UnstoreSession ();
    RETURN ( 0 );
  }
  oSlots	= structure_slotref ( oStructDescr, eshStructDescrIdxPNSlots );
  if ( ! fixnump ( oSlots ) ) {
    UnstoreSession ();
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oStructDescr, (LPSTR) NULL, 0 ),
	     szStructure, szNSlotsNoFixnum ));
    RETURN ( 0 );
  }
  nSlots	= OBJID2FIXNUM ( oSlots );
  if ( pnSlots != NULL ) {
    *pnSlots	= nSlots;
  }
  oDependent	=
    structure_slotref ( oStructDescr, eshStructDescrIdxDependent );

  for ( /* nCreated = 0 */; nCreated < nObjIds; nCreated++ ) {
    oStructure	=
      fnCreateObject ( (SHTYPETAG) eshStructureTag, nSlots, NULLTYPETAG, 0 );
    ASSERT ( boundp ( oStructure ) );
    structure_slotref ( oStructure, eshStructIdxDesc )	= oStructDescr;
    if ( boundp ( oDependent ) ) {
      fnMakeDependentFromSymbol ( oStructure, oDependent );
    }
    if ( pObjIds != NULL ) {
      pObjIds [ nCreated ]	= LONG2SHORTOBJID ( oStructure );
    }
  }

  UnstoreSession ();
  RETURN ( nCreated );
} EndFunction ( fnServerDbCreateStructures );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
