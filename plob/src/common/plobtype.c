/* -------------------------------------------------------------------------
| Module	plobtype.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB source code common for server and client.
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
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobintern.h"
#include	"plobmisc.h"
#include	"plobtype.h"
#include	"plobsequ.h"
#include	"plobstruct.h"
#include	"plobbtree.h"
#include	"plobnumber.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Error message formats
 ------------------------------------------------------------------------- */
const char		szObjIdNotTypeTag []	=
"The long objid %d is no immediate type tag.";

const char		szObjIdNotFlag []	=
"The long objid %d is no immediate flag word.";

static const char	szUnknownTypeTag []	=
"Encountered unknwon type tag 0x%X (%s).";

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintBuiltIn	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintCharacter( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
/* PLOB type info enumeration callback which calls the LISP callback: */
static BOOL	fnPLOBenumClassInfoCallback	( int nFirstEnumArgument,
						  CLASSTAG nClassTag,
						  LPCSTR lpszTypeName,
						  LPVOID lpClassInfo );


/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitCommonTypeModule		( void )
{
  /* Type description table.
     The type names specified here correspond directly to LISP type names.
     They are read at startup-time from the LISP system and are used
     for mapping beween LISP types and PLOB types.
     In an ultimate persistent system this table should be loaded from
     the persistent store ... */
  static CLASSINFO ClassInfoTable []	= {
    { IMMEDIATE_TYPE_INFO ( eshCharacterTag,
			    SZCOMMONLISP,
			    "character",
			    sizeof ( char ) * nBitsPerByte,
			    "#\\%c" ) },
    { IMMEDIATE_TYPE_INFO ( eshBitmaskTag,
			    SZPLOB,
			    "bitmask",
			    0,
			    "bitmask=0x%X" ) },
    { IMMEDIATE_TYPE_INFO ( eshBuiltInTag,
			    SZCOMMONLISP,
			    "built-in-class",
			    0,
			    "type-tag=0x%X" ) },
    { MARKER_TYPE_INFO ( eshMarkerTag, "marker" ) },
    { MARKER_TYPE_INFO ( eshUnboundTag, "unbound-marker" ) },
    { MARKER_TYPE_INFO ( eshSlotUnboundTag, "slot-unbound-marker" ) },
    { MARKER_TYPE_INFO ( eshUnstorableTag, "unstorable-object-marker" ) },
    { MARKER_TYPE_INFO ( eshEndTag, "end-marker" ) },
    { MARKER_TYPE_INFO ( eshMinTag, "min-marker" ) },
    { MARKER_TYPE_INFO ( eshMaxTag, "max-marker" ) },
    { MARKER_TYPE_INFO ( eshTrueTag, "true-marker" ) },
    { MARKER_TYPE_INFO ( eshIgnoreSlotTag, "ignore-slot-marker" ) },
    { MARKER_TYPE_INFO ( eshNilTag, "nil-marker" ) },
    { MARKER_TYPE_INFO ( eshAllowTag, "allow-marker" ) },
    { MARKER_TYPE_INFO ( eshDenyTag, "deny-marker" ) },
    { MARKER_TYPE_INFO ( eshEqTag, "eq-marker" ) },
    { MARKER_TYPE_INFO ( eshEqlTag, "eql-marker" ) },
    { MARKER_TYPE_INFO ( eshEqualTag, "equal-marker" ) },
    /* The type with tag eshObjIdTag is to PLOB what the class T is to
       LISP; a PLOB element of type eshObjIdTag can hold any object: */
    { (SHTYPETAG) eshObjIdTag,
        SZCOMMONLISP,
	"t",
	sizeof ( psint ) * nBitsPerByte,
	0,
	"objid=%d",
	typeNotAllocatableP },
    { (SHTYPETAG) eshSymbolTag,
        SZCOMMONLISP,
	"symbol",
	eshSymbolObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeNoFlags },
    { (SHTYPETAG) eshFunctionTag,
        SZCOMMONLISP,
	"function",
	eshFunctionSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeNoFlags },
    { (SHTYPETAG) eshTLatterTag,
        SZPLOB,
	"tlatter",
	eshTLatterObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeNoFlags },
    { (SHTYPETAG) eshShortObjIdTag,
        SZPLOB,
	"short-objid",
	sizeof ( SHORTOBJID ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeNotAllocatableP },
    { (SHTYPETAG) eshDynCStringPtrTag,
        SZPLOB,
	"dynamic-cstring",
	0,
	0,
	(LPCSTR) NULL,
	(TYPEFLAGS) ( (unsigned int) typeNotAllocatableP |
		      (unsigned int) typeTransientP ) }
  };

  int		i;

  PROCEDURE	( fnInitCommonTypeModule );

  /* Register classes: */
  for ( i = 0; i < length ( ClassInfoTable ); i++ ) {
    RegisterPlobClass ( & ClassInfoTable [ i ] );
  }

  /* Register methods: */
  RegisterMethod ( eshBuiltInTag, gfnPrintObjectDetails,
		   mfnPrintBuiltIn );
  RegisterMethod ( eshCharacterTag, gfnPrintObjectDetails,
		   mfnPrintCharacter );

  RETURN ( VOID );
} /* fnInitCommonTypeModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitCommonTypeModule	( void )
{
  PROCEDURE	( fnDeinitCommonTypeModule );

  RETURN ( VOID );
} /* fnDeinitCommonTypeModule */

/* -------------------------------------------------------------------------
| Object print methods
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintBuiltIn	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  static const char	szTypeFlag2Char []	=
    "i" /* typeImmediateP */
    "a"	/* typeNotAllocatableP */
    "t"	/* typeTransientP */
    "o"	/* typeVarSizeObjIdP */
    "v"	/* typeVarSizeValueP */
    "r"	/* typeRecycleP */
    "n"	/* typeNotYetImplemented */;

  char		szBuffer [ 128 ], szTypeFlags [ 64 ];
  LPCLASSINFO	lpClass;
  SHTYPETAG	nTypeTag;
  unsigned int	i, l, f;

  PROCEDURE	( mfnPrintBuiltIn );

  nTypeTag	= OBJID2TYPETAG ( oObjId );
  lpClass	= (LPCLASSINFO) FindClassInfo ( nTypeTag );
  if ( lpClass != NULL ) {
    for ( i = 0, f = 1, l = 0; f <= typeFlagMax; i++, f <<= 1 ) {
      if ( ( lpClass->nTypeFlags & f ) != 0 ) {
	if ( l == 0 ) {
	  strcpy ( szTypeFlags, " flags=" );
	  l	= strlen ( szTypeFlags );
	}
	szTypeFlags [ l++ ]	= ( i < length ( szTypeFlag2Char ) ) ?
	  szTypeFlag2Char [ i ] : '?';
      }
    }
    szTypeFlags [ l++ ]	= '\0';
    sprintf ( szBuffer,
	      UNREADABLE_OBJECT_PREFIX
	      "%s%s %s"
	      UNREADABLE_OBJECT_SUFFIX,
	      lpClassInfo->lpszTypeName, szTypeFlags, lpClass->lpszTypeName );
  } else {
    sprintf ( szBuffer,
	      UNREADABLE_OBJECT_PREFIX
	      "%s type-tag=%d"
	      UNREADABLE_OBJECT_SUFFIX,
	      lpClassInfo->lpszTypeName, nTypeTag );
  }
  strncpy ( lpszBuffer, szBuffer, nBuffer );

  RETURN ( lpszBuffer );
} /* mfnPrintBuiltIn */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintCharacter( OBJID oObjId, LPOBJID lpSHvector,
				   LPCLASSINFO lpClassInfo,
				   LPSTR lpszBuffer, size_t nBuffer )
{
  typedef struct {
    int		nAsciiCode;
    LPCSTR	pszName;
  }	NAMEENTRY;

  static BOOL	bFirstTime	= TRUE;

  static const NAMEENTRY NameEntries [] = {
    {   0, "Null" },
    {   1, "SOH" },
    {   2, "STX" },
    {   3, "ETX" }, 
    {   4, "EOT" },
    {   5, "ENQ" },
    {   6, "ACK" },
    {   7, "Bell" },
    {   8, "Backspace" },
    {   9, "Tab" },
    {  10, "Newline" },
    {  11, "VT" },
    {  12, "Page" }, 
    {  13, "Return" },
    {  14, "SO" },
    {  15, "SI" },
    {  16, "DLE" },
    {  17, "DC1" },
    {  18, "DC2" },
    {  19, "DC3" },
    {  20, "DC4" },
    {  21, "NAK" },
    {  22, "SYN" },
    {  23, "ETB" },
    {  24, "CAN" },
    {  25, "EM" },
    {  26, "SUB" },
    {  27, "Escape" },
    {  28, "FS" },
    {  29, "GS" },
    {  30, "RS" },
    {  31, "US" },
    {  32, "Space" },
    { 127, "Rubout" }
  };

  static LPCSTR	ppszNames [ 256 ];

  int		nCode;
  char		szBuffer [ 16 ];

  PROCEDURE	( mfnPrintCharacter );

  ASSERT ( lpClassInfo != NULL );
  ASSERT ( lpClassInfo->lpszFormat != NULL );
  ASSERT ( lpszBuffer != NULL );


  if ( bFirstTime ) {
    int		i;
    BOOL	bScannedPercent = FALSE;
    char	szStringFormat [ 32 ];

    bFirstTime	= FALSE;
    strncpy ( szStringFormat, lpClassInfo->lpszFormat,
	      sizeof ( szStringFormat ) );
    for ( i = 0; szStringFormat [ i ] != '\0' &&
	    i < sizeof ( szStringFormat ); i++ ) {
      if ( szStringFormat [ i ] == '%' ) {
	bScannedPercent	= TRUE;
      }	else if ( bScannedPercent && szStringFormat [ i ] == 'c' ) {
	szStringFormat [ i ]	= 's';
	break;
      }
    }
    for ( i = 0; i < length ( NameEntries ); i++ ) {
      char szName [ 32 ];
      if ( ppszNames [ NameEntries [ i ].nAsciiCode ] != NULL ) {
	free ( (LPVOID) ppszNames [ NameEntries [ i ].nAsciiCode ] );
	ppszNames [ NameEntries [ i ].nAsciiCode ]	= NULL;
      }
      sprintf ( szName, szStringFormat, NameEntries [ i ].pszName );
      ppszNames [ NameEntries [ i ].nAsciiCode ]	= strdup ( szName );
    }
    for ( i = 0; i < length ( ppszNames ); i++ ) {
      char	szName [ 32 ];
      sprintf ( szName, lpClassInfo->lpszFormat, (char) i );
      if ( ppszNames [ i ] == NULL ) {
	ppszNames [ i ]	= strdup ( szName );
      }
    }
  }

  nCode	= fnObjId2Immediate ( oObjId, lpClassInfo->nTypeTag );
  if ( nCode >= 0 && nCode < length ( ppszNames ) ) {
    ASSERT ( ppszNames [ nCode ] != NULL );
    strncpy ( szBuffer, ppszNames [ nCode ], sizeof ( szBuffer ) );
  } else {
    sprintf ( szBuffer, lpClassInfo->lpszFormat, (char) nCode );
  }
  
  strncpy ( lpszBuffer, szBuffer, nBuffer );

  RETURN ( lpszBuffer );
} /* mfnPrintCharacter */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
BOOL		fnRegisterPlobClass	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  LPCLASSINFO lpClassInfo,
					  int nSizeOfClassInfo )
{
  LPCSTR	lpszNon;

  PROCEDURE	( fnRegisterPlobClass );

  INITIALIZEPLOB;
  ASSERT ( lpClassInfo );

  if ( nSizeOfClassInfo != sizeof ( *lpClassInfo ) ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( "Inconsistent state of module %s; please re-compile it.",
	       lpszFile ) );
    RETURN ( FALSE );
  }

  if ( ( ( lpClassInfo->nTypeFlags & typeImmediateP ) != 0 ) !=
       ( immediatep ( lpClassInfo->nTypeTag ) != 0 ) ) {
    lpszNon	= immediatep ( lpClassInfo->nTypeTag ) ? szEmpty : "non-";
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( "Wrong %simmediate type tag %d for class %s:"
	       " lower %d bits are %s0.", lpszNon,
	       lpClassInfo->nTypeTag, lpClassInfo->lpszTypeName,
	       nTagBits, lpszNon ) );
    RETURN ( FALSE );
  }

  RETURN ( fnRegisterClass ( lpszFile, lpszProc, nLine,
			     lpClassInfo->nTypeTag,
			     lpClassInfo->lpszTypeName,
			     lpClassInfo,
			     sizeof ( *lpClassInfo ) ) );
} /* fnRegisterPlobClass */

/* ----------------------------------------------------------------------- */
LPSTR		fnPrintImmediateObject	( FIXNUM	nState,
					  SHTYPETAG	nTypeTag,
					  LPSTR		lpszBuffer,
					  size_t	nBuffer )
{
  OBJID		oSelf;
  LPCLASSINFO	lpClassInfo;
  LPFNMETHOD	lpfnMethod;
  char		szBuffer [ 80 ];

  PROCEDURE	( fnPrintImmediateObject );
  INITIALIZEPLOB;
  ASSERT ( lpszBuffer != NULL );

  lpClassInfo	= (LPCLASSINFO) FindClassInfo ( nTypeTag );
  lpfnMethod	= _FindMethod ( nTypeTag, gfnPrintObjectDetails );

  *lpszBuffer	= '\0';

  if ( lpClassInfo != NULL ) {
    if ( lpfnMethod != NULL ) {
      SHTYPETAG	nTypeTagImmediate	= nTypeTag;
      oSelf	= fnImmediate2ObjId ( nState, &nTypeTagImmediate );
      ( * lpfnMethod ) ( oSelf, NULL, lpClassInfo, lpszBuffer, nBuffer );
    } else if ( lpClassInfo->lpszFormat ) {
      sprintf ( szBuffer, lpClassInfo->lpszFormat,
		nState );
      strncpy ( lpszBuffer, szBuffer, nBuffer );
    } else if ( lpClassInfo->lpszTypeName ) {
      sprintf ( szBuffer,
		UNREADABLE_OBJECT_PREFIX
		"%s=0x%X"
		UNREADABLE_OBJECT_SUFFIX,
		lpClassInfo->lpszTypeName,
		nState );
      strncpy ( lpszBuffer, szBuffer, nBuffer );
    }
  }

  if ( *lpszBuffer == '\0' ) {
    sprintf ( szBuffer,
	      UNREADABLE_OBJECT_PREFIX
	      "immediate-object=%d"
	      UNREADABLE_OBJECT_SUFFIX,
	      nState );
    strncpy ( lpszBuffer, szBuffer, nBuffer );
  }

  RETURN ( lpszBuffer );
} /* fnPrintImmediateObject */

/* ----------------------------------------------------------------------- */
LPCSTR		fnCompareTag2String	( COMPARETAG	eFrom,
					  BOOL		bSwap )
{
  PROCEDURE	( fnCompareTag2String );
  INITIALIZEPLOB;

  switch ( eFrom ) {
  case eshEqual:
      RETURN ( "==" );
  case eshEql:
    RETURN ( "eql" );
  case eshEq:
    RETURN ( "eq" );
  case eshNotEqual:
    RETURN ( "!=" );
  case eshNotEql:
    RETURN ( "not eql" );
  case eshNotEq:
    RETURN ( "not eq" );
  case eshLessEqual:
    RETURN ( ( bSwap ) ? ">=" : "<=" );
  case eshLess:
    RETURN ( ( bSwap ) ? ">" : "<" );
  case eshGreater:
    RETURN ( ( bSwap ) ? "<" : ">" );
  case eshGreaterEqual:
    RETURN ( ( bSwap ) ? "<=" : ">=" );
  default:
    break;
  }
  RETURN ( UNREADABLE_OBJECT_PREFIX "unknown compare tag"
	   UNREADABLE_OBJECT_SUFFIX );
} /* fnCompareTag2String */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnTypeTagName		( SHTYPETAG	eTypeTag )
{
  LPCLASSINFO	pClassInfo = NULL;
  LPCSTR	pszName = NULL;

  PROCEDURE	( fnTypeTagName );
  INITIALIZEPLOB;

  if ( eTypeTag == NULLTYPETAG ) {
    pszName	= UNREADABLE_OBJECT_PREFIX "null-type-tag"
      UNREADABLE_OBJECT_SUFFIX;
  } else {
    pClassInfo	= (LPCLASSINFO) FindClassInfo ( eTypeTag );
    if ( pClassInfo == NULL ) {
      /* 1998/11/11 HK: Multi-threading restriction: */
      static char	szBuffer [ 256 ];
      sprintf ( szBuffer, UNREADABLE_OBJECT_PREFIX "type-tag=0x%X"
		UNREADABLE_OBJECT_SUFFIX, eTypeTag );
      pszName	= szBuffer;
    } else {
      pszName	= pClassInfo->lpszTypeName;
    }
  }
  RETURN ( pszName );
} /* fnTypeTagName */

/* ----------------------------------------------------------------------- */
FIXNUM DLLEXPORT	fnTypeTagSizeValue	( FIXNUM	nObjects,
						  SHTYPETAG	* pnTypeTags,
						  FIXNUM	* pnElements )
{
  FIXNUM	i, nSizeValue = 0;
  LPCLASSINFO	pClassInfo = NULL;

  PROCEDURE	( fnTypeTagSizeValue );
  INITIALIZEPLOB;

  ASSERT ( pnTypeTags != NULL );
  ASSERT ( pnElements != NULL );

  for ( i = 0; i < nObjects; i++ ) {
    if ( pnTypeTags [ i ] != NULLTYPETAG && pnElements [ i ] > 0 ) {
      pClassInfo	= (LPCLASSINFO) FindClassInfo ( pnTypeTags [ i ] );
      if ( pClassInfo != NULL ) {
	nSizeValue	+=
	  AlignBitsToWords ( pClassInfo->nFixSizeValue * pnElements [ i ] );
      }
    }
  }

  RETURN ( nSizeValue );
} /* fnTypeTagSizeValueArray */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnShortObjIdValidP, "c-sh-objid-valid-p",
		( argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  INITIALIZEPLOB;
  if ( StableHeap_is_open ) {
    RETURN ( ObjId_is_valid ( SHORT2LONGOBJID ( oShortObjId ) ) );
  }
  RETURN ( FALSE );
} EndFunction ( fnShortObjIdValidP );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnLISPmapClassInfoFirst, "c-sh-map-class-info-first",
		( argument ( SHTYPETAG, value_out, pnTypeTag )
		  and
		  argument ( STRING ( nTypeName ),
			     vector_out, lpszTypeName )
		  and
		  argument ( FIXNUM, value_in, nTypeName )
		  and
		  argument ( FIXNUM, value_out, pnObjIdSize )
		  and
		  argument ( FIXNUM, value_out, pnValueSize)
		  and
		  argument ( TYPEFLAGS, value_out, pnTypeFlags ) ) )
{
  LPCLASSINFO	lpClassInfo;

  INITIALIZEPLOB;
  if ( oGlobalSession == NULLOBJID ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }

  if ( fnClassInfoFirst ( (LPCLASSTAG) NULL, (LPCSTR *) NULL,
			  (LPVOID FAR *) &lpClassInfo ) ) {
    if ( pnTypeTag != NULL ) {
      *pnTypeTag	= lpClassInfo->nTypeTag;
    }
    if ( lpszTypeName != NULL ) {
      strncpy ( lpszTypeName, lpClassInfo->lpszTypeName, nTypeName );
    }
    if ( pnObjIdSize != NULL ) {
      *pnObjIdSize	= lpClassInfo->nFixSizeObjId;
    }
    if ( pnValueSize != NULL ) {
      *pnValueSize	= lpClassInfo->nFixSizeValue;
    }
    if ( pnTypeFlags != NULL ) {
      *pnTypeFlags	= lpClassInfo->nTypeFlags;
    }
    RETURN ( TRUE );
  }
  RETURN ( FALSE );
} EndFunction ( fnLISPmapClassInfoFirst );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnLISPmapClassInfoNext, "c-sh-map-class-info-next",
		( argument ( SHTYPETAG, value_out, pnTypeTag )
		  and
		  argument ( STRING ( nTypeName ),
			     vector_out, lpszTypeName )
		  and
		  argument ( FIXNUM, value_in, nTypeName )
		  and
		  argument ( FIXNUM, value_out, pnObjIdSize )
		  and
		  argument ( FIXNUM, value_out, pnValueSize )
		  and
		  argument ( TYPEFLAGS, value_out, pnTypeFlags ) ) )
{
  LPCLASSINFO	lpClassInfo;

  INITIALIZEPLOB;
  if ( oGlobalSession == NULLOBJID ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }

  if ( fnClassInfoNext ( (LPCLASSTAG) NULL, (LPCSTR *) NULL,
			 (LPVOID FAR *) &lpClassInfo ) ) {
    if ( pnTypeTag != NULL ) {
      *pnTypeTag	= lpClassInfo->nTypeTag;
    }
    if ( lpszTypeName != NULL ) {
      strncpy ( lpszTypeName, lpClassInfo->lpszTypeName, nTypeName );
    }
    if ( pnObjIdSize != NULL ) {
      *pnObjIdSize	= lpClassInfo->nFixSizeObjId;
    }
    if ( pnValueSize != NULL ) {
      *pnValueSize	= lpClassInfo->nFixSizeValue;
    }
    if ( pnTypeFlags != NULL ) {
      *pnTypeFlags	= lpClassInfo->nTypeFlags;
    }
    RETURN ( TRUE );
  }
  RETURN ( FALSE );
} EndFunction ( fnLISPmapClassInfoNext );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
