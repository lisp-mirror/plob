/* -------------------------------------------------------------------------
| Module	splobnumber.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94 Derived from c-plob.c
| Description	PLOB various types
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

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
DLLEXPORTVAR const char	szALLEGRO []		= "ALLEGRO"; 
DLLEXPORTVAR const char	szALLEGRO4 []		= "ALLEGRO4"; 
DLLEXPORTVAR const char	szALLEGRO5 []		= "ALLEGRO5"; 
DLLEXPORTVAR const char	szALLEGRO6 []		= "ALLEGRO6"; 
DLLEXPORTVAR const char	szALLEGRO7 []		= "ALLEGRO7"; 
DLLEXPORTVAR const char	szLISPWORKS []		= "LISPWORKS";
DLLEXPORTVAR const char	szLISPWORKS3 []		= "LISPWORKS3";
DLLEXPORTVAR const char	szLISPWORKS4 []		= "LISPWORKS4";

DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro	= NULLOBJID;
DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro4	= NULLOBJID;
DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro5	= NULLOBJID;
DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro6	= NULLOBJID;
DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro7	= NULLOBJID;
DLLEXPORTVAR OBJID	oGlobalSymKeywordLispworks	= NULLOBJID;
DLLEXPORTVAR OBJID	oGlobalSymKeywordLispworks3	= NULLOBJID;
DLLEXPORTVAR OBJID	oGlobalSymKeywordLispworks4	= NULLOBJID;

/* -------------------------------------------------------------------------
| static types and constants
 ------------------------------------------------------------------------- */
static const char	szBignumMismatch []	=
"Expected format %s, received %s on\n"
"       %s";

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */
/* Object initialization methods: */
static BOOL	mfnInitSingleFloat( OBJID oObjId, LPOBJID lpSHvector,
				    LPCLASSINFO lpClassInfo );
static BOOL	mfnInitDoubleFloat( OBJID oObjId, LPOBJID lpSHvector,
				    LPCLASSINFO lpClassInfo );
static BOOL	mfnInitBignum	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
static BOOL	mfnInitRatio	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
static BOOL	mfnInitComplex	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
/* Sample prototype for mfnInit...-functions:
static BOOL	mfnInit	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
*/

/* Object print methods: */
static LPSTR	mfnPrintSingleFloat( OBJID oObjId, LPOBJID lpSHvector,
				     LPCLASSINFO lpClassInfo,
				     LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintDoubleFloat( OBJID oObjId, LPOBJID lpSHvector,
				     LPCLASSINFO lpClassInfo,
				     LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintBignum	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	fnPrintRorC	( LPSTR lpszBuffer, size_t nBuffer,
				  OBJID oLeft, OBJID oRight,
				  char cDivide );
static LPSTR	mfnPrintRatio	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintComplex	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );

/* Object equal methods: */
static double		fnObjId2Double		( LPCVOID	pSelf,
						  SHTYPETAG	nTypeTagSelf,
						  LPBOOL	lpbDone );
COMPARETAG		mfnNumberCompare	( LPVOID	pSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );

/* Object value methods: */
static FIXNUM		mfnBignumValues		( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );
static SHTYPETAG	mfnBignumValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeNumberModule	( void )
{
  PROCEDURE	( fnInitializeNumberModule );

  ASSERT ( sizeof ( float ) == eshSingleFloatValueSize * sizeof ( psint ) );
  ASSERT ( sizeof ( double ) == eshDoubleFloatValueSize * sizeof ( psint ) );

  /* Register methods: */
  RegisterMethod ( eshFixnumTag, gfnCompare, mfnNumberCompare );

  RegisterMethod ( eshShortFloatTag, gfnCompare, mfnNumberCompare );

  RegisterMethod ( eshSingleFloatTag, gfnInitializeInstance,
		   mfnInitSingleFloat );
  RegisterMethod ( eshSingleFloatTag, gfnPrintObjectDetails,
		   mfnPrintSingleFloat );
  RegisterMethod ( eshSingleFloatTag, gfnCompare, mfnNumberCompare );
  RegisterMethod ( eshSingleFloatTag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshSingleFloatTag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshDoubleFloatTag, gfnInitializeInstance,
		   mfnInitDoubleFloat );
  RegisterMethod ( eshDoubleFloatTag, gfnPrintObjectDetails,
		   mfnPrintDoubleFloat );
  RegisterMethod ( eshDoubleFloatTag, gfnCompare, mfnNumberCompare );
  RegisterMethod ( eshDoubleFloatTag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshDoubleFloatTag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshBignumTag, gfnInitializeInstance, mfnInitBignum );
  RegisterMethod ( eshBignumTag, gfnPrintObjectDetails, mfnPrintBignum );
  RegisterMethod ( eshBignumTag, gfnCompare, mfnNumberCompare );
  RegisterMethod ( eshBignumTag, gfnValues, mfnBignumValues );
  RegisterMethod ( eshBignumTag, gfnValueTypeTag,
		   mfnBignumValueTypeTag );

  RegisterMethod ( eshRatioTag, gfnInitializeInstance,
		   mfnInitRatio );
  RegisterMethod ( eshRatioTag, gfnPrintObjectDetails,
		   mfnPrintRatio );

  RegisterMethod ( eshComplexTag, gfnInitializeInstance,
		   mfnInitComplex );
  RegisterMethod ( eshComplexTag, gfnPrintObjectDetails,
		   mfnPrintComplex );

  RegisterMethod ( eshDynCFloatPtrTag, gfnCompare, mfnNumberCompare );

  RegisterMethod ( eshDynCDoublePtrTag, gfnCompare, mfnNumberCompare );

  RETURN ( VOID );
} /* fnInitializeNumberModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeNumberModule	( void )
{
  PROCEDURE	( fnDeinitializeNumberModule );

  RETURN ( VOID );
} /* fnDeinitializeNumberModule */

/* -------------------------------------------------------------------------
| Static function
 ------------------------------------------------------------------------- */
static void	fnLocateSymbols ( void )
{
  PROCEDURE	( fnLocateSymbols );

  find_symbol ( &oGlobalSymKeywordAllegro,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szALLEGRO );
  find_symbol ( &oGlobalSymKeywordAllegro4,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szALLEGRO4 );
  find_symbol ( &oGlobalSymKeywordAllegro5,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szALLEGRO5 );
  find_symbol ( &oGlobalSymKeywordAllegro6,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szALLEGRO6 );
  find_symbol ( &oGlobalSymKeywordAllegro7,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szALLEGRO7 );

  find_symbol ( &oGlobalSymKeywordLispworks,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szLISPWORKS );
  find_symbol ( &oGlobalSymKeywordLispworks3,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szLISPWORKS3 );
  find_symbol ( &oGlobalSymKeywordLispworks4,
		find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		szLISPWORKS4 );

  RETURN ( VOID );
} /* fnLocateSymbols */

/* -------------------------------------------------------------------------
| Object initialization methods
 ------------------------------------------------------------------------- */
static BOOL	mfnInitSingleFloat( OBJID oObjId, LPOBJID lpSHvector,
				    LPCLASSINFO lpClassInfo )
{
  static float	fZero	= 0.0;

  PROCEDURE	( mfnInitSingleFloat );

  memcpy ( & lpSHvector [ eshSHvectorIdxFirstData ], &fZero,
	   sizeof ( fZero ) );
  makdependent ( oObjId, flagDependentRead );
	   
  RETURN ( TRUE );
} /* mfnInitSingleFloat */

/* ----------------------------------------------------------------------- */
static BOOL	mfnInitDoubleFloat( OBJID oObjId, LPOBJID lpSHvector,
				    LPCLASSINFO lpClassInfo )
{
  static double	fZero	= 0.0;

  PROCEDURE	( mfnInitDoubleFloat );

  memcpy ( & lpSHvector [ eshSHvectorIdxFirstData ], &fZero,
	   sizeof ( fZero ) );
  makdependent ( oObjId, flagDependentRead );
	   
  RETURN ( TRUE );
} /* mfnInitDoubleFloat */

/* ----------------------------------------------------------------------- */
static BOOL	mfnInitBignum	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitBignum );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  lpSHvector [ Cooked2RawIndex ( eshBignumIdxSize ) ]	= o0;
  makdependent ( oObjId, flagDependentRead );

  RETURN ( TRUE );
} /* mfnInitBignum */

/* ----------------------------------------------------------------------- */
static BOOL	mfnInitRatio	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitRatio );

  lpSHvector [ Cooked2RawIndex ( eshRatioIdxNumerator ) ]	= o0;
  lpSHvector [ Cooked2RawIndex ( eshRatioIdxDenominator ) ]	= o1;
  makdependent ( oObjId, flagDependentRead );

  RETURN ( TRUE );
} /* mfnInitRatio */

/* ----------------------------------------------------------------------- */
static BOOL	mfnInitComplex	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitComplex );

  lpSHvector [ Cooked2RawIndex ( eshComplexIdxRealPart ) ]	= o0;
  lpSHvector [ Cooked2RawIndex ( eshComplexIdxImagPart ) ]	= o0;
  makdependent ( oObjId, flagDependentRead );

  RETURN ( TRUE );
} /* mfnInitComplex */

/* -------------------------------------------------------------------------
| Object print methods
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintSingleFloat( OBJID oObjId, LPOBJID lpSHvector,
				     LPCLASSINFO lpClassInfo,
				     LPSTR lpszBuffer, size_t nBuffer )
{
  float		fSingleFloat;

  PROCEDURE	( mfnPrintSingleFloat );

  memcpy ( (void *) &fSingleFloat,
	   (void *) & lpSHvector [ eshSHvectorIdxFirstData ],
	   sizeof ( fSingleFloat ) );
  RETURN ( fnPrintFloat ( lpszBuffer, fSingleFloat,
			  cExponentSingleFloat ) );
} /* mfnPrintSingleFloat */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintDoubleFloat( OBJID oObjId, LPOBJID lpSHvector,
				     LPCLASSINFO lpClassInfo,
				     LPSTR lpszBuffer, size_t nBuffer )
{
  double	fDoubleFloat;

  PROCEDURE	( mfnPrintDoubleFloat );

  memcpy ( (void *) &fDoubleFloat,
	   (void *) & lpSHvector [ eshSHvectorIdxFirstData ],
	   sizeof ( fDoubleFloat ) );
  RETURN ( fnPrintFloat ( lpszBuffer, fDoubleFloat,
			  cExponentDoubleFloat ) );
} /* mfnPrintDoubleFloat */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintBignum	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  OBJID		oSize, oFormat;
  int		i, j, n, b;
  unsigned int	d;
  BOOL		bNegative, bLeading0 = FALSE;
  LPCSTR	lpszFormat;
  LPBYTE	lpbSHvector;
  char		szBuffer [ 512 ], szDetails [ 256 ];

  PROCEDURE	( mfnPrintBignum );

  fnLocateSymbols ();
  oSize		= lpSHvector [ Cooked2RawIndex ( eshBignumIdxSize ) ];
  oFormat	= lpSHvector [ Cooked2RawIndex ( eshBignumIdxFormat ) ];

  bNegative	= (BOOL) ( OBJID2FIXNUM ( oSize ) < 0 );

  if ( oFormat == oGlobalSymKeywordAllegro ||
       oFormat == oGlobalSymKeywordAllegro4 ) {
    /* Print an Allegro 4.x bignum: */
    bLeading0	= TRUE;
    strcpy ( lpszBuffer, "#x" );
    i		= strlen ( lpszBuffer );
    n		= ABS ( OBJID2FIXNUM ( oSize ) ) / nBitsPerBigit - 1;
    j		=
      ( ( lpSHvector [ eshSHvectorIdxSize ] -
	  eshSHvectorIdxFirstData - eshBignumObjIdSize ) *
	nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerBigit - 1;
    if ( n > j ) {
      n		= j;
    }
    lpbSHvector = (LPBYTE)
      & ( (LPBYTE)lpSHvector ) [ ( ( eshSHvectorIdxFirstData +
				     eshBignumObjIdSize ) *
				   nSizeOfPostoreWord * nBitsPerByte +
				   n * nBitsPerBigit ) / nBitsPerByte ];
    for ( j = n;
	  i < nBuffer && j >= 0;
	  j--, lpbSHvector -= nBitsPerBigit / nBitsPerByte ) {
      for ( b = 0; b < nBitsPerBigit / nBitsPerByte; b++ ) {
	d	= lpbSHvector [ b ];
	if ( d || ! bLeading0 ) {
	  if ( bLeading0 ) {
	    lpszFormat	= "%X";
	    if ( bNegative ) {
	      lpszBuffer [ i++ ]	= '-';
	    }
	  }
	  bLeading0	= FALSE;
	  sprintf ( & lpszBuffer [ i ], lpszFormat, d );
	  i		+= strlen ( & lpszBuffer [ i ] );
	  lpszFormat	= "%02.2X";
	}
      }
    }
  } else if ( oFormat == oGlobalSymKeywordAllegro5 ||
	      /* 2005-05-10 hkirschk: Not yet tested for Allegro 6 & 7: */
	      oFormat == oGlobalSymKeywordAllegro6 ||
	      oFormat == oGlobalSymKeywordAllegro7 ) {
    /* Print an Allegro 5.x bignum: */
    bLeading0	= TRUE;
    strcpy ( lpszBuffer, "#x" );
    i		= strlen ( lpszBuffer );
    n		= ABS ( OBJID2FIXNUM ( oSize ) ) / nBitsPerByte - 1;
    j		=
      ( ( lpSHvector [ eshSHvectorIdxSize ] -
	  eshSHvectorIdxFirstData - eshBignumObjIdSize ) *
	nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerByte - 1;
    if ( n > j ) {
      n		= j;
    }
    lpbSHvector = (LPBYTE)
      & ( (LPBYTE)lpSHvector ) [ ( ( eshSHvectorIdxFirstData +
				     eshBignumObjIdSize ) *
				   nSizeOfPostoreWord * nBitsPerByte +
				   n * nBitsPerByte ) / nBitsPerByte ];
    for ( j = n;
	  i < nBuffer && j >= 0;
	  j--, lpbSHvector-- ) {
      d	= *lpbSHvector;
      if ( d || ! bLeading0 ) {
	if ( bLeading0 ) {
	  lpszFormat	= "%X";
	  if ( bNegative ) {
	    lpszBuffer [ i++ ]	= '-';
	  }
	}
	bLeading0	= FALSE;
	sprintf ( & lpszBuffer [ i ], lpszFormat, d );
	i		+= strlen ( & lpszBuffer [ i ] );
	lpszFormat	= "%02.2X";
      }
    }
  } else if ( oFormat == oGlobalSymKeywordLispworks ||
	      oFormat == oGlobalSymKeywordLispworks3 ||
	      oFormat == oGlobalSymKeywordLispworks4 ) {
    /* Print a LispWorks 3.x bignum: */
    bLeading0	= TRUE;
    strcpy ( lpszBuffer, "#x" );
    i		= strlen ( lpszBuffer );
    n		= eshSHvectorIdxFirstData + eshBignumObjIdSize - 1 +
      ABS ( OBJID2FIXNUM ( oSize ) ) / ( nSizeOfPostoreWord * nBitsPerByte );
    if ( n > lpSHvector [ eshSHvectorIdxSize ] - 1 ) {
      n		= lpSHvector [ eshSHvectorIdxSize ] - 1;
    }
    /* The format analysed and printed here is the LispWorks bignum format:
       the least significant word contains the sign of the bignum in 2's
       complement; all other words are in 1's complement: */
    for ( j = n; i < nBuffer && j >= eshSHvectorIdxFirstData +
	    eshBignumObjIdSize; j-- ) {
      if ( bNegative ) {
	d	= ( j > eshSHvectorIdxFirstData + eshBignumObjIdSize ) ?
	  ~ lpSHvector [ j ] : - (int) lpSHvector [ j ];
      } else {
	d	= lpSHvector [ j ];
      }
      if ( d || ! bLeading0 ) {
	if ( bLeading0 ) {
	  lpszFormat	= "%X";
	  if ( bNegative ) {
	    lpszBuffer [ i++ ]	= '-';
	  }
	}
	bLeading0	= FALSE;
	sprintf ( & lpszBuffer [ i ], lpszFormat, d );
	i		+= strlen ( & lpszBuffer [ i ] );
	lpszFormat	= "%08.8X";
      }
    }
  } else {
    /* Unknown bignum format: */
    sprintf ( szBuffer, "%s %d",
	      gfnPrintObjectDetails ( oFormat, szDetails,
				      sizeof ( szDetails ) ),
	      OBJID2FIXNUM ( oSize ) );
    strncpy ( lpszBuffer, szBuffer, nBuffer );
  }
  if ( bLeading0 ) {
    strcpy ( lpszBuffer, "0" );
  }
  RETURN ( lpszBuffer );
} /* mfnPrintBignum */

/* ----------------------------------------------------------------------- */
static LPSTR	fnPrintRorC	( LPSTR lpszBuffer, size_t nBuffer,
				  OBJID oLeft, OBJID oRight,
				  char cDivide )
{
  char		szLeft [ 64 ], szRight [ 64 ];
  LPOBJID	lpSelf;

  PROCEDURE	( fnPrintRorC );

  if ( fixnump ( oLeft ) ) {
    sprintf ( szLeft, "%d", OBJID2FIXNUM ( oLeft ) );
  } else if ( shortfloatp ( oLeft ) ) {
    fnPrintFloat ( szLeft, OBJID2SINGLEFLOAT ( oLeft ), cExponentShortFloat );
  } else if ( singlefloatp ( oLeft ) ) {
    float	fSingleFloat;
    lpSelf	= SH_key_to_address ( oLeft );
    ASSERT ( lpSelf != NULL );
    memcpy ( (void *) &fSingleFloat,
	     (void *) & lpSelf [ eshSHvectorIdxFirstData ],
	     sizeof ( fSingleFloat ) );
    fnPrintFloat ( szLeft, fSingleFloat, cExponentSingleFloat );
  } else if ( doublefloatp ( oLeft ) ) {
    double	fDoubleFloat;
    lpSelf	= SH_key_to_address ( oLeft );
    ASSERT ( lpSelf != NULL );
    memcpy ( (void *) &fDoubleFloat,
	     (void *) & lpSelf [ eshSHvectorIdxFirstData ],
	     sizeof ( fDoubleFloat ) );
    fnPrintFloat ( szLeft, fDoubleFloat, cExponentDoubleFloat );
  } else {
    szLeft [ 0 ]	= '\0';
  }

  if ( fixnump ( oRight ) ) {
    sprintf ( szRight, "%d", OBJID2FIXNUM ( oRight ) );
  } else if ( shortfloatp ( oRight ) ) {
    fnPrintFloat ( szRight, OBJID2SINGLEFLOAT ( oRight ),
		   cExponentShortFloat );
  } else if ( singlefloatp ( oRight ) ) {
    float	fSingleFloat;
    lpSelf	= SH_key_to_address ( oRight );
    ASSERT ( lpSelf != NULL );
    memcpy ( (void *) &fSingleFloat,
	     (void *) & lpSelf [ eshSHvectorIdxFirstData ],
	     sizeof ( fSingleFloat ) );
    fnPrintFloat ( szRight, fSingleFloat, cExponentSingleFloat );
  } else if ( doublefloatp ( oRight ) ) {
    double	fDoubleFloat;
    lpSelf	= SH_key_to_address ( oRight );
    ASSERT ( lpSelf != NULL );
    memcpy ( (void *) &fDoubleFloat,
	     (void *) & lpSelf [ eshSHvectorIdxFirstData ],
	     sizeof ( fDoubleFloat ) );
    fnPrintFloat ( szRight, fDoubleFloat, cExponentDoubleFloat );
  } else {
    szRight [ 0 ]	= '\0';
  }

  if ( szLeft [ 0 ] && szRight [ 0 ] ) {
    sprintf ( lpszBuffer, "%s%c%s", szLeft, cDivide, szRight );
  } else if ( szLeft [ 0 ] ) {
    strncpy ( lpszBuffer, szLeft, nBuffer );
  } else if ( szRight [ 0 ] ) {
    strncpy ( lpszBuffer, szRight, nBuffer );
  } else if ( typetagof ( oRight ) == typetagof ( oLeft ) ) {
    strncpy ( lpszBuffer, fnTypeString ( typetagof ( oLeft ) ),
	      nBuffer );
  } else {
    sprintf ( lpszBuffer, "%s%c%s",
	      fnTypeString ( typetagof ( oLeft ) ),
	      cDivide,
	      fnTypeString ( typetagof ( oRight ) ) );
  }

  RETURN ( lpszBuffer );
} /* fnPrintRorC */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintRatio	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  PROCEDURE	( mfnPrintRatio );

  fnPrintRorC ( lpszBuffer, nBuffer,
	        lpSHvector [ Cooked2RawIndex ( eshRatioIdxNumerator ) ],
	        lpSHvector [ Cooked2RawIndex ( eshRatioIdxDenominator ) ],
	        '/' );
  RETURN ( lpszBuffer );
} /* mfnPrintRatio */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintComplex	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  PROCEDURE	( mfnPrintComplex );

  fnPrintRorC ( lpszBuffer, nBuffer,
	        lpSHvector [ Cooked2RawIndex ( eshComplexIdxRealPart ) ],
	        lpSHvector [ Cooked2RawIndex ( eshComplexIdxImagPart ) ],
	        ' ' );
  RETURN ( lpszBuffer );
} /* mfnPrintComplex */

/* -------------------------------------------------------------------------
| Object value methods
 ------------------------------------------------------------------------- */
static FIXNUM		mfnBignumValues		( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  FIXNUM	nValues = 0;
  FIXNUM	nValuesInWords = 0;
  LPOBJID	pBignum = NULL;

  PROCEDURE	( mfnBignumValues );

  pBignum		= SH_key_to_address ( oSelf );
  nValuesInWords	= pBignum [ eshSHvectorIdxSize ] -
    pBignum [ eshSHvectorIdxObjIds ] - eshSHvectorIdxFirstObjId;
  nValues		= nValuesInWords * nSizeOfPostoreWord;

  RETURN ( nValues );
} /* mfnBignumValues */

/* ----------------------------------------------------------------------- */
static SHTYPETAG	mfnBignumValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  SHTYPETAG	eValueTypeTag = NULLTYPETAG;

  PROCEDURE	( mfnBignumValueTypeTag );

  eValueTypeTag	= eshUnsignedByte8Tag;

  RETURN ( eValueTypeTag );
} /* mfnBignumValueTypeTag */

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
static double		fnObjId2Double		( LPCVOID	pSelf,
						  SHTYPETAG	nTypeTagSelf,
						  LPBOOL	lpbDone )
{
  LPOBJID	pNumber;
  float		fSingle;
  double	fReturn	= 0.0;

  PROCEDURE	( fnObjId2Double );

  if ( lpbDone ) {
    *lpbDone	= TRUE;
  }
  switch ( nTypeTagSelf ) {
  case eshFixnumTag:
    fReturn	= ObjId2Fixnum ( * (LPOBJID) pSelf );
    break;
  case eshShortFloatTag:
    fReturn	= ObjId2SingleFloat ( * (LPOBJID) pSelf );
    break;
  case eshSingleFloatTag:
    ASSERT_ObjId_is_valid ( * (LPOBJID) pSelf );
    pNumber	= SH_key_to_address ( * (LPOBJID) pSelf );
    ASSERT ( pNumber != NULL );
    memcpy ( (void *) &fSingle, (void *) & pNumber [ eshSHvectorIdxFirstData ],
	     sizeof ( float ) );
    fReturn	= fSingle;
    break;
  case eshDoubleFloatTag:
    ASSERT_ObjId_is_valid ( * (LPOBJID) pSelf );
    pNumber	= SH_key_to_address ( * (LPOBJID) pSelf );
    ASSERT ( pNumber != NULL );
    ASSERT_TYPE ( * (LPOBJID) pSelf, pNumber, eshDoubleFloatTag );
    memcpy ( (void *) &fReturn, (void *) & pNumber [ eshSHvectorIdxFirstData ],
	     sizeof ( double ) );
    break;
  case eshDynCFloatPtrTag:
    ASSERT ( pSelf != NULL );
    fReturn	= * (float *) pSelf;
    break;
  case eshDynCDoublePtrTag:
    ASSERT ( pSelf != NULL );
    fReturn	= * (double *) pSelf;
    break;
  default:
    if ( lpbDone ) {
      *lpbDone	= FALSE;
    }
    break;
  }
  RETURN ( fReturn );
} /* fnObjId2Double */

/* -------------------------------------------------------------------------
| Compare method
 ------------------------------------------------------------------------- */
COMPARETAG		mfnNumberCompare	( LPVOID	pSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  BOOL		bSelfIntP, bDone;
  int		nSelf, nCompare;
  double	fSelf, fCompare;

  PROCEDURE	( mfnNumberCompare );

  if ( markerp ( oCompare ) ) {
    if ( maxmarkerp ( oCompare ) ) {
      RETURN ( eshLess );
    } else if ( minmarkerp ( oCompare ) ) {
      RETURN ( eshGreater );
    } else if ( matchanymarkerp ( oCompare ) ) {
      RETURN ( eshEqual );
    } else if ( matchnevermarkerp ( oCompare ) ) {
      RETURN ( eshNotEqual );
    }
    RETURN ( eshNotEq );
  }

  bSelfIntP	= (BOOL)
    ( eTypeTagSelf == eshFixnumTag || eTypeTagSelf == eshBignumTag );
  if ( ! bSelfIntP || ! integerp ( oCompare ) ) {
    /* Do a float compare: */
    bDone	= FALSE;
    fSelf	= fnObjId2Double ( pSelf, eTypeTagSelf, &bDone );
    if ( ! bDone ) {
      RETURN ( eshNotEqual );
    }
    bDone	= FALSE;
    fCompare	= fnObjId2Double ( &oCompare, typetagof ( oCompare ), &bDone );
    if ( ! bDone ) {
      RETURN ( eshNotEqual );
    }
    if ( fSelf < fCompare ) {
      RETURN ( eshLess );
    }
    if ( fSelf > fCompare ) {
      RETURN ( eshGreater );
    }
    RETURN ( eshEqual );
  }
  if ( eTypeTagSelf == eshFixnumTag && fixnump ( oCompare ) ) {
    /* Do a integer compare: */
    nSelf	= ObjId2Fixnum ( * (LPOBJID) pSelf );
    nCompare	= OBJID2FIXNUM ( oCompare );
    if ( nSelf < nCompare ) {
      RETURN ( eshLess );
    }
    if ( nSelf > nCompare ) {
      RETURN ( eshGreater );
    }
    RETURN ( eshEqual );
  }
  RETURN ( eshNotEqual );
} /* mfnNumberCompare */

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMakeFloat		( float fFrom )
{
  OBJID		oFloat;
  LPOBJID	lpFloat;

  PROCEDURE	( fnMakeFloat );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  oFloat	= fnCreateObject ( eshSingleFloatTag, 0, NULLTYPETAG, 0 );
  ASSERT ( oFloat != NULLOBJID );
  lpFloat	= SH_key_to_address ( oFloat );
  ASSERT ( lpFloat != NULL );
  memcpy ( & lpFloat [ eshSHvectorIdxFirstData ],
	   &fFrom, sizeof ( fFrom ) );
  RETURN ( oFloat );
} /* fnMakeFloat */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMakeDouble		( double fFrom )
{
  OBJID		oDouble;
  LPOBJID	lpDouble;

  PROCEDURE	( fnMakeDouble );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  oDouble	= fnCreateObject ( eshDoubleFloatTag, 0, NULLTYPETAG, 0 );
  ASSERT ( oDouble != NULLOBJID );
  lpDouble	= SH_key_to_address ( oDouble );
  ASSERT ( lpDouble != NULL );
  memcpy ( & lpDouble [ eshSHvectorIdxFirstData ],
	   &fFrom, sizeof ( fFrom ) );
  RETURN ( oDouble );
} /* fnMakeDouble */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnServerDbMakeBignum, "c-sh-make-bignum",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdFormat )
		  and
		  argument ( FIXNUM, value_in, nSizeInBits )
		  and
		  argument ( VECTOR
			     ( as_is,
			       AlignBitsToWords ( ABS ( nSizeInBits ) ) ),
			     vector_in, pnBignum ) ) )
{
  OBJID		oBignum;
  SHORTOBJID	oShortBignum = NULLOBJID;
  psint		*pnBignumNew;
  int		nSize, nWritten;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( NULLOBJID );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBignum	=
    fnCreateObject ( eshBignumTag, 0, eshUnsignedByte1Tag,
		     ABS ( nSizeInBits ) );
  if ( boundp ( oBignum ) ) {
    oShortBignum	= LONG2SHORTOBJID ( oBignum );
    pnBignumNew		= SH_key_to_address ( oBignum );
    ASSERT ( pnBignumNew != NULL );
    if ( oShortObjIdFormat != NULLOBJID ) {
      pnBignumNew [ Cooked2RawIndex ( eshBignumIdxFormat ) ]	=
	Short2LongObjId ( oShortObjIdFormat );
    }
    pnBignumNew [ Cooked2RawIndex ( eshBignumIdxSize ) ]	=
      Fixnum2ObjId ( nSizeInBits );
    memcpy ( & pnBignumNew [ eshSHvectorIdxFirstData + eshBignumObjIdSize ],
	     pnBignum,
	     AlignBitsToWords ( ABS ( nSizeInBits ) ) * nSizeOfPostoreWord );
  }

  UnstoreSession ();
  RETURN ( oShortBignum );
} EndFunction ( fnServerDbMakeBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnShortMakeSingleFloat, "c-sh-make-single-float",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SINGLE_FLOAT, value_in, fFrom ) ) )
{
  SHORTOBJID	oShortFloat;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( NULLOBJID );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oShortFloat	= LONG2SHORTOBJID ( make_float ( fFrom ) );

  UnstoreSession ();
  RETURN ( oShortFloat );
} EndFunction ( fnShortMakeSingleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnShortMakeDoubleFloat, "c-sh-make-double-float",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fFrom ) ) )
{
  SHORTOBJID	oShortFloat;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( NULLOBJID );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oShortFloat	= LONG2SHORTOBJID ( make_double ( fFrom ) );

  UnstoreSession ();
  RETURN ( oShortFloat );
} EndFunction ( fnShortMakeDoubleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerObjectReadBignum, "c-sh-read-bignum",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdFormat )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( FIXNUM, value_in, nSizeInBits )
		  and
		  argument ( VECTOR
			     ( as_is,
			       AlignBitsToWords ( ABS ( nSizeInBits ) ) ),
			     vector_out, pnBignum ) ) )
{
  OBJID		oBignum, oSize;
  OBJID		oFormat = NULLOBJID, oFormatRead = NULLOBJID;
  char		szFormat [ 256 ], szFormatRead  [ 256 ];
  int		nSize, nRead;
  psint		*pnBignumRead;
  SHTYPETAG	t;
  FIXNUM	e;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBignum	= Short2LongObjId ( oShortObjId );
  pnBignumRead	= SH_key_to_address ( oBignum );
  ASSERT ( pnBignumRead != NULL );
  oFormatRead	= pnBignumRead [ Cooked2RawIndex ( eshBignumIdxFormat ) ];
  if ( oShortObjIdFormat != NULLOBJID ) {
    oFormat	= Short2LongObjId ( oShortObjIdFormat );
  }
  if ( boundp ( oFormatRead ) && boundp ( oFormat ) &&
       oFormatRead != oFormat ) {
    /* HK 1997/04/07: Better solution would be to convert between the
       bignum formats; left for future expansion: */
    ERROR (( szBignumMismatch,
	     gfnPrintObjectDetails ( oFormat, szFormat,
				     sizeof ( szFormat ) ),
	     gfnPrintObjectDetails ( oFormatRead, szFormatRead,
				     sizeof ( szFormatRead ) ),
	     fnPrintObject ( oBignum, (LPSTR) NULL, 0 ) ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }
  oSize		= pnBignumRead [ Cooked2RawIndex ( eshBignumIdxSize ) ];
  nSize		= ObjId2Fixnum ( oSize );
  nSize		= MIN ( ABS ( nSize ), ABS ( nSizeInBits ) );
  nRead		=
    fnServerObjectReadValues ( oShortObjIdHeap, oShortObjId,
			       (SHORTOBJID) NULLOBJID, eshBignumTag, 0,
			       eshUnsignedByte1Tag, nSize, &t, &e,
			       pnBignum );

  UnstoreSession ();
  RETURN ( (SHLOCK) nRead );
} EndFunction ( fnServerObjectReadBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerObjectReadDoubleFloat, "c-sh-read-double-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( DOUBLE_FLOAT, value_out,
			     pfDoubleFloat ) ) )
{
  SHLOCK	nLockOld;
  SHTYPETAG	t;
  FIXNUM	e;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nLockOld	= (SHLOCK)
    fnServerObjectReadValues ( oShortObjIdHeap, oShortObjId,
			       (SHORTOBJID) NULLOBJID, eshDoubleFloatTag,
			       0, eshDoubleFloatTag, 1, &t, &e,
			       (int *) pfDoubleFloat );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectReadDoubleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerObjectReadFixnum, "c-sh-read-fixnum",
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
		  argument ( INTEGER, value_out, pnFixnum ) ) )
{
  SHTYPETAG	nTypeTag;
  SHLOCK	nLockOld;
  OBJID		oObjId;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nTypeTag	= eshFixnumTag;
  nLockOld	=
    fnServerObjectReadAtIndex ( oShortObjIdHeap, oShortObjId,
				oExpectingClass, nExpectingTypeTag,
				nIndex, pnFixnum, &nTypeTag );
  if ( nTypeTag != eshFixnumTag ) {
    oObjId	= Short2LongObjId ( oShortObjId );
    UNEXPECTED_TYPE_TAG ( oObjId, nIndex, eshFixnumTag );
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectReadFixnum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerObjectReadSingleFloat, "c-sh-read-single-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SINGLE_FLOAT, value_out, pfSingleFloat ) ) )
{
  SHLOCK	nLockOld;
  SHTYPETAG	t;
  FIXNUM	e;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nLockOld	= (SHLOCK)
    fnServerObjectReadValues ( oShortObjIdHeap, oShortObjId,
			       (SHORTOBJID) NULLOBJID, eshSingleFloatTag,
			       0, eshSingleFloatTag, 1, &t, &e,
			       (int *) pfSingleFloat );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectReadSingleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerObjectWriteBignum, "c-sh-write-bignum",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdFormat )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( FIXNUM, value_in, nSizeInBits )
		  and
		  argument ( VECTOR
			     ( as_is,
			       AlignBitsToWords ( ABS ( nSizeInBits ) ) ),
			     vector_in, pnBignum ) ) )
{
  OBJID		oBignum, oSize;
  OBJID		oFormat = NULLOBJID, oFormatWrite = NULLOBJID;
  char		szFormat [ 256 ], szFormatWrite  [ 256 ];
  int		nSize, nWritten;
  psint		*pnBignumWrite;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oBignum	= Short2LongObjId ( oShortObjId );
  pnBignumWrite	= SH_key_to_address ( oBignum );
  ASSERT ( pnBignumWrite != NULL );
  oFormatWrite	= pnBignumWrite [ Cooked2RawIndex ( eshBignumIdxFormat ) ];
  if ( oShortObjIdFormat != NULLOBJID ) {
    oFormat	= Short2LongObjId ( oShortObjIdFormat );
  }
  if ( boundp ( oFormatWrite ) && boundp ( oFormat ) &&
       oFormatWrite != oFormat ) {
    /* HK 1997/04/07: Better solution would be to convert between the
       bignum formats; left for future expansion: */
    ERROR (( szBignumMismatch,
	     gfnPrintObjectDetails ( oFormat, szFormat,
				     sizeof ( szFormat ) ),
	     gfnPrintObjectDetails ( oFormatWrite, szFormatWrite,
				     sizeof ( szFormatWrite ) ),
	     fnPrintObject ( oBignum, (LPSTR) NULL, 0 ) ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }
  oSize		= pnBignumWrite [ Cooked2RawIndex ( eshBignumIdxSize ) ];
  nSize		= ObjId2Fixnum ( oSize );
  nSize		= MIN ( ABS ( nSize ), ABS ( nSizeInBits ) );
  nWritten	=
    fnServerObjectWriteValues ( oShortObjIdHeap, oShortObjId,
				(SHORTOBJID) NULLOBJID, eshBignumTag, 0,
				eshUnsignedByte1Tag, nSize, pnBignum );

  UnstoreSession ();
  RETURN ( (SHLOCK) nWritten );
} EndFunction ( fnServerObjectWriteBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerObjectWriteDoubleFloat, "c-sh-write-double-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fDoubleFloat ) ) )
{
  SHLOCK	nLockOld;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nLockOld	= (SHLOCK)
    fnServerObjectWriteValues ( oShortObjIdHeap, oShortObjId,
				(SHORTOBJID) NULLOBJID, eshDoubleFloatTag,
				0, eshDoubleFloatTag, 1,
				(int *) &fDoubleFloat );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectWriteDoubleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerObjectWriteFixnum, "c-sh-write-fixnum",
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
		  argument ( FIXNUM, value_in, nFixnumWrite ) ) )
{
  SHLOCK	nLockOld;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nLockOld	=
    fnServerObjectWriteAtIndex ( oShortObjIdHeap, oShortObjId,
				 oExpectingClass, nExpectingTypeTag,
				 nIndex, nFixnumWrite,
				 eshFixnumTag );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectWriteFixnum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerObjectWriteSingleFloat, "c-sh-write-single-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SINGLE_FLOAT, value_in, fSingleFloat ) ) )
{
  SHLOCK	nLockOld;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nLockOld	= (SHLOCK)
    fnServerObjectWriteValues ( oShortObjIdHeap, oShortObjId,
				(SHORTOBJID) NULLOBJID, eshSingleFloatTag,
				0, eshSingleFloatTag, 1,
				(int *) &fSingleFloat );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectWriteSingleFloat );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
