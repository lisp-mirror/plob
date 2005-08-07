/* -------------------------------------------------------------------------
| Module	splobsequ.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		4.3.94
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

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show some messages what's happening: */
#define	LOGGING	0

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Error message formats
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Static method declarations
 ------------------------------------------------------------------------- */
/* Object initialization methods: */
static BOOL	mfnInitIVector	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
static BOOL	mfnInitArray	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
static BOOL	mfnInitString	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );
static BOOL	mfnInitBitVector( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );

/* Object print methods: */
static LPSTR	mfnPrintCons	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintArray	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintString	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintBitVector( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintVector	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );
static LPSTR	mfnPrintIVector	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );

/* Object name methods: */
static LPSTR		mfnStringNameOf		( OBJID oSelf,
						  LPINT lpnName );

/* Object equal methods: */
static COMPARETAG	mfnSequenceCompare	( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );
static COMPARETAG	mfnStringCompare	( LPVOID	pSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );

/* Object value methods: */
static FIXNUM		mfnStringValues		( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );
static FIXNUM		mfnBitVectorValues	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );
static FIXNUM		mfnIVectorValues	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );
static SHTYPETAG	mfnStringValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );
static SHTYPETAG	mfnBitVectorValueTypeTag( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );
static SHTYPETAG	mfnIVectorValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );

/* -------------------------------------------------------------------------
| Static function declarations
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitializeSequModule		( void )
{
  PROCEDURE	( fnInitializeSequModule );

  /* Make sure that the struct's components offsets match the sh-vector
     indices. If one of the following ASSERTs fails, the eshSHvectorIdx...-
     constants have been modified without reflecting these modifications
     in the corresponding structs PLOBSTRING (or vice versa): */
  ASSERT ( Offset_matches_Index ( PLOBSTRING, oLength,
				  Cooked2RawIndex ( eshStringIdxLength ) ) );

  /* Register methods: */
  RegisterMethod ( eshSignedByte2Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshSignedByte2Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshSignedByte4Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshSignedByte4Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshSignedByte8Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshSignedByte8Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshSignedByte16Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshSignedByte16Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshSignedByte32Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshSignedByte32Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshUnsignedByte1Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshUnsignedByte1Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshUnsignedByte2Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshUnsignedByte2Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshUnsignedByte4Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshUnsignedByte4Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshUnsignedByte8Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshUnsignedByte8Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshUnsignedByte16Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshUnsignedByte16Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshUnsignedByte32Tag, gfnValues, mfnStandardValues );
  RegisterMethod ( eshUnsignedByte32Tag, gfnValueTypeTag,
		   mfnStandardValueTypeTag );

  RegisterMethod ( eshConsTag, gfnInitializeInstance,
		   mfnInitStandard );
  RegisterMethod ( eshConsTag, gfnPrintObjectDetails,
		   mfnPrintCons );
  RegisterMethod ( eshConsTag, gfnCompare, mfnSequenceCompare );

  RegisterMethod ( eshArrayTag, gfnInitializeInstance,
		   mfnInitArray );
  RegisterMethod ( eshArrayTag, gfnPrintObjectDetails,
		   mfnPrintArray );

  RegisterMethod ( eshStringTag, gfnInitializeInstance,
		   mfnInitString );
  RegisterMethod ( eshStringTag, gfnPrintObjectDetails,
		   mfnPrintString );
  RegisterMethod ( eshStringTag, gfnCompare, mfnStringCompare );
  RegisterMethod ( eshStringTag, gfnNameOf, mfnStringNameOf );
  RegisterMethod ( eshStringTag, gfnValues, mfnStringValues );
  RegisterMethod ( eshStringTag, gfnValueTypeTag,
		   mfnStringValueTypeTag );

  RegisterMethod ( eshBitVectorTag, gfnInitializeInstance,
		   mfnInitBitVector );
  RegisterMethod ( eshBitVectorTag, gfnPrintObjectDetails,
		   mfnPrintBitVector );
  RegisterMethod ( eshBitVectorTag, gfnValues, mfnBitVectorValues );
  RegisterMethod ( eshBitVectorTag, gfnValueTypeTag,
		   mfnBitVectorValueTypeTag );

  RegisterMethod ( eshVectorTag, gfnInitializeInstance,
		   mfnInitStandard );
  RegisterMethod ( eshVectorTag, gfnPrintObjectDetails,
		   mfnPrintVector );
  RegisterMethod ( eshVectorTag, gfnCompare, mfnSequenceCompare );

  RegisterMethod ( eshIVectorTag, gfnInitializeInstance,
		   mfnInitIVector );
  RegisterMethod ( eshIVectorTag, gfnPrintObjectDetails,
		   mfnPrintIVector );
  RegisterMethod ( eshIVectorTag, gfnValues, mfnIVectorValues );
  RegisterMethod ( eshIVectorTag, gfnValueTypeTag, mfnIVectorValueTypeTag );

  RETURN ( VOID );
} /* fnInitializeSequModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitializeSequModule	( void )
{
  PROCEDURE	( fnDeinitializeSequModule );

  RETURN ( VOID );
} /* fnDeinitializeSequModule */

/* -------------------------------------------------------------------------
| Initialization methods
 ------------------------------------------------------------------------- */
static BOOL	mfnInitIVector	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitIVector );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  lpSHvector [ Cooked2RawIndex ( eshIVectorIdxLength ) ]	= o0;
  RETURN ( TRUE );
} /* mfnInitIVector */

/* ----------------------------------------------------------------------- */
static BOOL	mfnInitArray	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  register int	i, n;

  PROCEDURE	( mfnInitArray );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  i	= Cooked2RawIndex ( eshArrayIdxFirstDimension );
  n	= eshSHvectorIdxFirstObjId + lpSHvector [ eshSHvectorIdxObjIds ];
  if ( i >= n ) {
    ERROR (( "Tried to create an array with rank %d.", n - i ));
    RETURN ( FALSE );
  }
  lpSHvector [ Cooked2RawIndex ( eshArrayIdxRank ) ]	=
    Fixnum2ObjId ( n - i );
  for ( /* i = i, n = n, */ lpSHvector += i; i < n; i++, lpSHvector++ ) {
    *lpSHvector	= o0;
  }
  RETURN ( TRUE );
} /* mfnInitArray */

/* ----------------------------------------------------------------------- */
static BOOL	mfnInitString	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitString );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  lpSHvector [ Cooked2RawIndex ( eshStringIdxLength ) ]		= o0;
  RETURN ( TRUE );
} /* mfnInitString */

/* ----------------------------------------------------------------------- */
static BOOL	mfnInitBitVector( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitBitVector );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  lpSHvector [ Cooked2RawIndex ( eshBitVectorIdxLength ) ]	= o0;
  RETURN ( TRUE );
} /* mfnInitBitVector */

/* -------------------------------------------------------------------------
| Print methods
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintCons	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  /* 1997/09/26 HK: Multi threading restriction? */
  static int	nEntered	= 0;
  enum {
    nExtraSpace			= 5,
    nExtraSpaceCeil2		= ( nExtraSpace + 1 ) / 2
  };

  OBJID		oCar, oCdr;
  char		szCar [ 512 ], szCdr [ 512 ];
  int		nCar, nCdr;

  PROCEDURE	( mfnPrintCons );

  if ( nEntered == 0 ) {
    nEntered++;
    find_symbol ( &oGlobalSymNil,
		  find_package ( &oGlobalPkgCommonLisp, szCOMMONLISP ),
		  szNIL );
    oCar	= lpSHvector [ Cooked2RawIndex ( eshConsIdxCar ) ];
    oCdr	= lpSHvector [ Cooked2RawIndex ( eshConsIdxCdr ) ];
    if ( symbolp ( oCar ) ) {
      fnPrintSymbol ( oCar, szCar, sizeof ( szCar ) );
    } else {
      PrintObject ( oCar, szCar );
    }
    nCar	= strlen ( szCar );
    if ( oCdr == oGlobalSymNil ) {
      if ( nCar + nExtraSpace > nBuffer ) {
	nCar		= nBuffer - nExtraSpace;
	nCar		= MAX ( nCar, 0 );
	szCar [ nCar ]	= '\0';
      }
      sprintf ( lpszBuffer, "(%s)", szCar );
    } else {
      if ( symbolp ( oCdr ) ) {
	fnPrintSymbol ( oCdr, szCdr, sizeof ( szCdr ) );
      } else {
	PrintObject ( oCdr, szCdr );
      }
      nCdr	= strlen ( szCdr );
      if ( nCar + nCdr + nExtraSpace > nBuffer ) {
	/* String buffer would overflow; divide fair between szCar and
	   szCdr: */
	nCar		= ( nBuffer * nCar ) / ( nCar + nCdr );
	nCdr		= nBuffer - nCar;
	nCar		-= nExtraSpaceCeil2;
	nCar		= MAX ( nCar, 0 );
	szCar [ nCar ]	= '\0';
	nCdr		-= nExtraSpaceCeil2;
	nCdr		= MAX ( nCdr, 0 );
	szCdr [ nCdr ]	= '\0';
      }
      sprintf ( lpszBuffer, "(%s . %s)", szCar, szCdr );
    }
    nEntered--;
  }
  RETURN ( lpszBuffer );
} /* mfnPrintCons */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintArray	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  OBJID		oDataVector;
  LPOBJID	lpDataVector;
  char		szBuffer [ 80 ];
  int		i, j, n;

  PROCEDURE	( mfnPrintArray );

  oDataVector	= lpSHvector [ Cooked2RawIndex ( eshArrayIdxDataVector ) ];
  switch ( typetagof ( oDataVector ) ) {
  case eshUnboundTag:
    strncpy ( szBuffer, "nil", sizeof ( szBuffer ) );
    break;
  case eshBitVectorTag:
    strncpy ( szBuffer, "(unsigned-byte 1)", sizeof ( szBuffer ) );
    break;
  case eshVectorTag:
    strncpy ( szBuffer, "t", sizeof ( szBuffer ) );
    break;
  case eshIVectorTag:
    lpDataVector	= SH_key_to_address ( oDataVector );
    strncpy ( szBuffer,
	      fnTypeString ( (SHTYPETAG)
			     OBJID2FIXNUM ( lpDataVector
					    [ Cooked2RawIndex
					      ( eshIVectorIdxType ) ] ) ),
	      sizeof ( szBuffer ) );
    break;
  default:
    sprintf ( szBuffer, "->%s", fnTypeString ( typetagof ( oDataVector ) ) );
    break;
  }
  i			= strlen ( szBuffer );
  strncpy ( lpszBuffer, szBuffer, nBuffer );
  if ( i < nBuffer )
    lpszBuffer [ i++ ]	= ' ';
  if ( i < nBuffer )
    lpszBuffer [ i++ ]	= '(';
  for ( j = Cooked2RawIndex ( eshArrayIdxFirstDimension ),
        n = eshSHvectorIdxFirstObjId + lpSHvector [ eshSHvectorIdxObjIds ];
        j < n; j++ ) {
    if ( i < nBuffer ) {
      sprintf ( & lpszBuffer [ i ], "%d ", ObjId2Fixnum ( lpSHvector [ j ] ) );
      i	+= strlen ( & lpszBuffer [ i ] );
    } else
      break;
  }
  if ( i < nBuffer )
    lpszBuffer [ i ]	= '\0';
  if ( i < nBuffer )
    lpszBuffer [ --i ]	= ')';
  RETURN ( lpszBuffer );
} /* mfnPrintArray */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintString	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  int		nLength, i;

  PROCEDURE	( mfnPrintString );

  nLength		=
    OBJID2FIXNUM ( lpSHvector [ Cooked2RawIndex ( eshStringIdxLength ) ] );
  i			= 0;
  lpszBuffer [ i++ ]	= '`';
  if ( nLength == 0 ) {
    lpszBuffer [ i ]	= '\0';
  } else if ( nLength + 5 >= nBuffer - i ) {
    strncpy ( & lpszBuffer [ i ], 
	      (LPSTR) & lpSHvector [ eshSHvectorIdxFirstObjId +
				     lpSHvector [ eshSHvectorIdxObjIds ] ],
	      nBuffer - i - 5 );
    lpszBuffer [ nBuffer - i - 4 ]	= '\0';
    strcat ( lpszBuffer, "..." );
  } else {
    strncpy ( & lpszBuffer [ i ],
	      (LPSTR) & lpSHvector [ eshSHvectorIdxFirstObjId +
				     lpSHvector [ eshSHvectorIdxObjIds ] ],
	      nLength );
  }
  strcat ( lpszBuffer, "'" );
  RETURN ( lpszBuffer );
} /* mfnPrintString */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintBitVector( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  PROCEDURE	( mfnPrintBitVector );

  sprintf ( lpszBuffer, "%d",
	    OBJID2FIXNUM ( lpSHvector [ Cooked2RawIndex
				        ( eshBitVectorIdxLength ) ] ) );
  RETURN ( lpszBuffer );
} /* mfnPrintBitVector */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintVector	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  PROCEDURE	( mfnPrintVector );

  sprintf ( lpszBuffer, "%d", vector_length ( oObjId ) );
  RETURN ( lpszBuffer );
} /* mfnPrintVector */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintIVector	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  SHTYPETAG	nType;
  LPCLASSINFO	lpClassInfoElements;
  int		nTotalSize, nMaxSize;
  OBJID		oCurrSize;

  PROCEDURE	( mfnPrintIVector );

  if ( fixnump ( lpSHvector [ Cooked2RawIndex ( eshIVectorIdxType ) ] ) ) {
    nType		= (SHTYPETAG)
      OBJID2FIXNUM ( lpSHvector [ Cooked2RawIndex ( eshIVectorIdxType ) ] );
    oCurrSize		=
      lpSHvector [ Cooked2RawIndex ( eshIVectorIdxLength ) ];
    lpClassInfoElements	= (LPCLASSINFO) FindClassInfo ( nType );
    if ( lpClassInfoElements ) {
      nTotalSize	= lpClassInfoElements->nFixSizeObjId +
	lpClassInfoElements->nFixSizeValue;
      if ( nTotalSize > 0 ) {
	nMaxSize	= (int)
	  ( (LONG) ( lpSHvector [ eshSHvectorIdxSize ] -
		     lpSHvector [ eshSHvectorIdxObjIds ] -
		     eshSHvectorIdxFirstObjId ) *
	   (LONG) sizeof ( psint ) * (LONG) nBitsPerByte ) /
	     (LONG) nTotalSize;
	if ( fixnump ( oCurrSize ) ) {
	  if ( OBJID2FIXNUM ( oCurrSize ) == nMaxSize ) {
	    sprintf ( lpszBuffer, "%s %d",
		     fnTypeString ( nType ), nMaxSize );
	  } else {
	    sprintf ( lpszBuffer, "%s %d/%d",
		     fnTypeString ( nType ),
		     OBJID2FIXNUM ( oCurrSize ), nMaxSize );
	  }
	} else {
	  sprintf ( lpszBuffer, "%s ?/%d",
		    fnTypeString ( nType ), nMaxSize );
	}
      } else if ( fixnump ( oCurrSize ) ) {
	sprintf ( lpszBuffer, "%s %d/?",
		  fnTypeString ( nType ),
		  OBJID2FIXNUM ( oCurrSize ) );
      } else {
	strncpy ( lpszBuffer, fnTypeString ( nType ), nBuffer );
      }
    }
  }
  RETURN ( lpszBuffer );
} /* mfnPrintIVector */

/* -------------------------------------------------------------------------
| Object name methods
 ------------------------------------------------------------------------- */
static LPSTR		mfnStringNameOf		( OBJID oSelf,
						  LPINT lpnName )
{
  OBJID		oLength;

  PROCEDURE	( mfnStringNameOf );

  if ( lpnName ) {
    oLength	= string_length ( oSelf );
    *lpnName	= OBJID2FIXNUM ( oLength );
  }
  RETURN ( string_ptr ( oSelf ) );
} /* mfnStringNameOf */

/* -------------------------------------------------------------------------
| Object value methods
 ------------------------------------------------------------------------- */

static FIXNUM		mfnStringValues		( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  FIXNUM	nValues = 0;
  FIXNUM	nValuesInWords = 0;
  LPOBJID	pString = NULL;

  PROCEDURE	( mfnStringValues );

  pString		= SH_key_to_address ( oSelf );
  nValuesInWords	= pString [ eshSHvectorIdxSize ] -
    pString [ eshSHvectorIdxObjIds ] - eshSHvectorIdxFirstObjId;
  nValues		= nValuesInWords * nSizeOfPostoreWord;

  RETURN ( nValues );
} /* mfnStringValues */

/* ----------------------------------------------------------------------- */
static FIXNUM		mfnBitVectorValues	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  FIXNUM	nValues = 0;
  FIXNUM	nValuesInWords = 0;
  LPOBJID	pBitVector = NULL;

  PROCEDURE	( mfnBitVectorValues );

  pBitVector		= SH_key_to_address ( oSelf );
  nValuesInWords	= pBitVector [ eshSHvectorIdxSize ] -
    pBitVector [ eshSHvectorIdxObjIds ] - eshSHvectorIdxFirstObjId;
  nValues		= nValuesInWords * sizeof ( psint ) * nBitsPerByte;

  RETURN ( nValues );
} /* mfnBitVectorValues */

/* ----------------------------------------------------------------------- */
static FIXNUM		mfnIVectorValues	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  FIXNUM	nValues = 0;
  LPOBJID	pIVector = NULL;

  PROCEDURE	( mfnIVectorValues );

  pIVector	= SH_key_to_address ( oSelf );
  nValues	=
    ObjId2Fixnum ( pIVector [ Cooked2RawIndex ( eshIVectorIdxLength ) ] );

  RETURN ( nValues );
} /* mfnIVectorValues */

/* ----------------------------------------------------------------------- */
static SHTYPETAG	mfnStringValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  SHTYPETAG	eValueTypeTag = NULLTYPETAG;

  PROCEDURE	( mfnStringValueTypeTag );

  eValueTypeTag	= eshCharacterTag;

  RETURN ( eValueTypeTag );
} /* mfnStringValueTypeTag */

/* ----------------------------------------------------------------------- */
static SHTYPETAG	mfnBitVectorValueTypeTag( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  SHTYPETAG	eValueTypeTag = NULLTYPETAG;

  PROCEDURE	( mfnBitVectorValueTypeTag );

  eValueTypeTag	= eshUnsignedByte1Tag;

  RETURN ( eValueTypeTag );
} /* mfnBitVectorValueTypeTag */

/* ----------------------------------------------------------------------- */
static SHTYPETAG	mfnIVectorValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  SHTYPETAG	eValueTypeTag = NULLTYPETAG;
  LPOBJID	pIVector = NULL;

  INITIALIZEPLOB;

  pIVector	= SH_key_to_address ( oSelf );
  eValueTypeTag =
    ObjId2Fixnum ( pIVector [ Cooked2RawIndex ( eshIVectorIdxType ) ] );

  RETURN ( eValueTypeTag );
} /* mfnIVectorValueTypeTag */

/* -------------------------------------------------------------------------
| Compare methods
 ------------------------------------------------------------------------- */

typedef struct {
  SHTYPETAG	eTypeTag;
  union {
    /* cons: */
    struct {
      OBJID	oRest;
    }	Cons;
    /* vector: */
    struct {
      LPOBJID	poFirst;
      int	nCurrent;
      int	nLength;
    } Vector;
  }	Current;
} SEQUITER, * PSEQUITER;
static const char	szUninitialized []	=
  "Usage of uninitialized iterator.";

/* ----------------------------------------------------------------------- */
static  BOOL		fnSequIterCreate	( PSEQUITER	pSelf,
						  OBJID		oIterate,
						  SHTYPETAG	eTypeTagIterate )
{
  PROCEDURE	( fnSequIterCreate );

  ASSERT ( pSelf != NULL );
  ASSERT ( eshConsIdxCdr - eshConsIdxCar == 1 );

  switch ( eTypeTagIterate ) {

  case eshConsTag:
    pSelf->Current.Cons.oRest		= oIterate;
    break;

  case eshVectorTag:
    pSelf->Current.Vector.poFirst	=
      fnObjId2ObjPtr ( oIterate, Cooked2RawIndex ( 0 ), eshVectorTag,
		       __szFile__, __szProc__, __LINE__ );
    pSelf->Current.Vector.nCurrent	= 0;
    pSelf->Current.Vector.nLength	= vector_length ( oIterate );
    break;

  case eshSymbolTag:
    find_symbol ( &oGlobalSymNil,
		  find_package ( &oGlobalPkgCommonLisp, szCOMMONLISP ),
		  szNIL );
    if ( oIterate == oGlobalSymNil ) {
      eTypeTagIterate		= eshConsTag;
      pSelf->Current.Cons.oRest	= NULLOBJID;
      break;
    }

  default:
    ERROR (( "Cannot iterate on %s\n"
	     "       since it is neither a cons nor a vector.",
	     fnPrintObject ( oIterate, NULL, 0 ) ));
    RETURN ( FALSE );
    break;
  }

  pSelf->eTypeTag	= eTypeTagIterate;

  RETURN ( TRUE );
} /* fnSequIterCreate */

/* ----------------------------------------------------------------------- */
static BOOL		fnSequIterNextP		( PSEQUITER	pSelf )
{
  BOOL	bNext = FALSE;

  PROCEDURE	( fnSequIterNextP );

  ASSERT ( pSelf != NULL );

  switch ( pSelf->eTypeTag ) {

  case eshConsTag:
    bNext	= ( pSelf->Current.Cons.oRest != NULLOBJID );
    break;

  case eshVectorTag:
    bNext	=
      ( pSelf->Current.Vector.nCurrent < pSelf->Current.Vector.nLength );
    break;

  default:
    ERROR (( szUninitialized ));
    break;
  }

  RETURN ( bNext );
}

/* ----------------------------------------------------------------------- */
static OBJID		fnSequIterNext		( PSEQUITER	pSelf )
{
  OBJID		oNext	= NULLOBJID;

  PROCEDURE	( fnSequIterNext );

  ASSERT ( pSelf != NULL );

  switch ( pSelf->eTypeTag ) {

  case eshConsTag:
    if ( consp ( pSelf->Current.Cons.oRest ) ) {
      OBJID	oCdr			= NULLOBJID;
      oNext	= car ( pSelf->Current.Cons.oRest );
      oCdr	= cdr ( pSelf->Current.Cons.oRest );
      find_symbol ( &oGlobalSymNil,
		    find_package ( &oGlobalPkgCommonLisp, szCOMMONLISP ),
		    szNIL );
      pSelf->Current.Cons.oRest	= ( oCdr == oGlobalSymNil ) ?
	NULLOBJID : oCdr;
    } else {
      oNext			= pSelf->Current.Cons.oRest;
      pSelf->Current.Cons.oRest	= NULLOBJID;
    }
    break;

  case eshVectorTag:
    if ( pSelf->Current.Vector.nCurrent < pSelf->Current.Vector.nLength ) {
      oNext	= pSelf->Current.Vector.poFirst [ pSelf->Current.Vector.nCurrent++ ];
    }
    break;

  default:
    ERROR (( szUninitialized ));
    break;
  }

  RETURN ( oNext );
} /* fnSequIterNext */

/* ----------------------------------------------------------------------- */
static COMPARETAG	mfnSequenceCompare	( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  SHTYPETAG	eTypeTagCompare;
  SEQUITER	Iter1, Iter2;

  PROCEDURE	( mfnSequenceCompare );

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

  fnSequIterCreate ( &Iter1, * (LPOBJID) poSelf, eTypeTagSelf );

  eTypeTagCompare	= typetagof ( oCompare );
  fnSequIterCreate ( &Iter2, oCompare, eTypeTagCompare );

  while ( fnSequIterNextP ( &Iter1 ) && fnSequIterNextP ( &Iter2 ) ) {
    OBJID	oNext1, oNext2;
    SHTYPETAG	eTypeTagNext1;
    COMPARETAG	eCompare;
    oNext1		= fnSequIterNext ( &Iter1 );
    oNext2		= fnSequIterNext ( &Iter2 );
    eTypeTagNext1	= typetagof ( oNext1 );
    eCompare		= gfnCompare ( (LPVOID) &oNext1, eTypeTagNext1, oNext2 );
#if (LOGGING+0)
    {
      char	szNext1 [ 256 ], szNext2 [ 256 ];
      LOG (( "Comparing %s\n"
	     "       with %s\n"
	     "       gives %d",
	     fnPrintObject ( oNext1, szNext1, sizeof ( szNext1 ) ),
	     fnPrintObject ( oNext2, szNext2, sizeof ( szNext2 ) ),
	     eCompare ));
    }
#endif
    switch ( eCompare ) {
    case eshNotEqual:
    case eshNotEql:
    case eshNotEq:
    case eshLessEqual:
    case eshLess:
    case eshGreater:
    case eshGreaterEqual:
      RETURN ( eCompare );
      break;
    }
  }

  if ( fnSequIterNextP ( &Iter1 ) ) {
    RETURN ( eshGreater );
  }

  if ( fnSequIterNextP ( &Iter2 ) ) {
    RETURN ( eshLess );
  }

  RETURN ( eshEqual );
} /* mfnSequenceCompare */

/* ----------------------------------------------------------------------- */
static COMPARETAG	mfnStringCompare	( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  LPCSTR	psName = NULL;
  int		nName = 0;

  PROCEDURE	( mfnStringCompare );

  psName	= mfnStringNameOf ( * (LPOBJID) poSelf, &nName );

  RETURN ( fnNameCompare ( psName, nName, oCompare, FALSE ) );
} /* mfnStringCompare */

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
OBJID DLLEXPORT		fnMakeVector		( int nElements )
{
  OBJID		oVector;

  PROCEDURE	( fnMakeVector );

  oVector	=
    fnCreateObject ( (SHTYPETAG) eshVectorTag,
		     eshVectorObjIdSize + nElements, NULLTYPETAG, 0 );
  ASSERT ( boundp ( oVector ) );

  RETURN ( oVector );
} /* fnMakeVector */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnMakeIVector		( SHTYPETAG nTypeTag,
						  int nElements )
{
  OBJID		oIVector;
  LPCLASSINFO	lpClassInfo;
  LPOBJID	lpIVector;

  PROCEDURE	( fnMakeIVector );

  oIVector	= NULLOBJID;
  lpClassInfo	= (LPCLASSINFO) FindClassInfo ( nTypeTag );
  if ( lpClassInfo == NULL ) {
    ERROR (( "Type tag %d is no valid PLOB type tag.", nTypeTag ));
    RETURN ( oIVector );
  }
  if ( lpClassInfo->nTypeFlags & ( typeVarSizeObjIdP | typeVarSizeValueP ) ) {
    ERROR (( "Can't allocate ivectors with variable-sized elements"
	     " of type %s.", lpClassInfo->lpszTypeName ));
    RETURN ( oIVector );
  }
  if ( lpClassInfo->nFixSizeObjId > 0 ) {
    CERROR (( "Allocate the ivector.",
	      "Requested to allocate an ivector with maybe non-immediate"
	      " element type %s.", lpClassInfo->lpszTypeName ));
  }
  oIVector	= fnCreateObject ( (SHTYPETAG) eshIVectorTag,
				   0, nTypeTag, nElements );
  ASSERT ( boundp ( oIVector ) );
  lpIVector	= SH_key_to_address ( oIVector );
  lpIVector [ Cooked2RawIndex ( eshIVectorIdxType ) ]	=
    Fixnum2ObjId ( nTypeTag );
  lpIVector [ Cooked2RawIndex ( eshIVectorIdxLength ) ]	=
    Fixnum2ObjId ( nElements );

  RETURN ( oIVector );
} /* fnMakeIVector */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnShortMakeIVector, "c-sh-make-ivector",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTag )
		  and
		  argument ( FIXNUM, value_in, nElements ) ) )
{
  SHORTOBJID	oShortVector;

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

  oShortVector	= LONG2SHORTOBJID ( fnMakeIVector ( nTypeTag, nElements ) );

  UnstoreSession ();
  RETURN ( oShortVector );
} EndFunction ( fnShortMakeIVector );

/* ----------------------------------------------------------------------- */
LPSTR DLLEXPORT		fnStringPtr	( OBJID oObjId,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine )
{
  psint		* pSHvector;

  PROCEDURE	( fnStringPtr );

  INITIALIZEPLOB;

  if ( ! StableHeap_is_open ) {
    _ERROR ( lpszFile, lpszProc, nLine, ( szNotOpen ) );
    RETURN ( (LPSTR) NULL );
  }
  if ( ! ObjId_is_valid ( oObjId ) ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szInvalidObjId, oObjId,
	       LONG2SHORTOBJID ( oGlobalMinObjId ),
	       LONG2SHORTOBJID ( oGlobalMaxObjId ) ) );
    RETURN ( (LPSTR) NULL );
  }
  pSHvector	= SH_key_to_address ( oObjId );
  if ( pSHvector == NULL ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szCantAddress, LONG2SHORTOBJID ( oObjId  ) ) );
    RETURN ( (LPSTR) NULL );
  }
  if ( eshStringTag !=
       ObjId2TypeTag ( pSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    fnUnexpectedTypeTag ( lpszFile, lpszProc, nLine, oObjId,
			  -1, (SHTYPETAG) eshStringTag );
    RETURN ( (LPSTR) NULL );
  }
  if ( pSHvector [ eshSHvectorIdxSize ] -
       pSHvector [ eshSHvectorIdxObjIds ] <= eshSHvectorIdxFirstObjId ) {
    RETURN ( (LPSTR) szEmpty );
  }
  nGlobalTouched++;
  RETURN ( (LPSTR) & pSHvector [ eshSHvectorIdxFirstObjId +
			         pSHvector [ eshSHvectorIdxObjIds ] ] );
} /* fnStringPtr */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnMakeString		( LPCSTR lpszFrom )
{
  int		nLength;
  OBJID		oString;
  LPSTR		lpszString;

  PROCEDURE	( fnMakeString );

  INITIALIZEPLOB;
  ASSERT ( lpszFrom );
  ASSERT ( StableHeap_is_open );
  nLength	= strlen ( lpszFrom );
  oString	=
    fnCreateObject ( (SHTYPETAG) eshStringTag, 0, eshCharacterTag,
		     nLength + 1 );
  string_length ( oString )	= Fixnum2ObjId ( nLength );
  lpszString			= string_ptr ( oString );
  strncpy ( lpszString, lpszFrom, nLength );
  lpszString [ nLength ]	= '\0';
  RETURN ( oString );
} /* fnMakeString */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnShortMakeString, "c-sh-make-string",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( CONST_STRING, vector_in, lpszFrom ) ) )
{
  SHORTOBJID	oShortString;

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
  oShortString	= LONG2SHORTOBJID ( fnMakeString ( lpszFrom ) );

  UnstoreSession ();
  RETURN ( oShortString );
} EndFunction ( fnShortMakeString );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
