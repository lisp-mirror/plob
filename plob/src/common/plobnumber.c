/* -------------------------------------------------------------------------
| Module	plobnumber.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/11/07
| Description	PLOB source code common for server and client.
|
| Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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
#include	"plobnumber.h"
#include	"plobsequ.h"
#include	"plobstruct.h"
#include	"plobbtree.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
DLLEXPORTVAR const char		szObjIdNotFixnum []	=
"The long objid %d is no immediate fixnum.";

DLLEXPORTVAR const char		szObjIdNotShortFloat []	=
"The long objid %d is no immediate short float.";

/* -------------------------------------------------------------------------
| static constants
 ------------------------------------------------------------------------- */
static const char	szFormatShortFloat []	= "%.7g";
static const char	szFormatSingleFloat []	= "%.8g";
static const char	szFormatDoubleFloat []	= "%.16g";

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintShortFloat( OBJID oObjId, LPOBJID lpSHvector,
				    LPCLASSINFO lpClassInfo,
				    LPSTR lpszBuffer, size_t nBuffer );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitCommonNumberModule	( void )
{
  /* Type description table.
     The type names specified here correspond directly to LISP type names.
     They are read at startup-time from the LISP system and are used
     for mapping beween LISP types and PLOB types.
     In an ultimate persistent system this table should be loaded from
     the persistent store ... */
  static CLASSINFO ClassInfoTable []	= {
    { IMMEDIATE_TYPE_INFO ( eshFixnumTag,
			    SZCOMMONLISP,
			    "fixnum",
			    sizeof ( psint ) * nBitsPerByte,
			    "%d" ) },
    { IMMEDIATE_TYPE_INFO ( eshShortFloatTag,
			    SZCOMMONLISP,
			    "short-float",
			    sizeof ( psint ) * nBitsPerByte,
			    "short-float=%d" ) },
    { (SHTYPETAG) eshSingleFloatTag,
      SZCOMMONLISP,
      "single-float",
      0,
      sizeof ( float ) * nBitsPerByte,
      (LPCSTR) NULL,
      typeRecycleP },
    { (SHTYPETAG) eshDoubleFloatTag,
      SZCOMMONLISP,
      "double-float",
      0,
      sizeof ( double ) * nBitsPerByte,
      (LPCSTR) NULL,
      typeRecycleP },
    { (SHTYPETAG) eshBignumTag,
      SZCOMMONLISP,
      "integer",
      eshBignumObjIdSize * sizeof ( psint ) * nBitsPerByte,
      0,
      (LPCSTR) NULL,
      typeVarSizeValueP },
    { (SHTYPETAG) eshRatioTag,
      SZCOMMONLISP,
      "ratio",
      eshRatioObjIdSize * sizeof ( psint ) * nBitsPerByte,
      0,
      (LPCSTR) NULL,
      typeNoFlags },
    { (SHTYPETAG) eshComplexTag,
      SZCOMMONLISP,
      "complex",
      eshComplexObjIdSize * sizeof ( psint ) * nBitsPerByte,
      0,
      (LPCSTR) NULL,
      typeNoFlags },
    { (SHTYPETAG) eshDynCFloatPtrTag,
      SZPLOB,
      "dynamic-cfloat-pointer",
      0,
      sizeof ( float ) * nBitsPerByte,
      (LPCSTR) NULL,
      (TYPEFLAGS) ( (unsigned int) typeNotAllocatableP | 
		    (unsigned int) typeTransientP ) },
    { (SHTYPETAG) eshDynCDoublePtrTag,
      SZPLOB,
      "dynamic-cdouble-pointer",
      0,
      sizeof ( double ) * nBitsPerByte,
      (LPCSTR) NULL,
      (TYPEFLAGS) ( (unsigned int) typeNotAllocatableP | 
		    (unsigned int) typeTransientP ) },
    { (SHTYPETAG) eshDynCQuadruplePtrTag,
      SZPLOB,
      "dynamic-cquadruple-pointer",
      0,
      sizeof ( long double ) * nBitsPerByte,
      (LPCSTR) NULL,
      (TYPEFLAGS) ( (unsigned int) typeNotAllocatableP |
		    (unsigned int) typeTransientP ) }
  };

  int		i;

  PROCEDURE	( fnInitCommonNumberModule );

  /* Register classes: */
  for ( i = 0; i < length ( ClassInfoTable ); i++ ) {
    RegisterPlobClass ( & ClassInfoTable [ i ] );
  }

  /* Register methods: */
  RegisterMethod ( eshShortFloatTag, gfnPrintObjectDetails,
		   mfnPrintShortFloat );

  RETURN ( VOID );
} /* fnInitCommonNumberModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitCommonNumberModule	( void )
{
  PROCEDURE	( fnDeinitCommonNumberModule );

  RETURN ( VOID );
} /* fnDeinitCommonNumberModule */

/* -------------------------------------------------------------------------
| Object print methods
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintShortFloat( OBJID oObjId, LPOBJID lpSHvector,
				    LPCLASSINFO lpClassInfo,
				    LPSTR lpszBuffer, size_t nBuffer )
{
  LPSTR	pszReturn;

  PROCEDURE	( mfnPrintShortFloat );

  pszReturn	= fnPrintFloat ( lpszBuffer, OBJID2SINGLEFLOAT ( oObjId ),
				 cExponentShortFloat );
  RETURN ( pszReturn );
} /* mfnPrintShortFloat */

/* ----------------------------------------------------------------------- */
LPSTR		fnPrintFloat		( LPSTR		lpszFloat,
					  double	fFloat,
					  EXPCHAR	cExponent )
{
  int		i;
  LPCSTR	lpszFormat;

  PROCEDURE	( fnPrintFloat );
  INITIALIZEPLOB;

  ASSERT ( lpszFloat != NULL );

  switch ( cExponent ) {
  case cExponentShortFloat:
    lpszFormat	= szFormatShortFloat;
    break;
  case cExponentDoubleFloat:
    lpszFormat	= szFormatDoubleFloat;
    break;
  default:
    lpszFormat	= szFormatSingleFloat;
    break;
  }
  sprintf ( lpszFloat, lpszFormat, fFloat );
  for ( i = 0; lpszFloat [ i ]; i++ ) {
    if ( lpszFloat [ i ] == 'e' || lpszFloat [ i ] == 'E' ) {
      lpszFloat [ i ]	= cExponent;
      break;
    }
  }
  RETURN ( lpszFloat );
} /* fnPrintFloat */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
