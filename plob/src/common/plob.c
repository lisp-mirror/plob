/* -------------------------------------------------------------------------
| Module	plob.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB source code common for server and client.
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobintern.h"
#include	"plobmisc.h"
#include	"plobff.h"
#include	"plobtype.h"
#include	"plobnumber.h"
#include	"plobsequ.h"
#include	"plobstruct.h"
#include	"plobclos.h"
#include	"plobroot.h"
#include	"ploblock.h"
#include	"plobheap.h"
#include	"plobbtree.h"
#include	"plobregex.h"
#include	"plobadmin.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern constants
 ------------------------------------------------------------------------- */
DLLEXPORTVAR char	szPlobd [ MAX_FNAME ]	= "plobd";

DLLEXPORTVAR const char	szOptionDirectory []	= "-directory";
DLLEXPORTVAR const char	szOptionPort []		= "-port";

DLLEXPORTVAR const char		szCantAddress []	=
"Can't get real address of object referenced by objid %d.";

DLLEXPORTVAR const char		szCantIndex []		=
"Can't index\n"
"       object %s\n"
"       with index %d.";

const char			szInvalidAlignment []	=
  "Illegal aligned index %d into values section when accessing\n"
  "       %s;\n"
  "       must be aligned to %d boundaries.";

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
DLLEXPORTVAR OBJID	__oFixnum__		= NULLOBJID;
DLLEXPORTVAR OBJID	__oObjId__		= NULLOBJID;
DLLEXPORTVAR OBJID	__oShortFloat__		= NULLOBJID;
DLLEXPORTVAR float	__fBuffer__		= 0.0e0;

DLLEXPORTVAR int	nGlobalStored		= 0;
DLLEXPORTVAR OBJID	oGlobalSession		= NULLOBJID;

int			nGlobalCallCallbacks	= 0;

int			nGlobalArgC		= 0;
char			**ppszGlobalArgV	= (char **) NULL;

DLLEXPORTVAR const char	szInvalidObjId []	=
  "Objid %d is out of allowed range %d .. %d.";

DLLEXPORTVAR const char	szObjIdNotCharacter []	=
  "The long objid %d is no immediate character.";

/* -------------------------------------------------------------------------
| Stable Heap administration
 ------------------------------------------------------------------------- */
/* Directory with currently opened stable heap: */
DLLEXPORTVAR char		szGlobalDirectory [ MAX_FNAME ];
DLLEXPORTVAR OBJID		oGlobalMinObjId		= NULLOBJID;
DLLEXPORTVAR OBJID		oGlobalMaxObjId		= NULLOBJID - 1;
DLLEXPORTVAR OBJID		oGlobalLastObjId	= NULLOBJID;

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitCommonPlobModule		( void )
{
  static CLASSINFO	ClassInfoTable []	= {
    { (SHTYPETAG) eshZombieTag,
        SZPLOB,
	"zombie",
	0,
	0,
        (LPCSTR) NULL,
	typeNotAllocatableP }
  };

  int		i;

  PROCEDURE	( fnInitCommonPlobModule );

  /* Register classes: */
  for ( i = 0; i < length ( ClassInfoTable ); i++ ) {
    RegisterPlobClass ( & ClassInfoTable [ i ] );
  }

  RETURN ( VOID );
} /* fnInitCommonPlobModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitCommonPlobModule	( void )
{
  PROCEDURE	( fnDeinitCommonPlobModule );

  RETURN ( VOID );
} /* fnDeinitCommonPlobModule */

/* ----------------------------------------------------------------------- */
DLLEXPORTVAR int	__bDeinitializePlob__	= FALSE;
void			fnDeinitializePlob	( void )
{
  typedef void			( FAR * LPFNDEINITIALIZE ) ( void );

#ifdef DEINITIALIZE_FUNCTIONS
  static const LPFNDEINITIALIZE	lpfnDeinitialize []	=
    DEINITIALIZE_FUNCTIONS;
#else
  static const LPFNDEINITIALIZE	lpfnDeinitialize	= NULL;
#endif

  int				i;

  PROCEDURE			( fnDeinitializePlob );

  if ( __bDeinitializePlob__ ) {
    RETURN ( VOID );
  }

  __bDeinitializePlob__	= TRUE;
  if ( lpfnDeinitialize ) {
    for ( i = 0; i < length ( lpfnDeinitialize ); i++ ) {
      ( * lpfnDeinitialize [ i ] ) ();
    }
  }

  RETURN ( VOID );
} /* fnDeinitializePlob */

/* ----------------------------------------------------------------------- */
DLLEXPORTVAR int	__bInitializePlob__	= TRUE;
void			fnInitializePlob	( void )
{
  typedef void			( FAR * LPFNINITIALIZE ) ( void );

#ifdef INITIALIZE_FUNCTIONS
  static const LPFNINITIALIZE	lpfnInitialize []	=
    INITIALIZE_FUNCTIONS;
#else
  static const LPFNINITIALIZE	lpfnInitialize		= NULL;
#endif

  int				i;

  PROCEDURE			( fnInitializePlob );

  if ( ! __bInitializePlob__ ) {
    RETURN ( VOID );
  }
  __bInitializePlob__	= FALSE;

  if ( (LPVOID) lpfnInitialize != NULL ) {
    for ( i = 0; i < length ( lpfnInitialize ); i++ ) {
      ( * lpfnInitialize [ i ] ) ();
    }
  }

  atexit ( fnDeinitializePlob );

  RETURN ( VOID );
} /* fnInitializePlob */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnImmediate2ObjId	( FIXNUM nImmediate,
						  SHTYPETAG *pnTypeTag )
{
  LPCLASSINFO	lpClassInfo = NULL;
  OBJID		oObjId = NULLOBJID;

  PROCEDURE	( fnImmediate2ObjId );
  INITIALIZEPLOB;

  lpClassInfo	= (LPCLASSINFO) FindClassInfo ( *pnTypeTag );
  ASSERT ( lpClassInfo != NULL );
  if ( ( lpClassInfo->nTypeFlags & typeTransientP ) != 0 ) {
    char	szBuffer [ 256 ];
    ERROR (( "Attempt to access transient immediate %d\n"
	     "      as a persistent instance of %s",
	     nImmediate,
	     fnPrintImmediateObject ( TYPETAG2OBJID ( *pnTypeTag ),
				      *pnTypeTag,
				      szBuffer, length ( szBuffer ) ) ));
    RETURN ( NULLOBJID );
  }

  if ( *pnTypeTag == eshShortObjIdTag ) {
    oObjId	= SHORT2LONGOBJID ( nImmediate );
    *pnTypeTag	= eshObjIdTag;
  } else if ( immediatep ( *pnTypeTag ) ) {
    switch ( (*pnTypeTag) & nTagMask ) {
    case eshFixnumTag:
      oObjId	= Fixnum2ObjId ( nImmediate );
      break;
    case eshMarkerTag:
      oObjId	= *pnTypeTag;
      break;
    default:
      oObjId	= ( ( (unsigned int) nImmediate ) << nTagBits ) |
	( (*pnTypeTag) & nTagMask );
      break;
    }
  } else {
    oObjId	= nImmediate;
  }

  RETURN ( oObjId );
} /* fnImmediate2ObjId */

/* ----------------------------------------------------------------------- */
FIXNUM DLLEXPORT	fnObjId2Immediate	( OBJID oObjId,
						  SHTYPETAG nTypeTag )
{
  FIXNUM	nImmediate;

  PROCEDURE	( fnObjId2Immediate );

  INITIALIZEPLOB;
  if ( immediatep ( nTypeTag ) ) {
    switch ( nTypeTag & nTagMask ) {
    case eshFixnumTag:
      nImmediate	= ObjId2Fixnum ( oObjId );
      break;
    case eshMarkerTag:
      nImmediate	= nTypeTag;
      break;
    default:
      nImmediate	= ( (unsigned int) oObjId ) >> nTagBits;
      break;
    }
  } else {
    nImmediate	= oObjId;
  }
  RETURN ( nImmediate );
} /* fnObjId2Immediate */

/* ----------------------------------------------------------------------- */
LPVOID		fnMakeLispPointer	( LPVOID	pRaw,
					  BOOL		bDereferencePointer,
					  FIXNUM	nUnmask,
					  FIXNUM	nObjectDataOffset )
{
  LPVOID	pCooked = NULL;

  PROCEDURE	( fnMakeLispPointer );
  INITIALIZEPLOB;

  ASSERT ( pRaw != NULL );
  if ( bDereferencePointer ) {
    pRaw	= * (LPVOID *) pRaw;
    ASSERT ( pRaw != NULL );
  }
  pCooked	= (LPVOID) ( ( (LPSTR) ( ( (unsigned long) pRaw ) &
					 ~(unsigned long) nUnmask ) ) +
			     nObjectDataOffset );

  RETURN ( pCooked );
} /* fnMakeLispPointer */

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
