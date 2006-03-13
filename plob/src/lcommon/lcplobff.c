/* -------------------------------------------------------------------------
| Module	lcplobff.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
| Description	PLOB source code for Allegro foreign function relocation.
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
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lcplob.h"
#include	"lcplobff.h"

/* -------------------------------------------------------------------------
|
| Creating/destroying a Allegro Common LISP string:
|
| HK 1997/03/12: This day, Allegro gave me the following message in the
| listener window:
|
| system error (gsgc): Unknown other object type at f8bc0
| The internal data structures in the running Lisp image have been
| corrupted and execution cannot continue.  Check all foreign functions
| and any Lisp code that was compiled with high speed and/or low safety,
| as these are two common sources of this failure.  If you cannot find
| anything incorrect in your code you should contact technical support
| for Allegro Common Lisp, and we will try to help determine whether
| this is a coding error or an internal bug.
| Would you like to dump core for debugging before exiting(y or n)? 
|
| HK: So it is perhaps no good idea to allocate a string as a
| LISP object in C and to pass it as an argument to Allegro.
| As I thought, this would be a clever solution, since Allegro is
| rather stupid in passing arguments from C to LISP.
 ------------------------------------------------------------------------- */
/* #define	MKLISPOBJECTS */

#ifdef	MKLISPOBJECTS
#include	<lisp.h>
#endif /* #ifdef MKLISPOBJECTS */

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
#ifdef	MKLISPOBJECTS

/* ----------------------------------------------------------------------- */
static LispVal		fnAllegroString		( LPCSTR	lpszFrom )
{
  LispVal	lpObject = NULL;
  unsigned int	n;
  unsigned char	*pObject;

  PROCEDURE	( fnAllegroString );

  n		= strlen ( lpszFrom );
  lpObject	= (LispVal)
    malloc ( n + 1 + sizeof ( struct Vector ) -
	     sizeof ( ((struct Vector *)NULL)->data ) );
  if ( lpObject != NULL ) {
    pObject		= (unsigned char *) lpObject;
    *(pObject++)	= V_charType;
    *(pObject++)	= ( n >> 16 ) & 0xFF;
    *(pObject++)	= ( n >>  8 ) & 0xFF;
    *(pObject++)	= n & 0xFF;
    strcpy ( (char *) &((struct Vector *)lpObject)->data, lpszFrom );
    lpObject		= (LispVal)
      ( ( (unsigned int) lpObject ) | OtherType );
  }

  RETURN ( lpObject );
} /* fnAllegroString */

/* ----------------------------------------------------------------------- */
static LispVal		fnAllegroDestroy	( LispVal	lpObject )
{
  PROCEDURE	( fnAllegroDestroy );

  switch ( GetType ( lpObject ) ) {
  case V_charType:
    free ( (void *) ( ( (unsigned int) lpObject ) - OtherTypeOfs ) );
    lpObject	= NULL;
    break;
  default:
    break;
  }
  RETURN ( lpObject );
} /* fnAllegroDestroy */

#else

/* ----------------------------------------------------------------------- */
static LPSTR	lpszGlobalErrorMsg	= (LPSTR) NULL;

/* ----------------------------------------------------------------------- */
static LPSTR	fnSetErrorMessage	( LPCSTR	pszBuffer )
{
  PROCEDURE	( fnSetErrorMessage );

  if ( lpszGlobalErrorMsg != NULL ) {
    free ( lpszGlobalErrorMsg );
    lpszGlobalErrorMsg	= (LPSTR) NULL;
  }
  if ( pszBuffer != (LPCSTR) NULL ) {
    lpszGlobalErrorMsg	= strdup ( pszBuffer );
  }
  RETURN ( lpszGlobalErrorMsg );
} /* fnSetErrorMessage */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		 fnGetErrorMessage, "sh-get-error-message",
		 ( argument ( STRING ( nBuffer ), vector_out, pszBuffer )
		   and
		   argument ( FIXNUM, value_in, nBuffer ) ) )
{
  int	n	= ( lpszGlobalErrorMsg != NULL ) ?
    strlen ( lpszGlobalErrorMsg ) : 0;

  PROCEDURE	( fnGetErrorMessage );
  INITIALIZEPLOB;

  n	= MIN ( n, nBuffer );
  if ( lpszGlobalErrorMsg != NULL ) {
    strncpy ( pszBuffer, lpszGlobalErrorMsg, n );
  }
  if ( n < nBuffer ) {
    pszBuffer [ n ]	= '\0';
  }

  RETURN ( n );
} EndFunction ( fnGetErrorMessage );

/* ----------------------------------------------------------------------- */
static LPSTR	lpszGlobalContinue	= (LPSTR) NULL;

/* ----------------------------------------------------------------------- */
static LPSTR	fnSetErrorContinue	( LPCSTR	pszBuffer )
{
  PROCEDURE	( fnSetErrorContinue );

  if ( lpszGlobalContinue != NULL ) {
    free ( lpszGlobalContinue );
    lpszGlobalContinue	= (LPSTR) NULL;
  }
  if ( pszBuffer != (LPCSTR) NULL ) {
    lpszGlobalContinue	= strdup ( pszBuffer );
  }
  RETURN ( lpszGlobalContinue );
} /* fnSetErrorContinue */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnGetErrorContinue, "sh-get-error-continue",
		( argument ( STRING ( nBuffer ), vector_out, pszBuffer )
		  and
		  argument ( FIXNUM, value_in, nBuffer ) ) )
{
  int	n	= ( lpszGlobalContinue != NULL ) ?
    strlen ( lpszGlobalContinue ) : 0;

  INITIALIZEPLOB;

  n	= MIN ( n, nBuffer );
  if ( lpszGlobalContinue != NULL ) {
    strncpy ( pszBuffer, lpszGlobalContinue, n );
  }
  if ( n < nBuffer ) {
    pszBuffer [ n ]	= '\0';
  }
  RETURN ( n );
} EndFunction ( fnGetErrorContinue );

/* ----------------------------------------------------------------------- */
static ERRLVL	nGlobalErrorLevel	= errLvl0;

/* ----------------------------------------------------------------------- */
static ERRLVL	fnSetErrorLevel		( ERRLVL	nErrorLevel )
{
  PROCEDURE	( fnSetErrorLevel );

  nGlobalErrorLevel	= nErrorLevel;
  RETURN ( nGlobalErrorLevel );
} /* fnSetErrorLevel */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnGetErrorLevel, "sh-get-error-level",
		( voidArgument ) )
{
  INITIALIZEPLOB;

  RETURN ( nGlobalErrorLevel );
} EndFunction ( fnGetErrorLevel );

#endif /* #ifdef MKLISPOBJECTS */

/* 1998/07/10 HK: Debug: */
static const BOOL	bSuppressCallbacks = FALSE;

/* -------------------------------------------------------------------------
| Foreign-callable functions
 ------------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnLISPerrorCallback, "sh-error-callback",
		( argument ( ERRLVL, value_in, eLevel )
		  and
		  argument ( CONST_STRING, vector_in, lpszContinue )
		  and
		  argument ( CONST_STRING, vector_in, lpszErrorMsg ) ) )
{
  static BOOL		bFirstTime	= TRUE;
#ifdef	MKLISPOBJECTS
  static LispVal	LispContinue	= NULL, LispErrorMsg	= NULL;
#endif /* #ifdef MKLISPOBJECTS */
  static LPFNERROR	lpfnCallback	= (LPFNERROR) NULL;

  INITIALIZEPLOB;

  /* 1998/07/10 HK: Debug: */
  if ( bSuppressCallbacks && eLevel <= errLvlWarn ) {
    RETURN ( VOID );
  }

  if ( bFirstTime ) {
    bFirstTime		= FALSE;
    lpfnCallback	= (LPFNERROR) fnFindFunctionByName ( __szCProc__ );
  }
  if ( lpfnCallback != NULL && ! __bDeinitializePlob__ ) {
#ifdef	MKLISPOBJECTS
    if ( LispContinue != NULL ) {
      LispContinue	= fnAllegroDestroy ( LispContinue );
    }
    LispContinue	= fnAllegroString ( lpszContinue );
    if ( LispErrorMsg != NULL ) {
      LispErrorMsg	= fnAllegroDestroy ( LispErrorMsg );
    }
    LispErrorMsg	= fnAllegroString ( lpszErrorMsg );
    ( * lpfnCallback ) ( IntToFixnum ( eLevel ), LispContinue, LispErrorMsg );
    LispErrorMsg	= fnAllegroDestroy ( LispErrorMsg );
    LispContinue	= fnAllegroDestroy ( LispContinue );
#else
    fnSetErrorLevel ( eLevel );
    fnSetErrorContinue ( lpszContinue );
    fnSetErrorMessage ( lpszErrorMsg );
    ( * lpfnCallback ) ( eLevel,
			 ( lpszContinue != NULL ) ? lpszContinue : szEmpty,
			 ( lpszErrorMsg != NULL ) ? lpszErrorMsg : szEmpty );
    fnSetErrorMessage ( (LPCSTR) NULL );
    fnSetErrorContinue ( (LPCSTR) NULL );
    fnSetErrorLevel ( errNone );
#endif /* #ifdef MKLISPOBJECTS */
  } else if ( (ERRORLEVEL) eLevel == errCError ) {
    LOG (( "%s: %s\naction taken was: %s",
	   ppszErrorLevel2String [ (ERRORLEVEL) eLevel - errMin ],
	   lpszErrorMsg, lpszContinue ));
  } else {
    LOG (( "%s: %s",
	   ppszErrorLevel2String [ (ERRORLEVEL) eLevel - errMin ],
	   lpszErrorMsg ));
  }

  RETURN ( VOID );
} EndFunction ( fnLISPerrorCallback );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
