/* -------------------------------------------------------------------------
| Module	splobregex.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		2005-05-10
| Description	PLOB server code
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
#include	"splobregex.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

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
static BOOL	mfnInitRegEx	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo );

/* Object print methods: */
static LPSTR	mfnPrintRegEx	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );

/* Object name methods: */
static LPSTR		mfnRegExNameOf		( OBJID oSelf,
						  LPINT lpnName );

/* Object equal methods: */
static COMPARETAG	mfnRegExCompare		( LPVOID	pSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );

/* Object value methods: */
static FIXNUM		mfnRegExValues		( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );
static SHTYPETAG	mfnRegExValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );

static BOOL		mfnRegExDestroy		( OBJID		oSelf,
						  BOOL		bKill );
static LPPLOBREGEX	fnGetRegEx		( OBJID		oObjId );

/* -------------------------------------------------------------------------
| Static function declarations
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
static time_t	timeGlobalStart		= 0;

void		fnInitializeRegExModule		( void )
{
  PROCEDURE	( fnInitializeRegExModule );

  time ( &timeGlobalStart );

  /* Make sure that the struct's components offsets match the sh-vector
     indices. If one of the following ASSERTs fails, the eshSHvectorIdx...-
     constants have been modified without reflecting these modifications
     in the corresponding structs PLOBSTRING (or vice versa): */
  ASSERT ( Offset_matches_Index ( PLOBREGEX, oPattern,
				  Cooked2RawIndex ( eshRegExIdxPattern ) ) );
  ASSERT ( Offset_matches_Index
	   ( PLOBREGEX, oFlagsRegComp,
	     Cooked2RawIndex ( eshRegExIdxFlagsRegComp ) ) );
  ASSERT ( Offset_matches_Index
	   ( PLOBREGEX, oErrRegComp,
	     Cooked2RawIndex ( eshRegExIdxErrRegComp ) ) );
  ASSERT ( Offset_matches_Index
	   ( PLOBREGEX, oFlagsRegExec,
	     Cooked2RawIndex ( eshRegExIdxFlagsRegExec ) ) );

  /* Register methods: */
  RegisterMethod ( eshRegExTag, gfnInitializeInstance,
		   mfnInitRegEx );
  RegisterMethod ( eshRegExTag, gfnPrintObjectDetails,
		   mfnPrintRegEx );
  RegisterMethod ( eshRegExTag, gfnCompare, mfnRegExCompare );
  RegisterMethod ( eshRegExTag, gfnNameOf, mfnRegExNameOf );
  RegisterMethod ( eshRegExTag, gfnValues, mfnRegExValues );
  RegisterMethod ( eshRegExTag, gfnValueTypeTag,
		   mfnRegExValueTypeTag );
  RegisterMethod ( eshRegExTag, gfnDestroy, mfnRegExDestroy );


  RETURN ( VOID );
} /* fnInitializeRegExModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitializeRegExModule	( void )
{
  PROCEDURE	( fnDeinitializeRegExModule );

  RETURN ( VOID );
} /* fnDeinitializeRegExModule */

/* -------------------------------------------------------------------------
| Initialization methods
 ------------------------------------------------------------------------- */
static BOOL	mfnInitRegEx	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitRegEx );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  ((LPPLOBREGEX)lpSHvector)->oFlagsRegComp	= o0;
  ((LPPLOBREGEX)lpSHvector)->oErrRegComp	= Fixnum2ObjId ( eregOk );
  ((LPPLOBREGEX)lpSHvector)->oFlagsRegExec	= o0;

  RETURN ( TRUE );
} /* mfnInitRegEx */

/* -------------------------------------------------------------------------
| Print methods
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintRegEx	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  LPPLOBREGEX	pRegEx = NULL;
  REGCOMP	nFlags;
  LPSTR		pszPrinted;
  char		cFlag = '\0';

  PROCEDURE	( mfnPrintRegEx );

  pRegEx	= fnGetRegEx ( oObjId );
  nFlags	= OBJID2FIXNUM ( pRegEx->oFlagsRegComp );

  if ( nFlags & eregNotMatching ) {
    *lpszBuffer++	= '!';
    *lpszBuffer		= '\0';
    nBuffer--;
  }

  pszPrinted	= gfnPrintObjectDetails ( pRegEx->oPattern, lpszBuffer, nBuffer );

  if ( OBJID2FIXNUM ( pRegEx->oErrRegComp ) != eregOk ) {
    cFlag	= 'E';
  } else if ( nFlags & eregCompiled ) {
    cFlag	= ( pRegEx->timeRegEx == timeGlobalStart ) ? 'c' : 'C';
  }

  if ( cFlag != '\0' ) {
    size_t	nPrinted = strlen ( pszPrinted );
    if ( nPrinted < nBuffer - 3 ) {
      pszPrinted [ nPrinted++ ]	= ' ';
      pszPrinted [ nPrinted++ ]	= cFlag;
      pszPrinted [ nPrinted++ ]	= '\0';
    }
  }

  RETURN ( pszPrinted );
} /* mfnPrintRegEx */

/* -------------------------------------------------------------------------
| Object name methods
 ------------------------------------------------------------------------- */
static LPSTR		mfnRegExNameOf		( OBJID oSelf,
						  LPINT lpnName )
{
  LPPLOBREGEX	pRegEx = NULL;

  PROCEDURE	( mfnRegExNameOf );

  pRegEx	= fnGetRegEx ( oSelf );

  RETURN ( gfnNameOf ( pRegEx->oPattern, lpnName ) );
} /* mfnRegExNameOf */

/* -------------------------------------------------------------------------
| Object value methods
 ------------------------------------------------------------------------- */
static FIXNUM		mfnRegExValues		( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  LPPLOBREGEX	pRegEx = NULL;

  PROCEDURE	( mfnRegExValues );

  pRegEx	= fnGetRegEx ( oSelf );

  RETURN ( gfnValues ( pRegEx->oPattern ) );
} /* mfnRegExValues */

/* ----------------------------------------------------------------------- */
static SHTYPETAG	mfnRegExValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo )
{
  LPPLOBREGEX	pRegEx = NULL;

  PROCEDURE	( mfnRegExValueTypeTag );

  pRegEx	= fnGetRegEx ( oSelf );

  RETURN ( gfnValueTypeTag ( pRegEx->oPattern ) );
} /* mfnRegExValueTypeTag */

/* ----------------------------------------------------------------------- */
static BOOL		mfnRegExDestroy		( OBJID		oSelf,
						  BOOL		bKill )
{
  LPPLOBREGEX	pRegEx = NULL;

  PROCEDURE	( mfnRegExDestroy );

  pRegEx	= fnGetRegEx ( oSelf );
  if ( pRegEx->timeRegEx == timeGlobalStart ) {
    gfnDestroy ( pRegEx->oPattern, bKill );
    if ( AtomicLock ( oSelf, oSelf ) ) {
      fnRegFree ( &pRegEx->regEx );
      memset ( &pRegEx->timeRegEx, 0, sizeof ( pRegEx->timeRegEx ) );
      pRegEx->oPattern	= NULLOBJID;
      AtomicUnlock ( oSelf, oSelf );
    }
  }
  RETURN ( (BOOL) TRUE );
} /* mfnRegExDestroy */

/* ----------------------------------------------------------------------- */
static int 		fnRegExCompile		( OBJID		oHeap,
						  OBJID		oRegEx,
						  LPPLOBREGEX	pRegEx,
						  LPSTR		pszErrorMsg,
						  int		nErrorMsg )
{
  REGERROR	eError = (REGERROR) -1;
  REGCOMP	nFlagsComp;

  PROCEDURE	( fnRegExCompile );

  eError	= ObjId2Fixnum ( pRegEx->oErrRegComp );
  nFlagsComp	= ObjId2Fixnum ( pRegEx->oFlagsRegComp );

  if ( eError == eregOk &&
       ( ! ( nFlagsComp & eregCompiled ) ||
	 pRegEx->timeRegEx != timeGlobalStart ) ) {
    /* Compile the regex (anew): */
    if ( AtomicLock ( oRegEx, oRegEx ) ) {
      LPSTR	pszPattern	= string_ptr ( pRegEx->oPattern );
      eError			=
	fnRegComp ( & pRegEx->regEx, pszPattern, nFlagsComp );
      pRegEx->oErrRegComp	= Fixnum2ObjId ( eError );
      /* Mark the regexp as compiled: */
      if ( eError == eregOk ) {
	pRegEx->timeRegEx	= timeGlobalStart;
	pRegEx->oFlagsRegComp	= Fixnum2ObjId ( nFlagsComp | eregCompiled );
      }
      AtomicUnlock ( oRegEx, oRegEx );
    }
  }

  if ( eError != eregOk && pszErrorMsg && nErrorMsg > 0 ) {
    /* Compilation failed; get error message: */
    fnRegError ( eError, NULL, pszErrorMsg, nErrorMsg );
  }

  RETURN ( eError );
} /* fnRegExCompile */

/* -------------------------------------------------------------------------
| Compare methods
 ------------------------------------------------------------------------- */
static COMPARETAG	mfnRegExCompare		( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  LPPLOBREGEX	pRegEx = NULL;
  REGCOMP	nFlagsComp;
  REGERROR	eError;
  COMPARETAG	eCompared;

  PROCEDURE	( mfnRegExCompare );

  pRegEx	= fnGetRegEx ( * (LPOBJID) poSelf );
  nFlagsComp	= ObjId2Fixnum ( pRegEx->oFlagsRegComp );
  eError	= ObjId2Fixnum ( pRegEx->oErrRegComp );
  eCompared	= ( nFlagsComp & eregNotMatching ) ? eshEq : eshNotEq;

  if ( markerp ( oCompare ) ) {
    if ( maxmarkerp ( oCompare ) ) {
      eCompared	= ( nFlagsComp & eregNotMatching ) ? eshGreater : eshLess;
    } else if ( minmarkerp ( oCompare ) ) {
      eCompared	= ( nFlagsComp & eregNotMatching ) ? eshLess : eshGreater;
    } else if ( matchanymarkerp ( oCompare ) ) {
      eCompared	= ( nFlagsComp & eregNotMatching ) ? eshNotEqual : eshEqual;
      RETURN ( eshEqual );
    } else if ( matchnevermarkerp ( oCompare ) ) {
      eCompared	= ( nFlagsComp & eregNotMatching ) ? eshEqual : eshNotEqual;
    }
  } else if ( stringp ( oCompare ) ) {
    LPSTR	pszCompare	= string_ptr ( oCompare );
    LPSTR	pszPattern	= NULL;
    char	szError [ 128 ];
    eError	= fnRegExCompile ( NULLOBJID, * (LPOBJID) poSelf, pRegEx,
				   szError, sizeof ( szError ) );
    if ( eError == eregOk ) {
      REGEXEC	nFlagsExec	= ObjId2Fixnum ( pRegEx->oFlagsRegExec );
      eError	= fnRegExec ( & pRegEx->regEx, pszCompare, 0, NULL, nFlagsExec );
      switch ( eError ) {
      case eregOk:
	eCompared	= ( nFlagsComp & eregNotMatching ) ? eshNotEqual : eshEqual;
	break;
      case eregNoMatch:
	eCompared	= ( nFlagsComp & eregNotMatching ) ? eshEqual : eshNotEqual;
	break;
      default:
	pszPattern	= string_ptr ( pRegEx->oPattern );
	fnRegError ( eError, NULL, szError, sizeof ( szError ) );
	ERROR (( "Error when matching pattern `%s'\n"
		 "       to string `%s':\n"
		 "       %s",
		 pszPattern, pszCompare, szError ));
	break;
      }
    } else {
      pszPattern	= string_ptr ( pRegEx->oPattern );
      ERROR (( "Error when compiling pattern `%s'\n"
	       "       for match to string `%s':\n"
	       "       %s",
	       pszPattern, pszCompare, szError ));
    }
  }

  RETURN ( eCompared );
} /* mfnRegExCompare */

/* ----------------------------------------------------------------------- */
static LPPLOBREGEX	fnGetRegEx		( OBJID		oObjId )
{
  LPPLOBREGEX	pRegEx = NULL;

  pRegEx	= (LPPLOBREGEX) SH_key_to_address ( oObjId );
  ASSERT ( pRegEx != NULL );
  if ( ! ASSERT_TYPE ( oObjId, pRegEx, eshRegExTag ) ) {
    pRegEx = NULL;
  }

  RETURN ( pRegEx );
} /* fnGetRegEx */

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
LPSTR DLLEXPORT		fnRegExPattern	( OBJID oObjId,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine )
{
  psint		* pSHvector;

  PROCEDURE	( fnRegExPattern );

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
  if ( eshRegExTag !=
       ObjId2TypeTag ( pSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    fnUnexpectedTypeTag ( lpszFile, lpszProc, nLine, oObjId,
			  -1, (SHTYPETAG) eshRegExTag );
    RETURN ( (LPSTR) NULL );
  }
  RETURN ( fnStringPtr ( pSHvector [ Cooked2RawIndex ( eshRegExIdxPattern ) ],
			 lpszFile, lpszProc, nLine ) );
} /* fnRegExPattern */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMakeRegEx		( LPCSTR	lpszPattern,
					  REGCOMP	eFlagsComp,
					  REGEXEC	eFlagsRegExec )
{
  OBJID		oRegEx, oPattern;
  LPPLOBREGEX	pRegEx = NULL;

  PROCEDURE	( fnMakeRegEx );

  INITIALIZEPLOB;
  ASSERT ( lpszPattern != NULL );
  ASSERT ( StableHeap_is_open );

  oRegEx	=
    fnCreateObject ( (SHTYPETAG) eshRegExTag, 0, NULLTYPETAG, 0 );

  pRegEx	= (LPPLOBREGEX) SH_key_to_address ( oRegEx );

  oPattern		= make_string ( lpszPattern );
  pRegEx->oPattern	= oPattern;
  makdependent ( oPattern, flagDependentRead );

  pRegEx->oFlagsRegComp	= Fixnum2ObjId ( eFlagsComp );
  pRegEx->oFlagsRegExec	= Fixnum2ObjId ( eFlagsRegExec );

  RETURN ( oRegEx );
} /* fnMakeRegEx */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		 fnShortMakeRegEx, "c-sh-make-regex",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( CONST_STRING, vector_in, lpszPattern )
		   and
		   argument ( REGCOMP, value_in, eFlagsComp )
		   and
		   argument ( REGEXEC, value_in, eFlagsRegExec ) ) );
{
  SHORTOBJID	oShortRegEx;

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

  oShortRegEx	=
    LONG2SHORTOBJID ( fnMakeRegEx ( lpszPattern, eFlagsComp,
				    eFlagsRegExec ) );

  UnstoreSession ();
  RETURN ( oShortRegEx );
} EndFunction ( fnShortMakeRegEx );

/* ----------------------------------------------------------------------- */
BeginFunction ( REGERROR,
		fnShortCompileRegEx, "c-sh-compile-regex",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdRegEx )
		  and
		  argument ( STRING ( nErrorMsg ), vector_out, pszRegExErrMsg )
		  and
		  argument ( FIXNUM, value_in, nRegExErrMsg ) ) )
{
  OBJID		oHeap, oRegEx;
  LPPLOBREGEX	pRegEx = NULL;
  REGERROR	eError = (REGERROR) -1;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    if ( pszRegExErrMsg && nRegExErrMsg > 0 ) {
      *pszRegExErrMsg	= '\0';
    }
    RETURN ( (REGERROR) -1  );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      if ( pszRegExErrMsg && nRegExErrMsg > 0 ) {
	*pszRegExErrMsg	= '\0';
      }
      UNSTORESESSION ();
      RETURN ( -1 );
    }
  }

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  oRegEx	= Short2LongObjId ( oShortObjIdRegEx );
  pRegEx	= fnGetRegEx ( oRegEx );

  eError	= fnRegExCompile ( oHeap, oRegEx, pRegEx,
				   pszRegExErrMsg, nRegExErrMsg );

  RETURN ( eError );

} EndFunction ( fnShortCompileRegex );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
