/* -------------------------------------------------------------------------
| Module	splobcallb.c
| Author	Heiko Kirschke
| Copyright	(C) 1993,1994,1998 Heiko Kirschke
| Date		1998/07/08 created
| Description	Callbacks for server
 ------------------------------------------------------------------------- */
#include	<stdarg.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobff.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobtype.h"
#include	"splobnumber.h"
#include	"splobstruct.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* Dummy callback for RPC server: */
BOOL		fnLISPsuspendCallback		( SHORTOBJID oLockBy,
						  SHORTOBJID oToLock,
						  LPCSTR lpszReason )
{
  PROCEDURE	( fnLISPsuspendCallback );
  fnStoreErrorIf ( errLvlInfo, lpszReason );
  RETURN ( FALSE );
} /* fnLISPsuspendCallback */

/* ----------------------------------------------------------------------- */
/* Dummy callback for RPC server: */
BOOL		fnLISPwakeupCallback		( SHORTOBJID oLockBy,
						  SHORTOBJID oToLock,
						  LPCSTR lpszReason )
{
  PROCEDURE	( fnLISPwakeupCallback );
  fnStoreErrorIf ( errLvlInfo, lpszReason );
  RETURN ( FALSE );
} /* fnLISPwakeupCallback */

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnLISPerrorCallback, "sh-error-callback",
		 ( argument ( ERRLVL, value_in, eLevel )
		   and
		   argument ( CONST_STRING, vector_in, pszContinue )
		   and
		   argument ( CONST_STRING, vector_in, pszErrorMsg ) ) )
{
  char		szBuffer [ 2048 ];

  if ( eLevel == errLvlCError ) {
    sprintf ( szBuffer, "%s\naction taken was: %s",
	      pszErrorMsg, ( pszContinue != NULL ) ? pszContinue : szEmpty );
    fnStoreErrorIf ( eLevel, szBuffer );
    _LOG ( NULL, NULL, -1,
	   ( "%s: %s\n"
	     "       action taken was: %s",
	     ppszErrorLevel2String [ (ERRORLEVEL) eLevel - errMin ],
	     pszErrorMsg, ( pszContinue != NULL ) ? pszContinue : szEmpty ) );
  } else {
    /* Detect the message 
       "Unexpected page fault encountered"
       raised from sheap.c; this is a hint on a `real' (i.e. unexpected)
       segmentation violation and should be handled as a fatal error
       with the implication that the server should be restarted as soon
       as possible: */
    if ( eLevel >= errLvlError &&
	 strstr ( pszErrorMsg, "page fault" ) != NULL ) {
      eLevel	= errLvlFatal;
    }
    fnStoreErrorIf ( eLevel, pszErrorMsg );
    if ( eLevel > errLvlInfo ) {
      _LOG ( NULL, NULL, -1,
	     ( "%s: %s",
	       ppszErrorLevel2String [ (ERRORLEVEL) eLevel - errMin ],
	       pszErrorMsg ) );
    }
  }

  /* 1996/11/07 HK: Debug */
  if ( eLevel >= errLvlFatal ) {
#if	WIN32
    DebugBreak ();
#endif
    abort ();
  }

  RETURN ( VOID );
} EndFunction ( fnLISPerrorCallback );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
