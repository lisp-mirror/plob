/* -------------------------------------------------------------------------
| Module	splobcallb.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/08 created
| Description	Callbacks for server
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
