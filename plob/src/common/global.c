/* -------------------------------------------------------------------------
| Module	global.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		27.10.93
|
| Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
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
 ------------------------------------------------------------------------- */

#include	<ctype.h>
#include	<errno.h>
#include	<stdarg.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
DLLEXPORTVAR const char		szEmpty []		= "";
DLLEXPORTVAR const char		szAt []			= "@";
DLLEXPORTVAR const char		szNULL []		= "NULL";
DLLEXPORTVAR const char		szSpace []		= " ";
DLLEXPORTVAR const char		szStdIn []		= "stdin";
DLLEXPORTVAR const char		szStdOut []		= "stdout";
DLLEXPORTVAR const char		szStdErr []		= "stderr";
DLLEXPORTVAR const char		szTimeFormat []		= "%Y/%m/%d %H:%M:%S";

#if	WIN32
#define	STREAMBINARY	"b"
#else
#define	STREAMBINARY	""
#endif

DLLEXPORTVAR const char		szStreamAppend []	= "a" STREAMBINARY;
DLLEXPORTVAR const char		szStreamAppendRead []	= "a+" STREAMBINARY;
DLLEXPORTVAR const char		szStreamRead []		= "r" STREAMBINARY;
DLLEXPORTVAR const char		szStreamReadWrite []	= "r+" STREAMBINARY;
DLLEXPORTVAR const char		szStreamWrite []	= "w" STREAMBINARY;
DLLEXPORTVAR const char		szStreamWriteTruncate []= "w+" STREAMBINARY;

DLLEXPORTVAR LPCSTR		szBool2Verb [ 2 ]	=
{ "failed", "succeeded" };

DLLEXPORTVAR const char		__szProcEntering__ []	=
"%3d: %s(%d): Entering %s ...\n";
DLLEXPORTVAR const char		__szProcLeaving__ []	=
"%3d: %s(%d): Leaving %s\n";
DLLEXPORTVAR int		nGlobalFlagWord		= 0;
DLLEXPORTVAR int		__nStackLevel__		= 0;

DLLEXPORTVAR const char		__szMallocedBytes__ []	=
  "Allocated %d bytes.";
DLLEXPORTVAR const char		__szFreedBytes__ []	=
  "Freed %d bytes.";
DLLEXPORTVAR const char		__szBrkIncreased__ []	=
  "Brk increased by %d bytes.";

DLLEXPORTVAR int		__nJmpBufError__	= 0;
DLLEXPORTVAR jmp_buf		__jmpbufError__;
DLLEXPORTVAR BOOL		__bJmpBufErrorValid__	= FALSE;
DLLEXPORTVAR const char *	__szProc__		= szEmpty;
DLLEXPORTVAR const LPFNVOID	__fnProc__		= (LPFNVOID) NULL;

DLLEXPORTVAR BOOL		__bInitializeCommon__	= TRUE;
DLLEXPORTVAR BOOL		__bDeinitializeCommon__	= FALSE;

/* -------------------------------------------------------------------------
| Static variables
 ------------------------------------------------------------------------- */
static PFNMKLOGPROMPT	pfnGlobalMkLogPrompt	= (PFNMKLOGPROMPT) NULL;
static LPFNVOID		ppfnGlobalInitializers []	= {
  fnInitializeMallocModule, fnInitializeGlobalModule,
  fnInitializeHashModule, fnInitializeGenericModule };
static LPFNVOID		ppfnGlobalDeinitializers []	= {
 fnDeinitializeGenericModule, fnDeinitializeHashModule,
 fnDeinitializeGlobalModule, fnDeinitializeMallocModule };

/* -------------------------------------------------------------------------
| Prototypes for static functions
 ------------------------------------------------------------------------- */
static int	fnShowExit		( int nExitCode );
static LPSTR	fnMkErrorPrompt		( LPSTR lpszBuf,
					  size_t nBuf,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  LPCSTR lpszPrefix,
					  LPCSTR lpszFormat,
					  va_list pArg );

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
static int	fnShowExit		( int nExitCode )
{
  PROCEDURE	( fnShowExit );

  while ( TRUE ) {
    fprintf ( stderr,
	      "(C)ontinue"
#if	WIN32
	      ", (b)reak"
#endif
	      " or (e)xit current process with code %d? ",
	      nExitCode );
    fflush ( stderr );
    fflush ( stdin );
    switch ( getchar () ) {
    case EOF:
      fputs ( "e\n", stderr );
    case 'e': case 'E':
      fprintf ( stderr, "Exiting with code %d ...\n", nExitCode );
      exit ( nExitCode );
      break;
#if	WIN32
    case 'b': case 'B':
      DebugBreak ();
      break;
#endif
    case 'c': case 'C':
      fputs ( "Continuing process ...\n", stderr );
      RETURN ( nExitCode );
    default:
      break;
    }
  }
  RETURN ( nExitCode );
} /* fnShowExit */

/* ------------------------------------------------------------------------- */
static LPSTR	fnMkErrorPrompt		( LPSTR lpszBuf,
					  size_t nBuf,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  LPCSTR lpszPrefix,
					  LPCSTR lpszFormat,
					  va_list pArg )
{
  static char	szBuf [ 2048 ];

  int		i;

  PROCEDURE	( fnMkErrorPrompt );

  if ( lpszFile && *lpszFile ) {
    strcpy ( szBuf, lpszFile );
    i	= strlen ( szBuf );
  } else {
    szBuf [ 0 ]	= '\0';
    i		= 0;
  }
  if ( nLine >= 0 ) {
    sprintf ( & szBuf [ i ], "(%d): ", nLine );
    i	+= strlen ( & szBuf [ i ] );
  }
  if ( lpszProc && *lpszProc ) {
    sprintf ( & szBuf [ i ], "%s: ", lpszProc );
    i	+= strlen ( & szBuf [ i ] );
  }
  if ( i > 0 ) {
    while ( i > 0 && ( szBuf [ i ] <= ' ' || szBuf [ i ] == ':' ) ) {
      i--;
    }
    i++;
    strcpy ( & szBuf [ i ], ":\n       " );
    i		+= strlen ( & szBuf [ i ] );
  }
  if ( lpszPrefix && *lpszPrefix ) {
    strcpy ( & szBuf [ i ], lpszPrefix );
    i		+= strlen ( & szBuf [ i ] );
  }
  vsprintf ( & szBuf [ i ], lpszFormat, pArg );
  if ( lpszBuf && nBuf ) {
    strncpy ( lpszBuf, szBuf, nBuf );
    RETURN ( lpszBuf );
  }
  RETURN ( szBuf );
} /* fnMkErrorPrompt */

/* ------------------------------------------------------------------------- */
DLLEXPORTVAR LPCSTR	ppszErrorLevel2String [ errNoE ]	=
{ "none", "info", "warning", "continuable error", "error", "fatal error" };

/* ------------------------------------------------------------------------- */
int DLLEXPORT	fnGlobalErrorHandler	( ERRORLEVEL	eLevel,
					  LPCSTR	lpszContinue,
					  LPCSTR	lpszErrorMsg )
{
  int		nReturnCode = 0;

  PROCEDURE	( fnGlobalErrorHandler );

  INITIALIZECOMMON;

  switch ( eLevel ) {

  case errCError:
    if ( lpszErrorMsg != NULL ) {
      fprintf ( stderr, "%s: %s\nIf continued: %s\n",
		ppszErrorLevel2String [ eLevel - errMin ],
		lpszErrorMsg, lpszContinue );
      fflush ( stderr );
    }
    fnShowExit ( 254 );
    break;

  case errError:
    nReturnCode	= 1;

  default:
    if ( lpszErrorMsg != NULL ) {
      fprintf ( stderr, "%s: %s\n",
		ppszErrorLevel2String [ eLevel - errMin ],
		lpszErrorMsg );
      fflush ( stderr );
    }
    if ( eLevel >= errError ) {
      fnShowExit ( 255 );
    }
    break;
  }

  RETURN ( nReturnCode );
} /* fnGlobalErrorHandler */

/* ------------------------------------------------------------------------- */
static LPFNERROR	lpfnErrorHandler	= fnGlobalErrorHandler;

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
BOOL DLLEXPORT	fnGlobalSetErrorHandler	( LPFNERROR lpfnError )
{
  PROCEDURE	( fnGlobalSetErrorHandler );
  INITIALIZECOMMON;

  lpfnErrorHandler	= ( lpfnError ) ?
    lpfnError : fnGlobalErrorHandler;

  RETURN ( TRUE );
} /* fnGlobalSetErrorHandler */

/* ----------------------------------------------------------------------- */
static LPFILE		pGlobalLogFile		= (LPFILE) NULL;
static char		szGlobalLogDirectory [ MAX_FNAME ]	= ".";
static const char	szGlobalLogFile []		= "messages";
static const char	szGlobalLogExtension []		= "log";
static const char	szFormatLogPath []		= "%s/%s.%s";

/* ----------------------------------------------------------------------- */
LPFILE DLLEXPORT	fnLogOpen		( void )
{
  PROCEDURE ( fnLogOpen );

  if ( pGlobalLogFile == NULL ) {
    if ( strcmp ( szGlobalLogDirectory, "-" ) == 0 ) {
      pGlobalLogFile	= stderr;
    } else {
      char	szLogPath [ MAX_FNAME ];
      sprintf ( szLogPath, szFormatLogPath,
		szGlobalLogDirectory, szGlobalLogFile, szGlobalLogExtension );
      errno		= 0;
      pGlobalLogFile	= fopen ( szLogPath, szStreamAppend );
      if ( pGlobalLogFile == NULL ) {
	pGlobalLogFile	= stderr;
      }
    }
  }

  RETURN ( pGlobalLogFile );
} /* fnLogOpen */

/* ----------------------------------------------------------------------- */
void DLLEXPORT		fnLogClose		( void )
{
  PROCEDURE ( fnLogClose );

  if ( pGlobalLogFile != NULL ) {
    fflush ( pGlobalLogFile );
    if ( pGlobalLogFile != stderr ) {
      fclose ( pGlobalLogFile );
    }
    pGlobalLogFile	= (LPFILE) NULL;
  }

  RETURN ( VOID );
} /* fnLogClose */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnLogSetDirectory	( LPCSTR	lpszDirectory )
{
  PROCEDURE	( fnLogSetDirectory );

  fnLogClose ();
  strncpy ( szGlobalLogDirectory, lpszDirectory,
	    sizeof ( szGlobalLogDirectory ) );

  RETURN ( lpszDirectory );
} /* fnLogSetDirectory */

/* ----------------------------------------------------------------------- */
/* Number of versions to keep of a log file: */
enum { nLogVersions	= 3 };
void DLLEXPORT		fnLogNextVersion	( void )
{
  static const char	szFormatLogVersionPath []	= "%s/%s.%d";

  LPFILE		pLogFile = (LPFILE) NULL;
  int			i;
  char			szOldName [ MAX_FNAME ], szNewName [ MAX_FNAME ];

  PROCEDURE ( fnLogNextVersion );

  fnLogClose ();

  sprintf ( szOldName, szFormatLogPath,
	    szGlobalLogDirectory, szGlobalLogFile, szGlobalLogExtension );
  pLogFile	= fopen ( szOldName, szStreamRead );
  if ( pLogFile != NULL ) {
    fclose ( pLogFile );
    pLogFile	= (LPFILE) NULL;
    for ( i = nLogVersions; i >= 1; i-- ) {
      sprintf ( szNewName, szFormatLogVersionPath,
		szGlobalLogDirectory, szGlobalLogFile, i );
      remove ( szNewName );
      if ( i > 1 ) {
	sprintf ( szOldName, szFormatLogVersionPath,
		  szGlobalLogDirectory, szGlobalLogFile, i - 1 );
      } else {
	sprintf ( szOldName, szFormatLogPath, szGlobalLogDirectory,
		  szGlobalLogFile, szGlobalLogExtension );
      }
      rename ( szOldName, szNewName );
    }
  }

  RETURN ( VOID );
} /* fnLogNextVersion */

/* ----------------------------------------------------------------------- */
void DLLEXPORT	fnSetFnMkLogPrompt	( PFNMKLOGPROMPT pfnMkLogPrompt )
{
  PROCEDURE	( fnSetFnMkLogPrompt );
  INITIALIZECOMMON;

  pfnGlobalMkLogPrompt	= pfnMkLogPrompt;

  RETURN ( VOID );
} /* fnGlobalLogPrompt */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	_fnGlobalLog		( LPCSTR lpszFormat, ... )
{
  LPCSTR	lpszFile, lpszProc;
  int		nLine;
  va_list	pArg;
  char		szBuf [ 2048 ];
  time_t	Time;
  FILE		* pLogFile;

  PROCEDURE	( _fnGlobalLog );
  INITIALIZECOMMON;

  lpszFile	= __lpszErrorFile__;
  lpszProc	= __lpszErrorProc__;
  nLine		= __nErrorLine__;

  pLogFile	= fnLogOpen ();
  if ( pLogFile != NULL ) {
    time ( &Time );
    strftime ( szBuf, sizeof ( szBuf ), szTimeFormat, localtime ( &Time ) );
    if ( ( fputs ( szBuf, pLogFile ) < 0 ||
	   fflush ( pLogFile ) != 0 ) && pLogFile != stderr ) {
      fnLogClose ();
      pLogFile	= fnLogOpen ();
      fputs ( szBuf, pLogFile );
    }
    szBuf [ 0 ]	= '\0';
    if ( pfnGlobalMkLogPrompt ) {
      ( * pfnGlobalMkLogPrompt ) ( szBuf, sizeof ( szBuf ) );
    }
    if ( szBuf [ 0 ] != '\0' ) {
      fputc ( ' ', pLogFile );
      fputs ( szBuf, pLogFile );
    } else {
      fprintf ( pLogFile, " %s@%s", fnGetUser (), fnGetHost () );
    }
    fputs ( "\n       ", pLogFile );
    va_start ( pArg, lpszFormat );
    szBuf [ 0 ]	= '\0';
    fnMkErrorPrompt ( szBuf, sizeof ( szBuf ),
		      lpszFile, lpszProc, nLine,
		      (LPCSTR) NULL, lpszFormat, pArg );
    va_end ( pArg );
    fputs ( szBuf, pLogFile );
    fputc ( '\n', pLogFile );
    fflush ( pLogFile );
  }
  return TRUE;
} /* _fnGlobalLog */

/* ------------------------------------------------------------------------- */
BOOL DLLEXPORT	_fnGlobalAssertFailed	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  ... )
{
  va_list	pArg;
  char		szBuf [ 2048 ];
  int		i;

  PROCEDURE	( _fnGlobalAssertFailed );
  INITIALIZECOMMON;

  va_start ( pArg, nLine );
  fnMkErrorPrompt ( szBuf, sizeof ( szBuf ), lpszFile, lpszProc, nLine,
		    (LPCSTR) NULL, "ASSERT failed: %s", pArg );
  va_end ( pArg );
  i	= ( * lpfnErrorHandler ) ( errError, NULL, szBuf );
  THROWERROR ( 254 );
  fnShowExit ( 254 );
  RETURN ( TRUE );
} /* _fnGlobalAssertFailed */

/* ------------------------------------------------------------------------- */
DLLEXPORTVAR LPCSTR	__lpszErrorFile__	= (LPCSTR) NULL;
DLLEXPORTVAR LPCSTR	__lpszErrorProc__	= (LPCSTR) NULL;
DLLEXPORTVAR ERRORLEVEL	__eErrorLevel__		= errNone;
DLLEXPORTVAR int	__nErrorLine__		= 0;

/* ------------------------------------------------------------------------- */
static int	fnHandleError		( LPCSTR	pszFile,
					  LPCSTR	pszProc,
					  ERRORLEVEL	eLevel,
					  int		nLine,
					  LPCSTR	pszContinue,
					  LPCSTR	pszFormat,
					  va_list	pArg )
{
  char		szBuf [ 2048 ];
  int		i, nReturnCode = 0;

  PROCEDURE	( fnHandleError );

  fnMkErrorPrompt ( szBuf, sizeof ( szBuf ),
		    pszFile, pszProc, nLine,
		    (LPCSTR) NULL, pszFormat, pArg );

  i	= ( * lpfnErrorHandler ) ( eLevel, pszContinue, szBuf );
  if ( eLevel >= errError ) {
    nReturnCode	= 255;
    THROWERROR ( nReturnCode );
    if ( i > 0 ) {
      fnShowExit ( i );
    }
  } else if ( eLevel >= errCError ) {
    if ( i > 0 ) {
      exit ( i );
    }
    nReturnCode	= 254;
  }
  RETURN ( nReturnCode );
} /* fnHandleError */

/* ------------------------------------------------------------------------- */
int DLLEXPORT	_fnGlobalCError		( LPCSTR lpszContinue,
					  LPCSTR lpszFormat, ... )
{
  LPCSTR	lpszFile, lpszProc;
  ERRORLEVEL	eLevel;
  int		nLine, nReturnCode;
  va_list	pArg;

  PROCEDURE	( _fnGlobalCError );
  INITIALIZECOMMON;

  lpszFile	= __lpszErrorFile__;
  lpszProc	= __lpszErrorProc__;
  eLevel	= __eErrorLevel__;
  nLine		= __nErrorLine__;

  va_start ( pArg, lpszFormat );
  nReturnCode	= fnHandleError ( lpszFile, lpszProc, eLevel, nLine,
				  lpszContinue, lpszFormat, pArg );
  va_end ( pArg );

  RETURN ( nReturnCode );
} /* _fnGlobalCError */

/* ------------------------------------------------------------------------- */
int DLLEXPORT		_fnGlobalError		( LPCSTR lpszFormat, ... )
{
  LPCSTR	lpszFile, lpszProc;
  ERRORLEVEL	eLevel;
  int		nLine, nReturnCode;
  va_list	pArg;

  PROCEDURE	( _fnGlobalError );
  INITIALIZECOMMON;

  lpszFile	= __lpszErrorFile__;
  lpszProc	= __lpszErrorProc__;
  eLevel	= __eErrorLevel__;
  nLine		= __nErrorLine__;

  va_start ( pArg, lpszFormat );
  nReturnCode	= fnHandleError ( lpszFile, lpszProc, eLevel, nLine,
				  (LPCSTR) NULL, lpszFormat, pArg );
  va_end ( pArg );

  RETURN ( nReturnCode );
} /* _fnGlobalError */

/* ------------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetHost		( void )
{
  static char	szHost [ 256 ];
  static BOOL	bFirstTime	= TRUE;

  PROCEDURE	( fnGetHost );
  INITIALIZECOMMON;

  if ( bFirstTime ) {
    bFirstTime		= FALSE;
    szHost [ 0 ]	= '\0';
    gethostname ( szHost, sizeof ( szHost ) );
    if ( szHost [ 0 ] == '\0' ) {
      strncpy ( szHost, "localhost", sizeof ( szHost ) );
    }
  }

  RETURN ( szHost );
} /* fnGetHost */

/* ------------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetUser		( void )
{
  static char	szUser [ 64 ];
  static BOOL	bFirstTime	= TRUE;

  PROCEDURE	( fnGetUser );
  INITIALIZECOMMON;

  if ( bFirstTime ) {
    bFirstTime		= FALSE;
    szUser [ 0 ]	= '\0';
#if WIN32
    {
      DWORD	nSize	= sizeof ( szUser );
      GetUserName ( szUser, &nSize );
    }
#else
    cuserid ( szUser );
#endif
    if ( szUser [ 0 ] == '\0' ) {
      strncpy ( szUser, UNREADABLE_OBJECT_PREFIX "unknown user"
		UNREADABLE_OBJECT_SUFFIX, sizeof ( szUser ) );
    }
  }

  RETURN ( szUser );
} /* fnGetUser */

/* ------------------------------------------------------------------------- */
int DLLEXPORT	fnStrCmp		( LPCSTR lpszString1,
					  LPCSTR lpszString2 )
{
  register char	c1, c2;

  PROCEDURE	( fnStrCmp );
  INITIALIZECOMMON;

  if ( ! lpszString1 )
    lpszString1	= szEmpty;
  if ( ! lpszString2 )
    lpszString2	= szEmpty;

  while ( ( c1 = *(lpszString1++) ) == ( c2 = *(lpszString2++) ) && c1 );

  RETURN ( c1 - c2 );
} /* fnStrCmp */

/* ------------------------------------------------------------------------- */
int DLLEXPORT	strnncmp		( LPCSTR lpszString1,
					  size_t nMaxString1,
					  LPCSTR lpszString2,
					  size_t nMaxString2 )
{
  register char	c1, c2;
  register int	i, n;

  PROCEDURE	( strnncmp );
  INITIALIZECOMMON;

  if ( ! lpszString1 )
    lpszString1	= szEmpty;
  if ( ! lpszString2 )
    lpszString2	= szEmpty;

  i	= 0;
  n	= MIN ( nMaxString1, nMaxString2 );

  while ( ( c1 = *(lpszString1++) ) == ( c2 = *(lpszString2++) ) &&
	  c1 && i < n )
    i++;

  if ( i >= nMaxString1 )
    c1	= '\0';
  if ( i >= nMaxString2 )
    c2	= '\0';

  RETURN ( c1 - c2 );
} /* strnncmp */

/* ------------------------------------------------------------------------- */
int DLLEXPORT	strnnicmp		( LPCSTR lpszString1,
					  size_t nMaxString1,
					  LPCSTR lpszString2,
					  size_t nMaxString2 )
{
  register char	c1, c2;
  register int	i, n;

  PROCEDURE	( strnncmp );
  INITIALIZECOMMON;

  if ( ! lpszString1 )
    lpszString1	= szEmpty;
  if ( ! lpszString2 )
    lpszString2	= szEmpty;

  i	= 0;
  n	= MIN ( nMaxString1, nMaxString2 );

  while ( ( c1 = tolower ( *(lpszString1++) ) ) ==
	  ( c2 = tolower ( *(lpszString2++) ) ) &&
	  c1 != '\0' && i < n ) {
    i++;
  }

  if ( i >= nMaxString1 )
    c1	= '\0';
  if ( i >= nMaxString2 )
    c2	= '\0';

  RETURN ( c1 - c2 );
} /* strnncimp */

/* ------------------------------------------------------------------------- */
LPSTR DLLEXPORT	fnStrLwr		( LPSTR lpszToLower )
{
  register char	c;
  register int	i;

  PROCEDURE	( fnStrLwr );
  INITIALIZECOMMON;

  if ( lpszToLower ) {
    i	= 0;
    while ( c = lpszToLower [ i ] )
      lpszToLower [ i++ ]	= tolower ( c );
  }
  RETURN ( lpszToLower );
} /* fnStrLwr */

/* ------------------------------------------------------------------------- */
LPSTR DLLEXPORT	fnStrUpr		( LPSTR lpszToUpper )
{
  register char	c;
  register int	i;

  PROCEDURE	( fnStrUpr );
  INITIALIZECOMMON;

  if ( lpszToUpper ) {
    i	= 0;
    while ( c = lpszToUpper [ i ] )
      lpszToUpper [ i++ ]	= toupper ( c );
  }
  RETURN ( lpszToUpper );
} /* fnStrUpr */

/* ------------------------------------------------------------------------- */
int DLLEXPORT	fnStrICmp		( LPCSTR lpszString1,
					  LPCSTR lpszString2 )
{
  register char	c1, c2;

  PROCEDURE	( fnStrICmp );
  INITIALIZECOMMON;

  if ( ! lpszString1 )
    lpszString1	= szEmpty;
  if ( ! lpszString2 )
    lpszString2	= szEmpty;

  while ( ( c1 = tolower ( *(lpszString1++) ) ) ==
	  ( c2 = tolower ( *(lpszString2++) ) ) && c1 != '\0' );

  RETURN ( c1 - c2 );
} /* fnStrICmp */

/* ------------------------------------------------------------------------- */
int DLLEXPORT	fnStrNICmp		( LPCSTR lpszString1,
					  LPCSTR lpszString2,
					  size_t nMaxString )
{
  register char	c1, c2;
  register int	i;

  PROCEDURE	( fnStrNICmp );
  INITIALIZECOMMON;

  if ( ! lpszString1 )
    lpszString1	= szEmpty;
  if ( ! lpszString2 )
    lpszString2	= szEmpty;

  i	= 0;
  while ( ( c1 = tolower ( *(lpszString1++) ) ) ==
	  ( c2 = tolower ( *(lpszString2++) ) ) && c1 && i < nMaxString )
    i++;

  RETURN ( c1 - c2 );
} /* fnStrNICmp */

/* ----------------------------------------------------------------------------
| Module initialization / Deinitialization
 --------------------------------------------------------------------------- */
void		fnDeinitializeCommon	( void )
{
  int		i;

  PROCEDURE	( fnDeinitializeCommon );

  if ( __bDeinitializeCommon__ ) {
    RETURN ( VOID );
  }
  __bDeinitializeCommon__	= TRUE;

  for ( i = 0; i < length ( ppfnGlobalDeinitializers ); i++ ) {
    ( * ppfnGlobalDeinitializers [ i ] ) ();
  }

  RETURN ( VOID );
} /* fnDeinitializeCommon */

/* ------------------------------------------------------------------------- */
void		fnInitializeCommon	( void )
{
  int		i;

  PROCEDURE	( fnInitializeCommon );

  if ( ! __bInitializeCommon__ ) {
    RETURN ( VOID );
  }
  __bInitializeCommon__	= FALSE;
					  
  for ( i = 0; i < length ( ppfnGlobalInitializers ); i++ ) {
    ( * ppfnGlobalInitializers [ i ] ) ();
  }
  atexit ( fnDeinitializeCommon );

  RETURN ( VOID );
} /* fnInitializeCommon */

/* ------------------------------------------------------------------------- */
void		fnInitializeGlobalModule	( void )
{
  PROCEDURE	( fnInitializeGlobalModule );

  fnGetUser ();
  fnGetHost ();

  RETURN ( VOID );
} /* fnInitializeGlobalModule */

/* ------------------------------------------------------------------------- */
void		fnDeinitializeGlobalModule	( void )
{
  PROCEDURE	( fnDeinitializeGlobalModule );
  RETURN ( VOID );
} /* fnDeinitializeGlobalModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
