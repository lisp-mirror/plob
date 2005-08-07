/* -------------------------------------------------------------------------
| Module	plobdadmin.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/11/23
| Description	PLOB admin utility
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

#include	"plobdadmin.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Globals
 ------------------------------------------------------------------------- */
static PADMINAPPL	pGlobalAdmin		= NULL;

typedef BOOL ( *LPFNCOMMAND ) ( PADMINAPPL	pAdmin,
				BOOL		bMandatory );

typedef struct {
  LPCSTR	pszCommand;
  LPCSTR	pszHelp;
  LPFNCOMMAND	pfnCommand;
}	DISPATCHENTRY, * PDISPATCHENTRY;
typedef const DISPATCHENTRY *	PCDISPATCHENTRY;

static const char	szFormatPrompt []	= "%s>";
#define			SZSTDIN			"-"
static const char	szStdIn []		= SZSTDIN;

#define	PLOBDRCVAR	"PLOBDRC"
static const char	szPlobdRcVar []		= PLOBDRCVAR;

#define	PLOBDRCFILE	".plobdrc"
static const char	szPlobdRcFile []	= PLOBDRCFILE;

#define	PLOBDADMIN	"plobdadmin"
static const char	szPlobdAdmin []		= PLOBDADMIN;

static const char	szMissingArgument []	= "Missing <%s> argument";

/* -------------------------------------------------------------------------
| Prototypes
 ------------------------------------------------------------------------- */
static LPSTR	fnAdminFormatDecimal	( ULONG		luNumber,
					  LPSTR		pszNumber,
					  size_t	nNumber );
static int 	fnAdminErrorHandler	( ERRORLEVEL	eLevel,
					  LPCSTR	lpszContinue,
					  LPCSTR	lpszErrorMsg );
static PADMINAPPL fnAdminInit		( PADMINAPPL	pAdmin,
					  int		argc,
					  char **	argv );
static void	fnAdminDeInit		( PADMINAPPL	pAdmin );
static void	fnAdminAtExit		( void );

static LPFILE	fnAdminStreamIn		( PADMINAPPL	pAdmin );
static LPFILE	fnAdminStreamOut	( PADMINAPPL	pAdmin );

static BOOL	fnAdminAssertOpen	( PADMINAPPL	pAdmin,
					  BOOL		bOpen );
static void	fnAdminClose		( PADMINAPPL	pAdmin,
					  BOOL		bWithGC );

static LPSTR	fnAdminSetRootDirectory	( PADMINAPPL	pAdmin,
					  LPCSTR	pszRootDirectory );
static LPSTR	fnAdminReadRootDirectory( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument,
					  size_t	nArgument );
static LPSTR	fnAdminGetURL		( PADMINAPPL	pAdmin,
					  LPSTR		pszURL,
					  size_t	nURL );
static LPSTR	fnAdminSetURL		( PADMINAPPL	pAdmin,
					  LPCSTR	pszURL );
static LPSTR	fnAdminReadURL		( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument,
					  size_t	nArgument );

static int	fnAdminLineGet		( PADMINAPPL	pAdmin,
					  LPCSTR	pszPrompt );

static LPFILE	fnAdminFilePush		( PADMINAPPL	pAdmin,
					  LPSTR		pszName );
static BOOL	fnAdminFilePop		( PADMINAPPL	pAdmin );
static int	fnAdminFileLineInc	( PADMINAPPL	pAdmin );
static void	fnAdminFileStackPrint	( PADMINAPPL	pAdmin );

static void	fnAdminArgPush		( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument );
static LPSTR	fnAdminArgPeek		( PADMINAPPL	pAdmin );
static LPSTR	fnAdminArgPeekNotOption	( PADMINAPPL	pAdmin );
static LPSTR	fnAdminArgPeekGet	( PADMINAPPL	pAdmin,
					  LPCSTR	pszArgumentName,
					  BOOL		bOptional,
					  LPSTR		pszArgument,
					  size_t	nArgument );
static LPSTR	fnAdminArgPop		( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument,
					  size_t	nArgument );
static int	fnAdminArgPopAll	( PADMINAPPL	pAdmin );

static int	fnAdminParse		( PADMINAPPL	pAdmin,
					  LPSTR		pszLine );
static LPSTR	fnAdminArgGet		( PADMINAPPL	pAdmin,
					  LPCSTR	pszArgumentName,
					  LPSTR		pszArgument,
					  size_t	nArgument );

static int	fnAdminCreateStartLocal	( PADMINAPPL	pAdmin,
					  LPCSTR	pszRootDirectory,
					  LPCSTR	pszDirectory,
					  GETACTION	eAction );
static int	fnAdminCreateStartRemote( PADMINAPPL	pAdmin,
					  LPCSTR	pszURL,
					  GETACTION	eAction );
static BOOL	fnAdminCreateStart	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory,
					  GETACTION	eAction );

static BOOL	fnAdminCmdClose		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdCreate	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdEcho		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdFlush		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdHelp		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdInfo		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdQuit		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdReset		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdRestart	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdResume	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdRoot		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdSessions	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdSource	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdConnect	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdShell		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdSilent	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdStatistics	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdStop		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdSuspend	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdURL		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );
static BOOL	fnAdminCmdVerbose	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory );

static BOOL	fnAdminRun		( PADMINAPPL	pAdmin );

int		main			( int		argc,
					  char **	argv );

/* -------------------------------------------------------------------------
| Dispatch table
 ------------------------------------------------------------------------- */
static const DISPATCHENTRY GlobalDispatchTable []	= {

  { "\n\t\tSetting parameters:",
    (LPCSTR) NULL,
    (LPFNCOMMAND) NULL
  },

  { "url [<new url>]",
    "Get or set the current database URL.\n"
    "\t  With this command, " PLOBDADMIN " can be connected to another\n"
    "\t  database or another host running a server process. The passed\n"
    "\t  <new url> is merged with the old url to form the current url.",
    fnAdminCmdURL },

  { "root [<new database root directory>]",
    "Get or set the database root directory.\n"
    "\t  Passing a value of `*' will request the current database root\n"
    "\t  directory from the current server. The root directory is only\n"
    "\t  needed for the `connect' command.",
    fnAdminCmdRoot },

  { "silent",
    "Switch to silent mode.",
    fnAdminCmdSilent },

  { "verbose",
    "Switch to verbose mode.",
    fnAdminCmdVerbose },
  
  { "\n\t\tStarting/stopping a server process:",
    (LPCSTR) NULL,
    (LPFNCOMMAND) NULL
  },

  { "connect [<url>] [<database root directory>]",
    "Connect to a PLOB server process and open a database.\n"
    "\t  If the server for the passed url is not yet running, it will be\n"
    "\t  started.",
    fnAdminCmdConnect },

  { "close [gc]",
    "Close the current database.\n"
    "\t  If the additional keyword gc is passed, a garbage collection\n"
    "\t  is triggered if the number of clients dropped to 0, i.e. when\n"
    "\t  the admin tool was the one and only client using the database.",
    fnAdminCmdClose },

  { "create [<url>]",
    "Create a new database.\n"
    "\t  For this call to be successfull, the master server process\n"
    "\t  must be running on the target host. Normally, this is the PLOB\n"
    "\t  server process started on the target host without any arguments.",
    fnAdminCmdCreate },

  { "restart [<url>]",
    "Restart a PLOB server process.\n"
    "\t  After a restart, all (possibly orphaned) sessions will be\n"
    "\t  deleted, and all client connections are lost.",
    fnAdminCmdRestart },

  { "stop [<url>]",
    "Stop the PLOB server process associated to <url>.\n"
    "\t  Only the PLOB administrator of a database can stop the server\n"
    "\t  process.",
    fnAdminCmdStop },

  { "suspend [<reason>]",
    "Suspend the current server. Clients requests will be blocked until\n"
    "\t  the server resumes its work. When suspending, care is taken that\n"
    "\t  all data is flushed to disk, e.g. during being suspended, a database\n"
    "\t  directory backup can be done.",
    fnAdminCmdSuspend},

  { "resume",
    "Resume the current server, so that clients requests will handled again.",
    fnAdminCmdResume },

  { "\n\t\tServer process administration:",
    (LPCSTR) NULL,
    (LPFNCOMMAND) NULL
  },

  { "flush",
    "Flush the current database.",
    fnAdminCmdFlush },

  { "reset [<url>]",
    "Reset a PLOB database.\n"
    "\t  The next user logging into the resetted database will become the\n"
    "\t  database administrator.",
    fnAdminCmdReset },

  { "\n\t\tServer information:",
    (LPCSTR) NULL,
    (LPFNCOMMAND) NULL
  },

  { "info",
    "Show some infos about the current server.",
    fnAdminCmdInfo },

  { "statistics",
    "Show some server statistics.",
    fnAdminCmdStatistics },

  { "sessions",
    "Show the active sessions of the current database.",
    fnAdminCmdSessions },

  { "\n\t\tUtility functions:",
    (LPCSTR) NULL,
    (LPFNCOMMAND) NULL
  },

  { "#",
    "Comment character.",
    (LPFNCOMMAND) NULL },

  { "! [<command>]",
    "Execute a shell command.",
    fnAdminCmdShell },

  { "help [<items>]",
    "Echo this help text.\n"
    "\t  If <items> are given, only the help text of commands matching\n"
    "\t  the <items> are printed.",
    fnAdminCmdHelp },

  { "echo [-n] [<text>]",
    "Echo <text>. If -n is passed, no newline is printed at the end.",
    fnAdminCmdEcho },

  { "source <file>",
    "Source in commands from <file>. `" SZSTDIN "' means to read from stdin.\n"
    "\t  On startup, " PLOBDADMIN " tries to source in file ${" PLOBDRCVAR
    "};\n"
    "\t  if that file does not exist, file ${HOME}/" PLOBDRCFILE
    " is tried.\n"
    "\t  The very last file can be used to initialize " PLOBDADMIN " to\n"
    "\t  a customized state, for example by setting the database root\n"
    "\t  directory and the url.",
    fnAdminCmdSource },

  { "quit",
    "Quit the " PLOBDADMIN " tool.",
    fnAdminCmdQuit },

  { "\n"
    "A command name can be abbreviated as long as it remains unique among\n"
    "the set of all command names, for example, the `flush' command can be\n"
    "specified as `f', `fl', `flu', `flus' or `flush'.",
    (LPCSTR) NULL,
    (LPFNCOMMAND) NULL
  },
};

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminFormatDecimal	( ULONG		luNumber,
					  LPSTR		pszNumber,
					  size_t	nNumber )
{
  static const int	nColumns	= 3;
  static const char	cDecimal	= ',';
  char			szRaw [ 64 ], szFormatted [ 96 ];

  PROCEDURE ( fnAdminFormatDecimal );

  ASSERT ( pszNumber != NULL );
  ASSERT ( nNumber > 0 );

  sprintf ( szRaw, "%lu", luNumber );
  if ( cDecimal ) {
    int	i, j, o;
    o	= strlen ( szRaw ) % nColumns;
    for ( i = 0, j = 0; szRaw [ i ]; i++ ) {
      if ( o == 0 ) {
	if ( j > 0 ) {
	  szFormatted [ j++ ]	= cDecimal;
	}
	o	= nColumns;
      }
      o--;
      szFormatted [ j++ ]	= szRaw [ i ];
    }
    szFormatted [ j ]	= '\0';
  } else {
    strcpy ( szFormatted, szRaw );
  }

  strncpy ( pszNumber, szFormatted, nNumber );
  pszNumber [ nNumber - 1 ]	= '\0';

  RETURN ( pszNumber );
} /* fnAdminFormatDecimal */

/* ----------------------------------------------------------------------- */
static int 	fnAdminErrorHandler	( ERRORLEVEL	eLevel,
					  LPCSTR	lpszContinue,
					  LPCSTR	lpszErrorMsg )
{
  static int	nErrors		= 0;
  const int	nExitCode	= 254;
  ERRORLEVEL	nErrorMax	= errError;
  int		nReturn = 0, a = 0;
  int		nActionBreak = -1;
  int		nActionContinue = -1;
  int		nActionExit = -1;
  int		nActionToplevel = -1;
  char		szLine [ 256 ];

  PROCEDURE	( fnAdminErrorHandler );

  if ( pGlobalAdmin == NULL ) {
    nReturn	= fnGlobalErrorHandler ( eLevel, lpszContinue, lpszErrorMsg );
    RETURN ( nReturn );
  }

  nErrorMax	= ( pGlobalAdmin->eSource == eFromInteractive ) ?
    errError : errCError;

  switch ( eLevel ) {

  case errInfo:
  case errWarn:
    if ( lpszErrorMsg != NULL ) {
      fprintf ( fnAdminStreamOut ( pGlobalAdmin ), "%s: %s\n",
		ppszErrorLevel2String [ eLevel - errMin ],
		lpszErrorMsg );
      if ( eLevel >= errWarn ) {
	fnAdminFileStackPrint ( pGlobalAdmin );
      }
      fflush ( fnAdminStreamOut ( pGlobalAdmin ) );
    }
    break;

  case errError:
  case errFatal:
    lpszContinue	= NULL;

  case errCError:
    nErrors++;
    if ( lpszErrorMsg != NULL ) {
      fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
		"%s: %s\n",
		ppszErrorLevel2String [ eLevel - errMin ], lpszErrorMsg );
    }
    fnAdminFileStackPrint ( pGlobalAdmin );
    if ( pGlobalAdmin->eSource != eFromInteractive ) {
      if ( lpszContinue != NULL ) {
	fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
		  "action taken was: %s\n", lpszContinue );
      }
      fflush ( fnAdminStreamOut ( pGlobalAdmin ) );
      break;
    }
    fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
	      "Restart actions:\n" );
    if ( lpszContinue != NULL ) {
      nActionContinue	= a++;
      fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
		"%2d: %s\n",
		nActionContinue, lpszContinue );
    }
    if ( pGlobalAdmin->bJmpBufErrorValid ) {
      nActionToplevel	= a++;
      fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
		"%2d: Back to toplevel event loop\n",
		nActionToplevel );
    }
#if	WIN32
    nActionBreak	= a++;
    fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
	      "%2d: Break current process\n",
	      nActionBreak );
#endif
    nActionExit	= a++;
    fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
	      "%2d: Exit current process with error code %d\n",
	      nActionExit, nExitCode );
    fflush ( fnAdminStreamOut ( pGlobalAdmin ) );
    while ( TRUE ) {
      fprintf ( fnAdminStreamOut ( pGlobalAdmin ),
		"[%d]: ", nErrors );
      fflush ( fnAdminStreamOut ( pGlobalAdmin ) );
      fflush ( fnAdminStreamIn ( pGlobalAdmin ) );
      if ( fgets ( szLine, sizeof ( szLine ),
		   fnAdminStreamIn ( pGlobalAdmin ) ) ) {
	a	= 0;
	if ( sscanf ( szLine, "%d", &a ) == 1 ) {
#if	WIN32
	  if ( a == nActionBreak ) {
	    DebugBreak ();
	    break;
	  }
#endif
	  if ( a == nActionContinue ) {
	    RETURN ( 0 );
	  }
	  if ( a == nActionExit ) {
	    exit ( nExitCode );
	    break;
	  }
	  if ( a == nActionToplevel && pGlobalAdmin->bJmpBufErrorValid ) {
	    longjmp ( pGlobalAdmin->jmpbufError, 1 );
	    break;
	  }
	}
      } else {
	break;
      }
    }
    break;

  default:
    break;
  }

  if ( eLevel >= nErrorMax ) {
    if ( pGlobalAdmin->bJmpBufErrorValid ) {
      longjmp ( pGlobalAdmin->jmpbufError, 1 );
    } else {
      exit ( nExitCode );
    }
  }

  RETURN ( nReturn );
} /* fnAdminErrorHandler */

/* ----------------------------------------------------------------------- */
static PADMINAPPL fnAdminInit		( PADMINAPPL	pAdmin,
					  int		argc,
					  char **	argv )
{
  const LPCSTR	pszDefaultProtocol	= szTcp;
  const LPCSTR	pszDefaultHost		= szLocalhost;
  const LPCSTR	pszDefaultDatabase	= DATABASE;

  char	szURL [ MAX_URL ];

  PROCEDURE	( fnAdminInit );

  if ( pAdmin == NULL ) {
    pAdmin	= Malloc ( sizeof ( *pAdmin ) );
    ASSERT ( pAdmin != NULL );
  }

  memset ( pAdmin, 0, sizeof ( *pAdmin ) );

  pAdmin->argc		= argc;
  pAdmin->argv		= argv;

  pAdmin->eSource0	= ( argc <= 1 ) ? eFromInteractive : eFromCommandLine;
  pAdmin->eSource	= pAdmin->eSource0;
  pAdmin->nFiles	= 0;
  pAdmin->Files [ 0 ].pStreamIn		= stdin;
  pAdmin->Files [ 0 ].pszFilename	= strdup ( "stdin" );
  pAdmin->Files [ 0 ].nLine		= 0;
  pAdmin->pStreamOut	= stderr;

  pAdmin->eVerbose	= ( pAdmin->eSource == eFromInteractive ) ?
    eDefaultVerbose : eSilent;

  sprintf ( szURL, szFormatURL,
	    pszDefaultProtocol, pszDefaultHost, pszDefaultDatabase );
  pAdmin->pszURL	= strdup ( szURL );
  pAdmin->oHeap		= NULLOBJID;

  {
    LPSTR	pszPlobdRcFile = getenv ( szPlobdRcVar );
    if ( pszPlobdRcFile != NULL ) {
      if ( *pszPlobdRcFile != '\0' ) {
	fnAdminFilePush ( pAdmin, pszPlobdRcFile );
      }
    } else {
      pszPlobdRcFile = getenv ( "HOME" );
      if ( pszPlobdRcFile != NULL ) {
	char szFile [ 512 ];
	sprintf ( szFile, "%s/%s", pszPlobdRcFile, szPlobdRcFile );
	fnAdminFilePush ( pAdmin, szFile );
      }
    }
  }

  RETURN ( pAdmin );
} /* fnAdminInit */

/* ----------------------------------------------------------------------- */
static void	fnAdminDeInit		( PADMINAPPL	pAdmin )
{

  PROCEDURE	( fnAdminDeInit );

  fnAdminArgPopAll ( pAdmin );
  if ( pAdmin->ppszCommand != NULL ) {
    Free ( pAdmin->ppszCommand );
    pAdmin->ppszCommand	= NULL;
  }

  fnAdminClose ( pAdmin, FALSE );

  fnAdminSetRootDirectory ( pAdmin, NULL );

  fnAdminSetURL ( pAdmin, NULL );

  while ( fnAdminFilePop ( pAdmin ) );

  RETURN ( VOID );
} /* fnAdminDeInit */

/* ----------------------------------------------------------------------- */
static void	fnAdminAtExit		( void )
{
  static int	nEntered	= 0;

  PROCEDURE	( fnAdminAtExit );

  if ( nEntered++ == 0 ) {
    if ( pGlobalAdmin != NULL ) {
      fnAdminDeInit ( pGlobalAdmin );
    }
  }

  RETURN ( VOID );
} /* fnAdminAtExit */

/* ----------------------------------------------------------------------- */
static LPFILE	fnAdminStreamIn		( PADMINAPPL	pAdmin )
{
  PROCEDURE	( fnAdminStreamIn );

  ASSERT ( pAdmin != NULL );
  ASSERT ( pAdmin->nFiles < length ( pAdmin->Files ) );
  ASSERT ( pAdmin->Files [ pAdmin->nFiles ].pStreamIn != NULL );

  RETURN ( pAdmin->Files [ pAdmin->nFiles ].pStreamIn );
} /* fnAdminStreamIn */

/* ----------------------------------------------------------------------- */
static LPFILE	fnAdminStreamOut	( PADMINAPPL	pAdmin )
{
  PROCEDURE	( fnAdminStreamOut );

  ASSERT ( pAdmin->pStreamOut != NULL );

  RETURN ( pAdmin->pStreamOut );
} /* fnAdminStreamOut */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminAssertOpen	( PADMINAPPL	pAdmin,
					  BOOL		bOpen )
{
  BOOL	bIsOpen = FALSE;

  PROCEDURE	( fnAdminAssertOpen );

  ASSERT ( pAdmin != NULL );

  bIsOpen	= ( pAdmin->oHeap != NULLOBJID );

  if ( bIsOpen ) {
    /* Check if the connection is still there: */
    PCLIENT	pClient = (PCLIENT) NULL;
    pClient	= fnClientPlobd ();
    if ( pClient != NULL ) {
      if ( ! fnClientPlobdFlush ( pClient ) ) {
	bIsOpen	= FALSE;
      }
    } else {
      bIsOpen	= FALSE;
    }
  }

  if ( ! bIsOpen ) {
    BOOL	bJmpBufErrorValid;
    jmp_buf	jmpbufError;
    int		nCaughtError;
    if ( pAdmin->pszURL == NULL ) {
      fnAdminCmdURL ( pAdmin, TRUE );
    }
    if ( pAdmin->pszURL == NULL ) {
      RETURN ( bIsOpen );
    }
    bJmpBufErrorValid		= pAdmin->bJmpBufErrorValid;
    memcpy ( &jmpbufError, &pAdmin->jmpbufError, sizeof ( jmpbufError ) );
    pAdmin->bJmpBufErrorValid	= TRUE;
    nCaughtError		= setjmp ( pAdmin->jmpbufError );
    if ( nCaughtError != 0 ) {
      pAdmin->oHeap	= NULLOBJID;
      bIsOpen		= FALSE;
    } else {
      bIsOpen		= fnClientDbConnect ( pAdmin->pszURL );
      if ( bIsOpen && bOpen ) {
	pAdmin->oHeap		=
	  fnClientDbOpen ( szPlobdAdmin, 0 );
	bIsOpen			= ( pAdmin->oHeap != NULLOBJID );;
      }
    }
    pAdmin->bJmpBufErrorValid	= bJmpBufErrorValid;
    memcpy ( &pAdmin->jmpbufError, &jmpbufError,
	     sizeof ( pAdmin->jmpbufError ) );
    if ( nCaughtError != 0 && pAdmin->bJmpBufErrorValid ) {
      longjmp ( pAdmin->jmpbufError, ++nCaughtError );
    }
  }

  if ( ! bIsOpen ) {
    pAdmin->oHeap	= NULLOBJID;
  }

  RETURN ( bIsOpen );
} /* fnAdminAssertOpen */

/* ----------------------------------------------------------------------- */
static void	fnAdminClose		( PADMINAPPL	pAdmin,
					  BOOL		bWithGC )
{
  PROCEDURE	( fnAdminClose );

  ASSERT ( pAdmin != NULL );

  if ( pAdmin->oHeap != NULLOBJID ) {
    fnClientDbClose ( pAdmin->oHeap, bWithGC );
    pAdmin->oHeap	= NULLOBJID;
  }

  RETURN ( VOID );
} /* fnAdminClose */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminSetRootDirectory	( PADMINAPPL	pAdmin,
					  LPCSTR	pszRootDirectory )
{
  PROCEDURE	( fnAdminSetRootDirectory );

  ASSERT ( pAdmin != NULL );

  if ( pAdmin->pszRootDirectory != NULL ) {
    free ( pAdmin->pszRootDirectory );
    pAdmin->pszRootDirectory	= NULL;
  }
  if ( pszRootDirectory != NULL ) {
    pAdmin->pszRootDirectory	= strdup ( pszRootDirectory );
  }

  RETURN ( pAdmin->pszRootDirectory );
} /* fnAdminSetRootDirectory */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminReadRootDirectory( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument,
					  size_t	nArgument )
{
  LPSTR	pszReturn	= NULL;
  char	szRootDirectory [ 256 ];

  PROCEDURE	( fnAdminReadRootDirectory );

  ASSERT ( pAdmin != NULL );

  szRootDirectory [ 0 ]	= '\0';
  while ( szRootDirectory [ 0 ] == '\0' ) {
    if ( fnAdminArgGet ( pAdmin, "database root directory", szRootDirectory,
			 sizeof ( szRootDirectory ) ) == NULL ) {
      break;
    }
    if ( strcmp ( szRootDirectory, "*" ) == 0 ) {
      szRootDirectory [ 0 ]	= '\0';
      if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
	fnServerGetDirectory ( sizeof ( szRootDirectory ),
			       szRootDirectory );
      }
    }
    if ( szRootDirectory [ 0 ] != '\0' ) {
      if ( pszArgument != NULL ) {
	strncpy ( pszArgument, szRootDirectory, nArgument );
      }
      pszReturn	= pszArgument;
      break;
    }
  }

  RETURN ( pszReturn );
} /* fnAdminReadRootDirectory */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminGetURL		( PADMINAPPL	pAdmin,
					  LPSTR		pszURL,
					  size_t	nURL )
{
  LPSTR	pszResultURL	= NULL;

  PROCEDURE	( fnAdminGetURL );

  ASSERT ( pAdmin != NULL );
  ASSERT ( pszURL != NULL );
  ASSERT ( nURL > 0 );

  pszURL [ 0 ]	= '\0';
  if ( pAdmin->pszURL == NULL ) {
    fnAdminCmdURL ( pAdmin, TRUE );
    pszResultURL	= pAdmin->pszURL;
  } else if ( fnAdminArgPeekNotOption ( pAdmin ) != NULL ) {
    fnAdminReadURL ( pAdmin, pszURL, nURL );
    pszResultURL	= pszURL;
  } else {
    pszResultURL	= pAdmin->pszURL;
  }

  RETURN ( pszResultURL );
} /* fnAdminGetURL */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminSetURL		( PADMINAPPL	pAdmin,
					  LPCSTR	pszURL )
{
  PROCEDURE	( fnAdminSetURL );

  ASSERT ( pAdmin != NULL );

  if ( pAdmin->pszURL != NULL ) {
    free ( pAdmin->pszURL );
    pAdmin->pszURL	= NULL;
  }
  if ( pszURL != NULL ) {
    pAdmin->pszURL	= strdup ( pszURL );
  }

  RETURN ( pAdmin->pszURL );
} /* fnAdminSetURL */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminReadURL		( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument,
					  size_t	nArgument )
{
  LPSTR	pszReturn	= NULL;
  char	szURL [ MAX_URL ];

  PROCEDURE	( fnAdminReadURL );

  if ( fnAdminArgGet ( pAdmin, "URL", szURL,
		       sizeof ( szURL ) ) != NULL ) {
    char	szMergedURL [ MAX_URL ];
    fnMergeURLs ( szURL,  pAdmin->pszURL,
		  szMergedURL, sizeof ( szMergedURL ) );
    if ( pszArgument != NULL ) {
      strncpy ( pszArgument, szMergedURL, nArgument );
    }
    pszReturn	= pszArgument;
  }

  RETURN ( pszReturn );
} /* fnAdminReadURL */

/* ----------------------------------------------------------------------- */
static int	fnAdminLineGet		( PADMINAPPL	pAdmin,
					  LPCSTR	pszPrompt )
{
  int	nParsed	= -1, i;
  char	szLine [ 512 ];
  LPSTR	pszLine;
  PROCEDURE	( fnAdminLineGet );

  switch ( pAdmin->eSource ) {

  case eFromCommandLine:
    if ( pAdmin->argc > 1 ) {
      for ( nParsed = 0, i = 1; i < pAdmin->argc; nParsed++, i++ ) {
	fnAdminArgPush ( pAdmin, pAdmin->argv [ i ] );
      }
      pAdmin->argc	= 0;
    }
    break;

  case eFromFile:
    pszLine		=
      fgets ( szLine, sizeof ( szLine ), fnAdminStreamIn ( pAdmin ) );
    if ( pszLine != NULL ) {
      fnAdminFileLineInc ( pAdmin );
      nParsed	= fnAdminParse ( pAdmin, szLine );
    } else {
      fnAdminFilePop ( pAdmin );
      nParsed	= ( pAdmin->eSource == eFromFile ) ?
	fnAdminLineGet ( pAdmin, pszPrompt ) : 0;
    }
    break;

  case eFromInteractive:
    if ( pszPrompt != NULL && *pszPrompt != '\0' ) {
      fputs ( pszPrompt, fnAdminStreamOut ( pAdmin ) );
      fflush ( fnAdminStreamOut ( pAdmin ) );
    }
    if ( fgets ( szLine, sizeof ( szLine ), fnAdminStreamIn ( pAdmin ) ) ) {
      fnAdminFileLineInc ( pAdmin );
      nParsed	= fnAdminParse ( pAdmin, szLine );
    }
    break;

  }

  RETURN ( nParsed );
} /* fnAdminLineGet */

/* ----------------------------------------------------------------------- */
static LPFILE	fnAdminFilePush		( PADMINAPPL	pAdmin,
					  LPSTR		pszName )
{
  LPFILE	pFile	= NULL;
  PROCEDURE	( fnAdminFilePush );

  if ( pAdmin->nFiles == length ( pAdmin->Files ) - 1 ) {
    WARN (( "Nesting depth of %d exceeded at opening command file %s",
	    pAdmin->nFiles, pszName ));
  } else {
    pFile	= ( strcmp ( pszName, szStdIn ) == 0 ) ?
      stdin : fopen ( pszName, szStreamRead );
    if ( pFile != NULL ) {
      pAdmin->nFiles++;
      pAdmin->Files [ pAdmin->nFiles ].pStreamIn	= pFile;
      pAdmin->Files [ pAdmin->nFiles ].pszFilename	= strdup ( pszName );
      pAdmin->Files [ pAdmin->nFiles ].nLine	= 0;
      pAdmin->eSource	= eFromFile;
    } else {
      WARN (( "Cannot open command file %s", pszName ));
    }
  }

  RETURN ( pFile );
} /* fnAdminFilePush */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminFilePop		( PADMINAPPL	pAdmin )
{
  BOOL	bPopped	= ( pAdmin->nFiles > 0 );

  PROCEDURE	( fnAdminFilePop );

  if ( bPopped ) {
    if ( pAdmin->Files [ pAdmin->nFiles ].pStreamIn != stdin ) {
      fclose ( pAdmin->Files [ pAdmin->nFiles ].pStreamIn );
    }
    pAdmin->Files [ pAdmin->nFiles ].pStreamIn	= NULL;
    if ( pAdmin->Files [ pAdmin->nFiles ].pszFilename != NULL ) {
      free ( pAdmin->Files [ pAdmin->nFiles ].pszFilename );
      pAdmin->Files [ pAdmin->nFiles ].pszFilename	= NULL;
    }
    pAdmin->nFiles--;
  }
  if ( bPopped && pAdmin->nFiles == 0 ) {
    pAdmin->eSource	= pAdmin->eSource0;
  }

  RETURN ( bPopped );
} /* fnAdminFilePop */

/* ----------------------------------------------------------------------- */
static int	fnAdminFileLineInc	( PADMINAPPL	pAdmin )
{
  int	nLine	= -1;

  PROCEDURE	( fnAdminFileLineInc );

  ASSERT ( pAdmin->nFiles < length ( pAdmin->Files ) );

  pAdmin->Files [ pAdmin->nFiles ].nLine++;
  nLine	= pAdmin->Files [ pAdmin->nFiles ].nLine;

  RETURN ( nLine );
} /* fnAdminFileLineInc */

/* ----------------------------------------------------------------------- */
static void	fnAdminFileStackPrint	( PADMINAPPL	pAdmin )
{
  int	i;

  PROCEDURE	( fnAdminFileStackPrint );

  for ( i = pAdmin->nFiles; i > 0; i-- ) {
    fprintf ( fnAdminStreamOut ( pAdmin ),
	      "       Offending command in file %s, line %d\n",
	      pAdmin->Files [ i ].pszFilename,
	      pAdmin->Files [ i ].nLine );
  }

  RETURN ( VOID );
} /* fnAdminFileStackPrint */

/* ----------------------------------------------------------------------- */
static void	fnAdminArgPush		( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument )
{
  PROCEDURE	( fnAdminArgPush );

  if ( pAdmin->nCommand >= pAdmin->nMaxCommand ) {
    int	nMaxCommand	= pAdmin->nMaxCommand + 16;

    pAdmin->ppszCommand	= ( pAdmin->ppszCommand != NULL ) ?
      ReAlloc ( pAdmin->ppszCommand,
		sizeof ( pAdmin->ppszCommand ) * nMaxCommand ) :
      Malloc ( sizeof ( pAdmin->ppszCommand ) * nMaxCommand );

    ASSERT ( pAdmin->ppszCommand != NULL );

    memset ( & pAdmin->ppszCommand [ pAdmin->nMaxCommand ],
	     0,
	     ( nMaxCommand - pAdmin->nMaxCommand ) *
	     sizeof ( pAdmin->ppszCommand ) );
	     
    pAdmin->nMaxCommand	= nMaxCommand;
  }

  pAdmin->ppszCommand [ pAdmin->nCommand++ ]	=
    strdup ( pszArgument );

  RETURN ( VOID );
} /* fnAdminArgPush */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminArgPeek		( PADMINAPPL	pAdmin )
{
  LPSTR	pszReturn	= NULL;

  PROCEDURE	( fnAdminArgPeek );

  if ( pAdmin->nCommand > 0 ) {
    pszReturn	= pAdmin->ppszCommand [ 0 ];
    ASSERT ( pszReturn != NULL );
  }

  RETURN ( pszReturn );
} /* fnAdminArgPeek */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminArgPeekNotOption	( PADMINAPPL	pAdmin )
{
  LPSTR	pszReturn	= NULL;

  PROCEDURE	( fnAdminArgPeekNotOption );

  pszReturn	= fnAdminArgPeek ( pAdmin );
  if ( pszReturn != NULL && *pszReturn == '-' ) {
    pszReturn	= NULL;
  }

  RETURN ( pszReturn );
} /* fnAdminArgPeekNotOption */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminArgPeekGet	( PADMINAPPL	pAdmin,
					  LPCSTR	pszArgumentName,
					  BOOL		bOptional,
					  LPSTR		pszArgument,
					  size_t	nArgument )
{
  LPSTR	pszReturn	= NULL;

  PROCEDURE	( fnAdminArgPeekGet );

  if ( fnAdminArgPeekNotOption ( pAdmin ) ||
       ( ! bOptional && pAdmin->eSource == eFromInteractive ) ) {
    pszReturn	=
      fnAdminArgGet ( pAdmin, pszArgumentName, pszArgument, nArgument );
  } else if ( ! bOptional ) {
    ERROR (( szMissingArgument, pszArgumentName ));
  }

  RETURN ( pszReturn );
} /* fnAdminArgPeekGet */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminArgPop		( PADMINAPPL	pAdmin,
					  LPSTR		pszArgument,
					  size_t	nArgument )
{
  LPSTR	pszReturn = NULL;

  PROCEDURE	( fnAdminArgPop );

  if ( pAdmin->nCommand > 0 ) {
    ASSERT ( pAdmin->ppszCommand [ 0 ] != NULL );
    if ( pszArgument != NULL ) {
      ASSERT ( nArgument > 0 );
      strncpy ( pszArgument, pAdmin->ppszCommand [ 0 ], nArgument );
      pszArgument [ nArgument - 1 ]	= '\0';
      pszReturn				= pszArgument;
    }
    free ( pAdmin->ppszCommand [ 0 ] );
    pAdmin->ppszCommand [ 0 ]	= NULL;
    memmove ( & pAdmin->ppszCommand [ 0 ],
	      & pAdmin->ppszCommand [ 1 ],
	      ( pAdmin->nCommand - 1 ) * sizeof ( *(pAdmin->ppszCommand) ) );
    pAdmin->nCommand--;
    pAdmin->ppszCommand [ pAdmin->nCommand ]	= NULL;
  }

  RETURN ( pszReturn );
} /* fnAdminArgPop */

/* ----------------------------------------------------------------------- */
static int	fnAdminArgPopAll	( PADMINAPPL	pAdmin )
{
  int	nPopped = 0;

  PROCEDURE	( fnAdminArgPopAll );

  while ( fnAdminArgPeek ( pAdmin ) != NULL ) {
    fnAdminArgPop ( pAdmin, NULL, 0 );
    nPopped++;
  }

  RETURN ( nPopped );
} /* fnAdminArgPop */

/* ----------------------------------------------------------------------- */
static int	fnAdminParse		( PADMINAPPL	pAdmin,
					  LPSTR		pszLine )
{
  static const char	szDelimiters []	= "\t\n\r ";

  int	nParsed	= 0;
  LPSTR	pszToken = NULL;

  PROCEDURE	( fnAdminParse );

  for ( pszToken = strtok ( pszLine, szDelimiters );
	pszToken != NULL;
	pszToken = strtok ( NULL, szDelimiters ) ) {
    fnAdminArgPush ( pAdmin, pszToken );
    nParsed++;
  }

  RETURN ( nParsed );
} /* fnAdminParse */

/* ----------------------------------------------------------------------- */
static LPSTR	fnAdminArgGet		( PADMINAPPL	pAdmin,
					  LPCSTR	pszArgumentName,
					  LPSTR		pszArgument,
					  size_t	nArgument )
{
  LPSTR	pszReturn	= NULL;

  PROCEDURE	( fnAdminArgGet );

  if ( fnAdminArgPeek ( pAdmin ) ) {
    pszReturn	= fnAdminArgPop ( pAdmin, pszArgument, nArgument );
  } else {
    int		nParsed	= -1;
    char	szPrompt [ 512 ];
    sprintf ( szPrompt, "%s? ", pszArgumentName );
    switch ( pAdmin->eSource ) {

    case eFromCommandLine:
      break;

    case eFromFile:
      while ( pAdmin->eSource == eFromFile &&
	      ( ( nParsed = fnAdminLineGet ( pAdmin, szPrompt ) ) == 0 ) );
      break;

    case eFromInteractive:
      nParsed	= fnAdminLineGet ( pAdmin, szPrompt );
      break;
    }
    if ( nParsed > 0 ) {
      pszReturn	= fnAdminArgPop ( pAdmin, pszArgument, nArgument );
    } else if ( nParsed < 0 ) {
      ERROR (( szMissingArgument, pszArgumentName ));
    }
  }

  RETURN ( pszReturn );
} /* fnAdminArgGet */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdClose		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  static const char	szKeywordGC []	= "gc";

  BOOL		bFinish	= FALSE;
  LPSTR		pszToken;
  BOOL		bWithGC = FALSE;

  PROCEDURE	( fnAdminCmdClose );

  pszToken	= fnAdminArgPeekNotOption ( pAdmin );
  if ( pszToken != NULL ) {
    bWithGC	= ( strcmp ( pszToken, szKeywordGC ) == 0 );
    if ( bWithGC ) {
      /* Pop gc keyword from argument stack: */
      fnAdminArgPop ( pAdmin, NULL, 0 );
    }
  }
  fnAdminClose ( pAdmin, bWithGC );

  RETURN ( bFinish );
} /* fnAdminCmdClose */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdEcho		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  LPSTR	pszPeeked	= NULL;
  int	nPeeked		= 0;
  BOOL	bNewline	= TRUE;
  BOOL	bFinish		= FALSE;

  PROCEDURE	( fnAdminCmdEcho );

  if ( ( pszPeeked = fnAdminArgPeek ( pAdmin ) ) != NULL &&
       strcmp ( pszPeeked, "-n" ) == 0 ) {
    bNewline	= FALSE;
    fnAdminArgPop ( pAdmin, NULL, 0 );
  }

  while ( ( pszPeeked = fnAdminArgPeek ( pAdmin ) ) != NULL ) {
    fprintf ( fnAdminStreamOut ( pAdmin ), "%s%s",
	      ( ( nPeeked > 0 ) ? szSpace : szEmpty ),
	      pszPeeked );
    fnAdminArgPop ( pAdmin, NULL, 0 );
    nPeeked++;
  }
  fputc ( ( ( bNewline ) ? '\n' : ' ' ), fnAdminStreamOut ( pAdmin ) );
  fflush ( fnAdminStreamOut ( pAdmin ) );

  RETURN ( bFinish );
} /* fnAdminCmdEcho */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdHelp		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  static const char	szFormatShortLine []	= "%s\t- %s\n";
  static const char	szFormatLongLine []	= "%s\n\t  %s\n";

  BOOL	bFinish	= FALSE;
  int	i;

  PROCEDURE	( fnAdminCmdHelp );

  if ( fnAdminArgPeekNotOption ( pAdmin ) != NULL ) {
    LPSTR	pszPeeked	= NULL;
    while ( ( pszPeeked = fnAdminArgPeek ( pAdmin ) ) != NULL ) {
      LPSTR	pszCommand	= ( pszPeeked [ 0 ] == '-' ) ?
	&  pszPeeked [ 1 ] : pszPeeked;
      int	nCommand	= strlen ( pszCommand );
      for ( i = 0; i < length ( GlobalDispatchTable ); i++ ) {
	int	n;
	for ( n = 0; GlobalDispatchTable [ i ].pszCommand [ n ] > ' '; n++ );
	if ( GlobalDispatchTable [ i ].pfnCommand != (LPFNCOMMAND) NULL &&
	     nCommand <= n &&
	     strncmp ( pszCommand, GlobalDispatchTable [ i ].pszCommand,
		       nCommand ) == 0 ) {
	  fprintf ( fnAdminStreamOut ( pAdmin ),
		    ( ( strlen ( GlobalDispatchTable [ i ].pszCommand ) < 8 ) ?
		      szFormatShortLine : szFormatLongLine ),
		    ( ( GlobalDispatchTable [ i ].pszCommand != NULL ) ?
		      GlobalDispatchTable [ i ].pszCommand : szEmpty ),
		    ( ( GlobalDispatchTable [ i ].pszHelp != NULL ) ?
		      GlobalDispatchTable [ i ].pszHelp : szEmpty ) );
	}
      }
      fnAdminArgPop ( pAdmin, NULL, 0 );
    }
  } else {
    for ( i = 0; i < length ( GlobalDispatchTable ); i++ ) {
      fprintf ( fnAdminStreamOut ( pAdmin ),
		( ( strlen ( GlobalDispatchTable [ i ].pszCommand ) < 8 ) ?
		  szFormatShortLine : szFormatLongLine ),
		( ( GlobalDispatchTable [ i ].pszCommand != NULL ) ?
		  GlobalDispatchTable [ i ].pszCommand : szEmpty ),
		( ( GlobalDispatchTable [ i ].pszHelp != NULL ) ?
		  GlobalDispatchTable [ i ].pszHelp : szEmpty ) );
    }
  }
  fflush ( fnAdminStreamOut ( pAdmin ) );

  RETURN ( bFinish );
} /* fnAdminCmdHelp */

/* ----------------------------------------------------------------------- */
static int	fnAdminCreateStartLocal	( PADMINAPPL	pAdmin,
					  LPCSTR	pszRootDirectory,
					  LPCSTR	pszDirectory,
					  GETACTION	eAction )
{
  int	nErrors	= 0;
  char	szCurrentDirectory [ 256 ];
  int	nChdir;

  PROCEDURE	( fnAdminCreateStartLocal );

  getcwd ( szCurrentDirectory, sizeof ( szCurrentDirectory ) );
  nChdir	= chdir ( pszRootDirectory );
  if ( nChdir < 0 ) {
    ERROR (( "Could not cd to %s", pszRootDirectory ));
    nErrors++;
  }

  if ( nErrors == 0 ) {
    BOOL	bJmpBufErrorValid;
    jmp_buf	jmpbufError;
    int		nCaughtError;
    strncpy ( szPlobd, "plobd", MAX_FNAME );
    bJmpBufErrorValid		= pAdmin->bJmpBufErrorValid;
    memcpy ( &jmpbufError, &pAdmin->jmpbufError, sizeof ( jmpbufError ) );
    pAdmin->bJmpBufErrorValid	= TRUE;
    nCaughtError		= setjmp ( pAdmin->jmpbufError );
    if ( nCaughtError != 0 ) {
    } else {
      if ( fnStartLocalServer ( pszDirectory, eAction, -1 ) < 0 ) {
	nErrors++;
      }
    }
    chdir ( szCurrentDirectory );
    pAdmin->bJmpBufErrorValid	= bJmpBufErrorValid;
    memcpy ( &pAdmin->jmpbufError, &jmpbufError,
	     sizeof ( pAdmin->jmpbufError ) );
    if ( nCaughtError != 0 && pAdmin->bJmpBufErrorValid ) {
      longjmp ( pAdmin->jmpbufError, ++nCaughtError );
    }
  }

  RETURN ( nErrors );
} /* fnAdminCreateStartLocal */

/* ----------------------------------------------------------------------- */
static int	fnAdminCreateStartRemote( PADMINAPPL	pAdmin,
					  LPCSTR	pszURL,
					  GETACTION	eAction )
{
  int	nErrors	= 0;

  PROCEDURE	( fnAdminCreateStartRemote );

  fnAdminClose ( pAdmin, FALSE );
  if ( fnStartRemoteServer ( pszURL, eAction ) < 0 ) {
    nErrors++;
  }

  RETURN ( nErrors );
} /* fnAdminCreateStartRemote */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCreateStart	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory,
					  GETACTION	eAction )
{
  char		szURL [ MAX_URL ], szRootDirectory [ 256 ];
  LPCSTR	pszURL = NULL, pszRootDirectory = NULL;
  BOOL		bFinish	= FALSE;
  char		szHost [ MAX_URL ], szProtocol [ MAX_URL ],
    szDirectory [ MAX_URL ];
  int		nErrors = 0;

  PROCEDURE	( fnAdminCreateStart );

  pszURL	= fnAdminGetURL ( pAdmin, szURL, sizeof ( szURL ) );
  if ( pszURL == NULL ) {
    nErrors++;
  }

  if ( nErrors == 0 ) {
    szRootDirectory [ 0 ]	= '\0';
    fnSplitURL ( pszURL, szHost, szProtocol, szDirectory );
    if ( strcmp ( szHost, szLocalhost ) == 0 ) {
      if ( pAdmin->pszRootDirectory == NULL ) {
	fnAdminCmdRoot ( pAdmin, TRUE );
	pszRootDirectory	= pAdmin->pszRootDirectory;
      } else if ( fnAdminArgPeekNotOption ( pAdmin ) != NULL ) {
	fnAdminReadRootDirectory ( pAdmin, szRootDirectory,
				   sizeof ( szRootDirectory ) );
	pszRootDirectory	= szRootDirectory;
      } else {
	pszRootDirectory	= pAdmin->pszRootDirectory;
      }
      if ( pszRootDirectory == NULL ) {
	nErrors++;
      }
      if ( nErrors == 0 ) {
	nErrors	+=
	  fnAdminCreateStartLocal ( pAdmin, pszRootDirectory, szDirectory,
				    eAction );
      }
    } else {
      nErrors	+= fnAdminCreateStartRemote ( pAdmin, pszURL, eAction );
    }
  }

  if ( nErrors == 0 ) {
    /* Set the root directory: */
    if ( szRootDirectory [ 0 ] != '\0' ) {
      fnAdminSetRootDirectory ( pAdmin, szRootDirectory );
    }
    /* Set the URL: */
    if ( szURL [ 0 ] != '\0' ) {
      fnAdminClose ( pAdmin, FALSE );
      fnAdminSetURL ( pAdmin, szURL );
    }
  }

  RETURN ( bFinish );
} /* fnAdminCreateStart */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdCreate	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL		bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdCreate );

  bFinish	= fnAdminCreateStart ( pAdmin, bMandatory, (GETACTION)
				       ( (unsigned int) eGetPortActive |
					 (unsigned int) eCreateDatabase |
					 (unsigned int) eStartServer ) );
  RETURN ( bFinish );
} /* fnAdminCmdCreate */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdStop		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  char		szURL [ MAX_URL ];
  LPCSTR	pszURL = NULL;
  BOOL		bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdStop );

  pszURL	= fnAdminGetURL ( pAdmin, szURL, sizeof ( szURL ) );
  if ( pszURL == NULL ) {
    RETURN ( bFinish );
  }

  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    fnClientExit ( pAdmin->oHeap, TRUE );
    pAdmin->oHeap	= NULLOBJID;
  }

  if ( szURL [ 0 ] != '\0' ) {
    fnAdminSetURL ( pAdmin, szURL );
  }

  RETURN ( bFinish );
} /* fnAdminCmdStop */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdSuspend	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  LPSTR	pszPeeked	= NULL;
  BOOL	bFinish	= FALSE;
  char	szReason [ 1024 ];
  int	i = 0;

  PROCEDURE	( fnAdminCmdSuspend );

  szReason [ i ]	= '\0';
  while ( ( pszPeeked = fnAdminArgPeek ( pAdmin ) ) != NULL ) {
    if ( i > 0 ) {
      strncpy ( & szReason [ i++ ], szSpace, sizeof ( szReason ) - 1 - i );
    }
    strncpy ( & szReason [ i ], pszPeeked, sizeof ( szReason ) -1 - i );
    fnAdminArgPop ( pAdmin, NULL, 0 );
    i	+= strlen ( & szReason [ i ] );
  }
  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    fnClientSuspend ( pAdmin->oHeap, szReason );
  }

  RETURN ( bFinish );
} /* fnAdminCmdSuspend */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdFlush		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdFlush );

  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    fnClientDbStabilise ( pAdmin->oHeap );
  }

  RETURN ( bFinish );
} /* fnAdminCmdFlush */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdInfo		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdInfo );

  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    char	szRootDirectory [ 256 ];
    int		i;
    char	szVersions [ esvVersionLen ] [ 16 ];
    FIXNUM	nPID	= fnServerGetDirectory ( sizeof ( szRootDirectory ),
						 szRootDirectory );
    for ( i = esvVersionMin; i <= esvVersionMax; i++ ) {
      FIXNUM	nVersion	=
	fnClientGetVersion ( pAdmin->oHeap, (GETVERSION) i );
      GetVersionString ( nVersion, szVersions [ i - esvVersionMin ] );
    }
    INFO (( "Connected to:     %s\n"
	    "       Server PID:       %d\n"
	    "       Database root:    %s\n"
	    "       Database version: %s\n"
	    "       Server version:   %s\n"
	    "       Client version:   %s",
	    pAdmin->pszURL,
	    nPID, szRootDirectory,
	    szVersions [ esvDatabase - esvVersionMin ],
	    szVersions [ esvServerCode - esvVersionMin ],
	    szVersions [ esvClientCcode - esvVersionMin ] ));
  }

  RETURN ( bFinish );
} /* fnAdminCmdInfo */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdQuit		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= TRUE;

  PROCEDURE	( fnAdminCmdQuit );

  RETURN ( bFinish );
} /* fnAdminCmdQuit */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdReset		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  char		szURL [ MAX_URL ];
  LPCSTR	pszURL = NULL;
  BOOL		bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdReset );

  pszURL	= fnAdminGetURL ( pAdmin, szURL, sizeof ( szURL ) );
  if ( pszURL == NULL ) {
    RETURN ( bFinish );
  }

  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    fnClientDbReset ( pAdmin->oHeap, TRUE );
    fnAdminClose ( pAdmin, TRUE );
  }

  if ( szURL [ 0 ] != '\0' ) {
    fnAdminSetURL ( pAdmin, szURL );
  }

  RETURN ( bFinish );
} /* fnAdminCmdReset */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdRestart	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  char		szURL [ MAX_URL ];
  LPCSTR	pszURL = NULL;
  BOOL		bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdRestart );

  pszURL	= fnAdminGetURL ( pAdmin, szURL, sizeof ( szURL ) );
  if ( pszURL == NULL ) {
    RETURN ( bFinish );
  }

  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    fnClientRestart ( pAdmin->oHeap, TRUE );
    pAdmin->oHeap	= NULLOBJID;
  }

  if ( szURL [ 0 ] != '\0' ) {
    fnAdminSetURL ( pAdmin, szURL );
  }

  RETURN ( bFinish );
} /* fnAdminCmdRestart */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdResume	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL		bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdResume );

  if ( fnAdminAssertOpen ( pAdmin, FALSE ) ) {
    fnClientResume ();
  }

  RETURN ( bFinish );
} /* fnAdminCmdResume */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdRoot		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdRoot );

  if ( bMandatory || fnAdminArgPeekNotOption ( pAdmin ) != NULL ) {
    char	szRootDirectory [ 256 ];
    if ( fnAdminReadRootDirectory ( pAdmin, szRootDirectory,
				    sizeof ( szRootDirectory ) ) != NULL ) {
      fnAdminSetRootDirectory ( pAdmin, szRootDirectory );
    }
  } else if ( pAdmin->pszRootDirectory != NULL ) {
    fprintf ( fnAdminStreamOut ( pAdmin ), "%s\n", pAdmin->pszRootDirectory );
  }

  RETURN ( bFinish );
} /* fnAdminCmdRoot */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdSessions	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL		bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdSessions );

  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    SHORTOBJID	oSessions	= fnClientDbSessions ( pAdmin->oHeap );
    if ( oSessions != NULLOBJID ) {
      SHORTOBJID	oMapper	= NULLOBJID;
      int		nValueKey, nValueData;
      u_int		nTypeTagKey, nTypeTagData;
      int		nMapped = 0;
      INFO (( "Sessions on %s:", pAdmin->pszURL ));
      fnClientTransactionBegin ( pAdmin->oHeap, TRUE );
      for ( nMapped =
	      fnClientBtreemapFirst ( &oMapper, pAdmin->oHeap, oSessions,
				      minmarker, minmarker, eshGreaterEqual,
				      maxmarker, maxmarker, eshLessEqual,
				      FALSE, NULLOBJID, 1,
				      &nValueKey, &nTypeTagKey,
				      &nValueData, &nTypeTagData );
	    nMapped == 1;
	    nMapped =
	      fnClientBtreemapNext ( oMapper, pAdmin->oHeap, 1,
				     &nValueKey, &nTypeTagKey,
				     &nValueData, &nTypeTagData ) ) {
	char	szObject [ 256 ];
	fnClientObjectPrettyPrint ( pAdmin->oHeap, nValueKey,
				    ( immediatep ( nTypeTagKey ) ?
				      nTypeTagKey : eshShortObjIdTag ),
				    szObject, sizeof ( szObject ) );
	fprintf ( fnAdminStreamOut ( pAdmin ), "%c %s\n",
		  ( nValueKey == pAdmin->oHeap ) ? '*' : ' ',
		  szObject );
      }
      fnClientTransactionEnd ( pAdmin->oHeap, TRUE );
    }
  }

  RETURN ( bFinish );
} /* fnAdminCmdSessions */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdSource	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  char	szFile [ 256 ];
  BOOL	bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdSource );

  if ( fnAdminArgPeekGet ( pAdmin, "file to read", FALSE,
			   szFile, sizeof ( szFile ) ) ) {
    fnAdminFilePush ( pAdmin, szFile );
  }

  RETURN ( bFinish );
} /* fnAdminCmdSource */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdConnect	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdConnect );

  bFinish	= fnAdminCreateStart ( pAdmin, bMandatory, (GETACTION)
				       ( (unsigned int) eGetPortActive |
					 (unsigned int) eStartServer ) );

  RETURN ( bFinish );
} /* fnAdminCmdConnect */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdShell		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  LPSTR	pszPeeked	= NULL;
  BOOL	bFinish	= FALSE;
  char	szCommand [ 1024 ];
  int	i = 0;

  szCommand [ i ]	= '\0';
  while ( ( pszPeeked = fnAdminArgPeek ( pAdmin ) ) != NULL ) {
    if ( i > 0 ) {
      strncpy ( & szCommand [ i++ ], szSpace, sizeof ( szCommand ) - 1 - i );
    }
    strncpy ( & szCommand [ i ], pszPeeked, sizeof ( szCommand ) -1 - i );
    fnAdminArgPop ( pAdmin, NULL, 0 );
    i	+= strlen ( & szCommand [ i ] );
  }
  system ( szCommand );

  RETURN ( bFinish );
} /* fnAdminCmdShell */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdSilent	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= FALSE;

  pAdmin->eVerbose	= eSilent;

  RETURN ( bFinish );
} /* fnAdminCmdSilent */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdStatistics	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  static const unsigned int	oneKB = 1024;
  static const unsigned int	halfKB = 512;

  BOOL		bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdStatistics );

  if ( fnAdminAssertOpen ( pAdmin, TRUE ) ) {
    FIXNUM	nMaximumSpace			= 0;
    char	szMaximumSpace [ 32 ];
    char	szMaximumSpaceKB [ 32 ];
    FIXNUM	nAllocatedSpace			= 0;
    char	szAllocatedSpace [ 32 ];
    char	szAllocatedSpaceKB [ 32 ];
    FIXNUM	nUnallocatedSpace		= 0;
    char	szUnallocatedSpace [ 32 ];
    char	szUnallocatedSpaceKB [ 32 ];
    FIXNUM	nUnusedAllocatedSpace		= 0;
    char	szUnusedAllocatedSpace [ 32 ];
    char	szUnusedAllocatedSpaceKB [ 32 ];
    FIXNUM	nAllocatedManagementSpace	= 0;
    char	szAllocatedManagementSpace [ 32 ];
    char	szAllocatedManagementSpaceKB [ 32 ];
    FIXNUM	nNumberOfObjects		= 0;
    char	szNumberOfObjects [ 32 ];
    fnClientDbStatistics ( pAdmin->oHeap, &nMaximumSpace, &nAllocatedSpace,
			   &nUnallocatedSpace, &nUnusedAllocatedSpace,
			   &nAllocatedManagementSpace, &nNumberOfObjects );
    INFO (( "Connected to: %s\n"
	    "       Maximum space              %15s bytes (%s KB)\n"
 	    "       Allocated space            %15s bytes (%s KB)\n"
	    "       Unallocated space          %15s bytes (%s KB)\n"
	    "       Unused allocated space     %15s bytes (%s KB)\n"
	    "       Allocated management space %15s bytes (%s KB)\n"
	    "       Number of objects          %15s",
	    pAdmin->pszURL,
	    fnAdminFormatDecimal ( nMaximumSpace, szMaximumSpace,
				   sizeof ( szMaximumSpace ) ),
	    fnAdminFormatDecimal ( ( (ULONG) nMaximumSpace + halfKB ) /
				   oneKB, szMaximumSpaceKB,
				   sizeof ( szMaximumSpaceKB ) ),
	    fnAdminFormatDecimal ( nAllocatedSpace, szAllocatedSpace,
				   sizeof ( szAllocatedSpace ) ),
	    fnAdminFormatDecimal ( ( (ULONG) nAllocatedSpace + halfKB ) /
				   oneKB, szAllocatedSpaceKB,
				   sizeof ( szAllocatedSpaceKB ) ),
	    fnAdminFormatDecimal ( nUnallocatedSpace, szUnallocatedSpace,
				   sizeof ( szUnallocatedSpace ) ),
	    fnAdminFormatDecimal ( ( (ULONG) nUnallocatedSpace + halfKB ) /
				   oneKB, szUnallocatedSpaceKB,
				   sizeof ( szUnallocatedSpaceKB ) ),
	    fnAdminFormatDecimal ( nUnusedAllocatedSpace,
				   szUnusedAllocatedSpace,
				   sizeof ( szUnusedAllocatedSpace ) ),
	    fnAdminFormatDecimal ( ( (ULONG) nUnusedAllocatedSpace + halfKB ) /
				   oneKB,
				   szUnusedAllocatedSpaceKB,
				   sizeof ( szUnusedAllocatedSpaceKB ) ),
	    fnAdminFormatDecimal ( nAllocatedManagementSpace,
				   szAllocatedManagementSpace,
				   sizeof ( szAllocatedManagementSpace ) ),
	    fnAdminFormatDecimal ( ( (ULONG) nAllocatedManagementSpace +
				     halfKB ) / oneKB,
				   szAllocatedManagementSpaceKB,
				   sizeof ( szAllocatedManagementSpaceKB ) ),
	    fnAdminFormatDecimal ( nNumberOfObjects, szNumberOfObjects,
				   sizeof ( szNumberOfObjects ) ) ));
  }

  RETURN ( bFinish );
} /* fnAdminCmdStatistics */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdURL		( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= FALSE;

  PROCEDURE	( fnAdminCmdURL );

  if ( bMandatory || fnAdminArgPeekNotOption ( pAdmin ) != NULL ) {
    char	szURL [ MAX_URL ];
    if ( fnAdminReadURL ( pAdmin, szURL, sizeof ( szURL ) ) != NULL ) {
      fnAdminClose ( pAdmin, FALSE );
      fnAdminSetURL ( pAdmin, szURL );
    }
  } else if ( pAdmin->pszURL != NULL ) {
    fprintf ( fnAdminStreamOut ( pAdmin ), "%s\n", pAdmin->pszURL );
  }

  RETURN ( bFinish );
} /* fnAdminCmdURL */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminCmdVerbose	( PADMINAPPL	pAdmin,
					  BOOL		bMandatory )
{
  BOOL	bFinish	= FALSE;

  pAdmin->eVerbose	= eVerbose;

  RETURN ( bFinish );
} /* fnAdminCmdVerbose */

/* ----------------------------------------------------------------------- */
static BOOL	fnAdminRun		( PADMINAPPL	pAdmin )
{

  BOOL			bFinish	= FALSE, bReadCommand = TRUE;
  char			szCommand [ 256 ];
  LPCSTR		pszCommand	= NULL;
  int			i, nCommand, nFound;
  PCDISPATCHENTRY	pEntry	= NULL;

  PROCEDURE	( fnAdminRun );

  bReadCommand	= ( pAdmin->eSource == eFromInteractive );

  if ( fnAdminArgPeek ( pAdmin ) == NULL ) {
    /* Read in next line: */
    char	szPrompt [ MAX_URL ];
    sprintf ( szPrompt, szFormatPrompt,
	      ( ( pAdmin->pszURL != NULL && *pAdmin->pszURL != '\0' ) ?
		pAdmin->pszURL : szPlobdAdmin ) );
    if ( fnAdminLineGet ( pAdmin, szPrompt ) < 0 ) {
      RETURN ( TRUE );
    }
  }

  while ( bReadCommand || fnAdminArgPeek ( pAdmin ) != NULL ) {
    bReadCommand	= FALSE;
    if ( fnAdminArgGet ( pAdmin, "command", szCommand,
			 sizeof ( szCommand ) ) == NULL ) {
      break;
    }
    if ( szCommand [ 0 ] == '#' ) {
      fnAdminArgPopAll ( pAdmin );
      break;
    }
    if ( pAdmin->eSource == eFromCommandLine && szCommand [ 0 ] != '-' ) {
      WARN (( "Options must begin with a leading '-'" ));
    }
    pszCommand	= ( szCommand [ 0 ] == '-' ) ?
      & szCommand [ 1 ] : szCommand;
    nCommand	= strlen ( pszCommand );
    nFound	= -1;
    if ( nCommand > 0 ) {
      for ( i = 0, nFound = 0; i < length ( GlobalDispatchTable ) && nFound < 2; i++ ) {
	int	n;
	for ( n = 0; GlobalDispatchTable [ i ].pszCommand [ n ] > ' '; n++ );
	if ( GlobalDispatchTable [ i ].pfnCommand != (LPFNCOMMAND) NULL &&
	     nCommand <= n &&
	     strncmp ( pszCommand, GlobalDispatchTable [ i ].pszCommand,
		       nCommand ) == 0 ) {
	  if ( nFound == 0 ) {
	    pEntry	= &GlobalDispatchTable [ i ];
	  }
	  nFound++;
	}
      }
    }
    switch ( nFound ) {
    case -1:
      break;

    case 0:
      WARN (( "Unknown command %s; use `help' for obtaining help",
	      szCommand ));
      break;

    case 1:
      {
	BOOL	bJmpBufErrorValid;
	jmp_buf	jmpbufError;
	int	nCaughtError;
	bJmpBufErrorValid		= pAdmin->bJmpBufErrorValid;
	memcpy ( &jmpbufError, &pAdmin->jmpbufError, sizeof ( jmpbufError ) );
	pAdmin->bJmpBufErrorValid	= TRUE;
	nCaughtError			= setjmp ( pAdmin->jmpbufError );
	if ( nCaughtError != 0 ) {
	  bFinish	= FALSE;
	} else {
	  bFinish	= (*(pEntry->pfnCommand)) ( pAdmin, FALSE );
	}
	pAdmin->bJmpBufErrorValid	= bJmpBufErrorValid;
	memcpy ( &pAdmin->jmpbufError, &jmpbufError,
		 sizeof ( pAdmin->jmpbufError ) );
	if ( nCaughtError != 0 && pAdmin->bJmpBufErrorValid ) {
	  longjmp ( pAdmin->jmpbufError, ++nCaughtError );
	}
      }
      break;

    default:
      WARN (( "Given command %s is not unique", szCommand ));
      break;
    }
  }

  RETURN ( bFinish );
} /* fnAdminRun */

/* ----------------------------------------------------------------------- */
int		main			( int		argc,
					  char **	argv )
{
  PADMINAPPL	pAdmin = NULL;

  fnMallocFlags ( emFast );
  fnRegisterCcallable ( "fnLISPerrorCallback", fnAdminErrorHandler );

  pAdmin	= fnAdminInit ( pAdmin, argc, argv );
  pGlobalAdmin	= pAdmin;
  atexit ( fnAdminAtExit );

  if ( pAdmin->eSource0 == eFromInteractive && pAdmin->eVerbose >= eVerbose ) {
    char	szVersion [ 16 ];
    FIXNUM	nVersion	=
      fnClientGetVersion ( pAdmin->oHeap, esvClientCcode );
    GetVersionString ( nVersion, szVersion );
    fprintf ( fnAdminStreamOut ( pAdmin ),
	      "PLOB! administration utility %s\n"
	      "%s\n",
	      szVersion, fnGetCopyrightString () );
  }

  while ( ! fnAdminRun ( pAdmin ) );

  exit ( 0 );
}
/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
