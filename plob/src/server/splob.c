/* -------------------------------------------------------------------------
| Module	splob.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.11.93 Created c-postore.c
|		20.12.93 Rename c-postore.c -> c-plob.c, heavily rewritten
|		23.09.96 Divided into server/client code
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
#include	<ctype.h>
#include	<errno.h>
#include	<time.h>
#include	<fcntl.h>
#if WIN32
#include	<process.h>
#include	<rpc/rpc.h>
#else
#include	<signal.h>
#include	<unistd.h>
#include	<sys/resource.h>
#include	<sys/time.h>
#endif

#include	"global.h"
#include	"trmalloc.h"
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
#include	"splobclos.h"
#include	"sploblock.h"
#include	"splobheap.h"
#include	"splobbtree.h"
#include	"splobroot.h"
#include	"splobadmin.h"

#define		RPCNOTYPES
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Stable Heap administration
 ------------------------------------------------------------------------- */
DLLEXPORTVAR int	nGlobalTouched		= 0;

DLLEXPORTVAR OBJID	__oAtomicLock__		= NULLOBJID;
DLLEXPORTVAR LPOBJID	__lpAtomicLock__	= (LPOBJID) NULL;
DLLEXPORTVAR OBJID	__oAtomicUnlock__	= NULLOBJID;
DLLEXPORTVAR LPOBJID	__lpAtomicUnlock__	= (LPOBJID) NULL;

static int		nGlobalKeyAlignment	= 0;

/* -------------------------------------------------------------------------
| Error message formats
 ------------------------------------------------------------------------- */
DLLEXPORTVAR const char		szAlreadyLocked []	=
"There is already an atomic lock on\n"
"       object %s.";

DLLEXPORTVAR const char		szClobberLock []	=
"Clobber the old lock.";

DLLEXPORTVAR const char		szLockFailed []		=
"Atomic lock on\n"
"       object %s\n"
"       failed.";

DLLEXPORTVAR const char		szNotLocked []		=
"There is no atomic lock on\n"
"       object %s.";

DLLEXPORTVAR const char		szNotOpen []		=
"Database is not open.";

static const char	szMissingArgument []	=
"Missing argument for %s option.";

static const char	szNoValues []		=
"The object %s\n"
"       has no values.";

static const char	szDontKnow []		= "dont-know";

static const char	szWriteInvObjId []	=
"Request to write invalid long\n"
"       objid %d (%s),\n"
"       immediate value %d of\n"
"       old class tag 0x%X (%s),\n"
"       new class tag 0x%X (%s),\n"
"       allowed range %d .. %d\n"
"       into %s,\n"
"       index %d (keeping old slot state\n"
"       of %s)";

const char		szOptionRestarted []	= "-restarted";
static const char	szOptionStarted []	= "-started";

const int	nAtomicLockSetMask	= 0x80000000;
const int	nAtomicLockValMask	= 0x00FFFFFF;

/* -------------------------------------------------------------------------
| Static method declarations
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintZombie	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );

/* -------------------------------------------------------------------------
| Static function declarations
 ------------------------------------------------------------------------- */
static OBJID	fnRecycleObject	( LPCLASSINFO lpClassInfo,
				  int nSlots,
				  SHTYPETAG eTypeTagValues,
				  FIXNUM nNumberOfValues );
static BOOL	fnMakeZombie	( OBJID oObjId, LPOBJID lpSHvector );
static void	fnGC		( void );
static void	fnStabilise	( BOOL	bSHstabilise );

/* ----------------------------------------------------------------------- */
void		fnInitializeAfterRegister	( void )
{
  static BOOL	bFirstTime	= TRUE;

  PROCEDURE	( fnInitializeAfterRegister );
  INITIALIZEPLOB;

  if ( bFirstTime ) {
    bFirstTime	= FALSE;
    if ( szGlobalDirectory [ 0 ] != '\0' ) {
      fnStorePid ( szGlobalDirectory );
    }
  }

  RETURN ( VOID );
} /* fnInitializeAfterRegister */

/* ----------------------------------------------------------------------- */
static time_t	tGlobalZero;
static time_t	tGlobalClose;
enum {
  nWaitForExit	= 600 /* seconds */
};

/* ----------------------------------------------------------------------- */
static void
#if WIN32
__cdecl
#endif
fnWaitForExit (
#if WIN32
	       void *		pArgument
#else
	       int		nSignal
#endif
	       )
{
  time_t		tCurrent;
  double		dfWaited;
  int			nWaited;
#if !WIN32
  struct itimerval	TimerValue, TimerValueOld;
#endif

  PROCEDURE	( fnWaitForExit );

#if !WIN32
  memset ( &TimerValue, 0, sizeof ( TimerValue ) );
  memset ( &TimerValueOld, 0, sizeof ( TimerValueOld ) );
#endif

  while ( TRUE ) {
    if ( tGlobalClose == tGlobalZero || StableHeap_is_open ) {
#if WIN32
      sleep ( nWaitForExit );
#else
      /* Set up a new timer event: */
      TimerValue.it_value.tv_sec	= nWaitForExit;
      setitimer ( ITIMER_REAL, &TimerValue, &TimerValueOld );
      signal ( SIGALRM, fnWaitForExit );
      RETURN ( VOID );
#endif
    } else {
      time ( &tCurrent );
      dfWaited	= difftime ( tCurrent, tGlobalClose );
      dfWaited	= ABS ( dfWaited );
      nWaited	= (int) ( dfWaited + 0.5 );
      if ( nWaitForExit < nWaited ) {
	LOG (( "Exit on timeout." ));
	exit ( 0 );
      } else {
#if WIN32
      sleep ( nWaitForExit - nWaited + 1 );
#else
      /* Set up a new timer event: */
      TimerValue.it_value.tv_sec	= nWaitForExit - nWaited + 1;
      setitimer ( ITIMER_REAL, &TimerValue, &TimerValueOld );
      signal ( SIGALRM, fnWaitForExit );
      RETURN ( VOID );
#endif
      }
    }
  }
  RETURN ( VOID );
} /* fnWaitForExit */

/* ----------------------------------------------------------------------- */
void		fnInitializePlobModule		( void )
{
  PROCEDURE	( fnInitializePlobModule );

  INITIALIZE_SSTORE ();

  /* Make sure that the struct's components offsets match the sh-vector
     indices. If one of the following ASSERTs fails, the eshSHvectorIdx...-
     constants have been modified without reflecting these modifications
     in the corresponding structs POSTOREHEADER resp. PLOBHEADER (or vice
     versa): */
  ASSERT ( Offset_matches_Index ( POSTOREHEADER, nObjIds,
				  eshSHvectorIdxObjIds ) );
  ASSERT ( Offset_matches_Index ( POSTOREHEADER, nSize,
				  eshSHvectorIdxSize ) );
  ASSERT ( Offset_matches_Index ( PLOBHEADER, oTypeTag,
				  eshSHvectorIdxTypeTag ) );
  ASSERT ( Offset_matches_Index ( PLOBHEADER, oLockedBy,
				  eshSHvectorIdxLockedBy ) );

  ASSERT ( nSizeOfPostoreWord == sizeof ( psint ) );
  
  RegisterMethod ( eshZombieTag, gfnPrintObjectDetails, mfnPrintZombie );

  memset ( &tGlobalZero, 0, sizeof ( tGlobalZero ) );
  tGlobalClose	= tGlobalZero;
  
  RETURN ( VOID );
} /* fnInitializePlobModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitializePlobModule	( void )
{
  static BOOL	bExit	= TRUE;

  PROCEDURE	( fnDeinitializePlobModule );

  if ( bExit ) {
    bExit			= FALSE;
    __bDeinitializePlob__	= TRUE;
    if ( StableHeap_is_open ) {
      makunbound ( oGlobalSession );
      fnStabilise ( TRUE );
      LOG (( "Closedown of database directory `%s'\n"
	     "       with %d active client(s).",
	     szGlobalDirectory, gfnCount ( Sessions () ) ));
      SH_close ();
      oGlobalMinObjId	= NULLOBJID;
      oGlobalMaxObjId	= NULLOBJID - 1;
      nGlobalTouched	= 0;
      oGlobalLastObjId	= NULLOBJID;
      fnInvalidateAllCaches ();
    }
  }

#if WIN32
  rpc_nt_exit ();
#endif

  RETURN ( VOID );
} /* fnDeinitializePlobModule */

/* ----------------------------------------------------------------------- */
static void	fnHelp			( void )
{
  int		nVersion = fnServerGetVersion ( NULLOBJID, esvServerCode );
  char		szVersion [ 16 ];

  printf ( "Usage:\n"
	   "  plobd [-help] [-debug] [-foreground] [-silent]\n"
	   "        [-directory <database directory>]\n"
	   "        [-port <rpc port number>]\n"
	   "Purpose:\n"
	   "  This is the daemon process for the object-oriented database\n"
	   "  Persistent LISP OBjects (aka PLOB). For normal processing,\n"
	   "  start up this process without any options or arguments.\n"
	   "  When started without any arguments, the process will serve\n"
	   "  the default database directory `%s'.\n"
	   "Options:\n"
	   "  -help        Show this help text.\n"
	   "  -debug       Start up daemon in debug mode.\n"
	   "  -foreground  Do not try to run as background process.\n"
	   "  -silent      Do not echo any error messages at startup.\n"
	   "  -directory   Start the process working on the database"
	   " located in\n"
	   "               <database directory>.\n"
	   "  -port        Start the process working on the"
	   " database directory\n"
	   "               associated to <rpc port number>.\n"
	   "See also:\n"
	   "  http://" STRINGURL " is the official distribution site\n"
	   "  for PLOB.\n"
	   "Version:\n"
	   "  Version %s of %s\n"
	   "  Compiled at %s %s for %s\n"
	   "Copyright:\n"
	   "  %s\n",
	   DATABASE,
	   GetVersionString ( nVersion, szVersion ),
	   fnGetVersionDateString (),
	   fnGetCompileDateString (), fnGetCompileTimeString (),
	   fnGetCompileOpsysString (), fnGetCopyrightString () );
  fflush ( stdout );
} /* fnHelp */

/* ----------------------------------------------------------------------- */
void		fnServerInitializePlob	( int argc, char * argv [] )
{
#if !WIN32
  static const char	szGettingFailed []	=
    "Getting maximum number of files failed; errno %d";
#endif
  int		nErrors = 0, i, nRpcPort, nPID, nDebugLevel = 0;
  BOOL		bDebug, bForeground, bHelp, bRestarted, bStarted, bVerbose;
  MALLFLAGS	mallocFlags	= emFast;
#if !WIN32
  struct rlimit	RLimit;
#endif
  char		szDirectory [ MAX_FNAME ], * pszTail;

  PROCEDURE	( fnServerInitializePlob );

  nGlobalArgC		= argc;
  ppszGlobalArgV	= argv;
  fnLogSetDirectory ( "-" );

  if ( argc > 0 ) {
    strncpy ( szPlobd, argv [ 0 ], MAX_FNAME );
  }

  nDebugLevel		= 0;
  szDirectory [ 0 ]	= '\0';
  mallocFlags		= emFast;
  bDebug		= FALSE;
  bForeground		= FALSE;
  bHelp			= FALSE;
  bRestarted		= FALSE;
  bStarted		= FALSE;
  bVerbose		= TRUE;
  nRpcPort		= -1;
    
  /* Scan options: */
  for ( i = 1; i < argc; i++ ) {
    if ( strcmp ( argv [ i ], szOptionRestarted ) == 0 ) {
      bRestarted	= TRUE;
    } else if ( strcmp ( argv [ i ], szOptionStarted ) == 0 ) {
      bStarted		= TRUE;
    } else if ( strcmp ( argv [ i ], szOptionDirectory ) == 0 ) {
      if ( ++i >= argc ) {
	if ( bVerbose ) {
	  LOG (( szMissingArgument, szOptionDirectory ));
	}
	nErrors++;
      } else {
	strncpy ( szDirectory, argv [ i ], sizeof ( szDirectory ) );
      }
    } else if ( strcmp ( argv [ i ], szOptionPort ) == 0 ) {
      if ( ++i >= argc ) {
	if ( bVerbose ) {
	  LOG (( szMissingArgument, szOptionPort ));
	}
	nErrors++;
      } else {
	nRpcPort	= (int) strtoul ( argv [ i ], &pszTail, 10 );
      }
    } else if ( strcmp ( argv [ i ], "-silent" ) == 0 ) {
      bVerbose		= FALSE;
    } else if ( strcmp ( argv [ i ], "-foreground" ) == 0 ) {
      bForeground	= TRUE;
    } else if ( strcmp ( argv [ i ], "-debug" ) == 0 ) {
      mallocFlags	= emDebug;
      if ( i + 1 < argc && isdigit ( argv [ i + 1 ] [ 0 ] ) ) {
	nDebugLevel	= (int) strtoul ( argv [ ++i ], &pszTail, 10 );
	mallocFlags	= (MALLFLAGS) nDebugLevel;
      }
    } else if ( argv [ i ] [ 0 ] == '-' &&
		( argv [ i ] [ 1 ] == 'h' || argv [ i ] [ 1 ] == 'H' ) ) {
      bHelp		= TRUE;
    } else if ( argv [ i ] [ 0 ] == '-' ) {
      if ( bVerbose ) {
	LOG (( "Unknown option `%s' was passed; try `-h' for help.",
	       argv [ i ] ));
      }
      nErrors++;
    }
  }

  if ( bHelp ) {
    fnHelp ();
    exit ( 0 );
  }

  fnMallocFlags ( mallocFlags );

  if ( bRestarted ) {
    /* Close all open files: */
#if WIN32
    fnLogClose ();
#else
    errno		= 0;
    RLimit.rlim_max	= 0;
    getrlimit ( RLIMIT_NOFILE, &RLimit );
    if ( RLimit.rlim_max == 0 ) {
      int	nErrNo	= errno;
      if ( bVerbose ) {
	LOG (( szGettingFailed, nErrNo ));
      }
      nErrors++;
    }
    fnLogClose ();
    for ( i = 3; i < RLimit.rlim_max; i++ ) {
      close ( i );
    }
#endif
  }

  if ( nErrors == 0 ) {
    if ( szDirectory [ 0 ] == '\0' ) {
      if ( nRpcPort < 0 ) {
	nRpcPort	= nMasterPort;
      }
      if ( fnGetDirectoryByPort ( szDirectory, sizeof ( szDirectory ),
				  nRpcPort ) == NULL ) {
	if ( nRpcPort == nMasterPort ) {
	  /* Try to create the default database directory: */
	  strncpy ( szDirectory, DATABASE, sizeof ( szDirectory ) );
	  if ( SH_create_database ( szDirectory, fnPLOBerrorCallback ) ) {
	    if ( bVerbose ) {
	      LOG (( "Created default database directory `%s',\n"
		     "       master rpc port %d.",
		     szDirectory, nRpcPort ));
	    }
	  }
	  if ( fnGetDirectoryByPort ( szDirectory, sizeof ( szDirectory ),
				      nRpcPort ) == NULL ) {
	    if ( bVerbose ) {
	      LOG (( "Could not get database directory"
		     " for master rpc port %d.", nRpcPort ));
	    }
	    nErrors++;
	  }
	} else {
	  if ( bVerbose ) {
	    LOG (( "Could not get database directory for rpc port %d.",
		   nRpcPort ));
	  }
	  nErrors++;
	}
      }
    } else if ( nRpcPort < 0 ) {
      nRpcPort	= fnGetPortByDirectory ( szDirectory );
      if ( nRpcPort < 0 ) {
	/* Try to create the database directory: */
	if ( SH_create_database ( szDirectory, fnPLOBerrorCallback ) ) {
	  nRpcPort	= fnGetPortByDirectory ( szDirectory );
	}
	if ( nRpcPort < 0 ) {
	  if ( bVerbose ) {
	    LOG (( "Creating database directory `%s' failed.",
		   szDirectory, nRpcPort ));
	  }
	  nErrors++;
	} else if ( bVerbose ) {
	  LOG (( "Created database directory `%s',\n"
		 "       rpc port %d.",
		 szDirectory, nRpcPort ));
	}
      }
    } else {
      if ( bVerbose ) {
	LOG (( "Usage error, both options %s and %s have been specified.",
	       szOptionDirectory, szOptionPort ));
      }
      nErrors++;
    }
  }

  if ( nErrors == 0 ) {
    nPID	=
      fnGetPidByPort ( szLocalhost, szTcp, nRpcPort, nDefaultTimeout );
    if ( nPID > 0 ) {
      if ( bVerbose ) {
	LOG (( "Process with PID %d is already serving\n"
	       "       rpc port %d, database directory `%s'.",
	       nPID, nRpcPort, szDirectory ));
      }
      nErrors++;
    }
  }

  if ( nErrors > 0 ) {
    if ( bVerbose ) {
      fprintf ( stderr, "       *** %s exits with error code %d\n",
		szPlobd, nErrors );
      fflush ( stderr );
    }
    exit ( nErrors );
  }

  fnLogSetDirectory ( szDirectory );
  if ( ! bRestarted ) {
    fnLogNextVersion ();
  }

#if WIN32
  if ( ! bStarted && ! bRestarted && ! bForeground ) {
    /* Restart the daemon, putting it into background: */
    char	** ppszArgV	= Malloc ( sizeof ( char * ) * ( argc + 2 ) );
    int		a = 0, i;
    ppszArgV [ a++ ]	= szPlobd;
    ppszArgV [ a++ ]	= szOptionStarted;
    for ( i = 1; i < argc; i++ ) {
      ppszArgV [ a++ ]	= argv [ i ];
    }
    ppszArgV [ a++ ]	= NULL;
    if ( spawnv ( _P_DETACH, szPlobd, ppszArgV ) != -1 ) {
      ppszArgV	= Free ( ppszArgV );
      exit ( 0 );
    }
    ppszArgV	= Free ( ppszArgV );
  }
#else
  /* fork() the daemon, putting it into background. */
  if ( ! bRestarted && ! bForeground ) {
    int	nChildPID	= 0;
    errno		= 0;
    nChildPID		= fork ();
    fnLogClose ();
    if ( nChildPID < 0 ) {
      int	nErrNo	= errno;
      if ( bVerbose ) {
	LOG (( "Cannot fork() daemon process with PID %d for\n"
	       "       rpc port %d, database directory `%s'; errno %d.",
	       nPID, nRpcPort, szDirectory, nErrNo ));
      }
      nErrors++;
    }
    if ( nChildPID != 0 ) {
      exit ( 0 );
    }
    errno		= 0;
    RLimit.rlim_max	= 0;
    getrlimit ( RLIMIT_NOFILE, &RLimit );
    if ( RLimit.rlim_max == 0 ) {
      int	nErrNo	= errno;
      if ( bVerbose ) {
	LOG (( szGettingFailed, nErrNo ));
      }
      nErrors++;
    }
    fnLogClose ();
    for ( i = 0; i < RLimit.rlim_max; i++ ) {
      close ( i );
    }
    i	= open ( "/dev/null", O_RDWR );
    dup2 ( i, 1 );
    dup2 ( i, 2 );
    setsid ();
  }
#endif

  {
    char	szDbDirectory [ 256 ];
    int		nPID, nServerVersion;
    char	szServerVersion [ 16 ];
    nPID		= fnServerGetDirectory ( sizeof ( szDbDirectory ),
						 szDbDirectory );
    nServerVersion	= fnServerGetVersion ( NULLOBJID, esvServerCode );

    LOG (( "%started %s, PID %d,\n"
	   "       rpc port %d, database directory `%s',\n"
	   "       server version %s, opsys %s, database root\n"
	   "       directory `%s'.",
	   ( bRestarted ) ? "Res" : "S",
	   szPlobd, nPID, nRpcPort, szDirectory,
	   GetVersionString ( nServerVersion, szServerVersion ),
	   fnGetCompileOpsysString (), szDbDirectory ));
  }

  /* Close the log file; it will be re-opened after the fork done in
     main(): */
  fnLogClose ();
  fnPlobdSetPort ( nRpcPort );
  strncpy ( szGlobalDirectory, szDirectory, MAX_FNAME );
  if ( nRpcPort > nMasterPort ) {
#if WIN32
    /* Set up the thread waiting for the process going idle: */
    _beginthread ( fnWaitForExit, 0, NULL );
#else
    /* Set up an itimer which checks for exit condition: */
    struct itimerval	TimerValue, TimerValueOld;
    memset ( &TimerValue, 0, sizeof ( TimerValue ) );
    memset ( &TimerValueOld, 0, sizeof ( TimerValueOld ) );
    TimerValue.it_value.tv_sec	= nWaitForExit;
    setitimer ( ITIMER_REAL, &TimerValue, &TimerValueOld );
    signal ( SIGALRM, fnWaitForExit );
#endif
  } 

  RETURN ( VOID );
} /* fnServerInitializePlob */

/* -------------------------------------------------------------------------
| Print functions
 ------------------------------------------------------------------------- */
static LPSTR	mfnPrintZombie	( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  SHTYPETAG	nTypeTagOld;
  LPCLASSINFO	lpClassInfoOld;

  PROCEDURE	( mfnPrintZombie );

  if ( lpSHvector [ eshSHvectorIdxSize ] > eshSHvectorIdxFirstData ) {
    nTypeTagOld		=
      OBJID2TYPETAG ( lpSHvector [ eshSHvectorIdxFirstData ] );
    lpClassInfoOld	= (LPCLASSINFO) FindClassInfo ( nTypeTagOld );
    if ( lpClassInfoOld && lpClassInfoOld->lpszTypeName )
      sprintf ( lpszBuffer, "old-class=%s", lpClassInfoOld->lpszTypeName );
  }
  RETURN ( lpszBuffer );
} /* fnPrintZombie */

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
static void	fnGC		( void )
{
  PROCEDURE	( fnGC );

  /* 1998/02/25 HK: Debug: */
  fnClientBtreeClear ( NULLOBJID,
		   LONG2SHORTOBJID ( fnGetRootLockQueue () ) );
  fnSetRootFreeList ( unbound );
  SH_garbage_collect ();
  fnIncGCcounter ();

  RETURN ( VOID );
} /* fnGC */

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnTypeString	( SHTYPETAG	nTypeTag )
{
  LPCLASSINFO	lpClassInfo;

  PROCEDURE	( fnTypeString );

  INITIALIZEPLOB;
  lpClassInfo	= (LPCLASSINFO) FindClassInfo ( nTypeTag );
  RETURN ( ( lpClassInfo ) ? lpClassInfo->lpszTypeName : szDontKnow );
} /* fnTypeString */

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
LPOBJID DLLEXPORT	fnObjId2ObjPtr	( OBJID oObjId,
					  psint nRawIndex,
					  SHTYPETAG nTypeTag,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine )
{
  psint		* pSHvector;

  PROCEDURE	( fnObjId2ObjPtr );

  INITIALIZEPLOB;
  if ( ! StableHeap_is_open ) {
    _ERROR ( lpszFile, lpszProc, nLine, ( szNotOpen ) );
    RETURN ( (LPOBJID) NULL );
  }
  if ( ! ObjId_is_valid ( oObjId ) ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szInvalidObjId, LONG2SHORTOBJID ( oObjId ),
	       LONG2SHORTOBJID ( oGlobalMinObjId ),
	       LONG2SHORTOBJID ( oGlobalMaxObjId ) ) );
    RETURN ( (LPOBJID) NULL );
  }
  pSHvector	= SH_key_to_address ( oObjId );
  if ( pSHvector == NULL ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szCantAddress, LONG2SHORTOBJID ( oObjId  ) ) );
    RETURN ( (LPOBJID) NULL );
  }
  if ( nRawIndex < 0 ||
       nRawIndex >= eshSHvectorIdxFirstObjId +
       pSHvector [ eshSHvectorIdxObjIds ] ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szCantIndex, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ),
	       nRawIndex ) );
    RETURN ( (LPOBJID) NULL );
  }
  if ( nRawIndex > eshSHvectorIdxTypeTag && nTypeTag >= 0 &&
       nTypeTag != ObjId2TypeTag ( pSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    fnUnexpectedTypeTag ( lpszFile, lpszProc, nLine, oObjId, -1, nTypeTag );
    RETURN ( (LPOBJID) NULL );
  }
  nGlobalTouched++;
  RETURN ( & pSHvector [ nRawIndex ] );
} /* fnObjId2ObjPtr */

/* ----------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnObjId2ValPtr	( OBJID oObjId,
					  SHTYPETAG nTypeTag,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine )
{
  psint		* pSHvector;

  PROCEDURE	( fnObjId2ValPtr );

  INITIALIZEPLOB;
  if ( ! StableHeap_is_open ) {
    _ERROR ( lpszFile, lpszProc, nLine, ( szNotOpen ) );
    RETURN ( NULL );
  }
  if ( ! ObjId_is_valid ( oObjId ) ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szInvalidObjId, oObjId,
	       LONG2SHORTOBJID ( oGlobalMinObjId ),
	       LONG2SHORTOBJID ( oGlobalMaxObjId ) ) );
    RETURN ( NULL );
  }
  pSHvector	= SH_key_to_address ( oObjId );
  if ( ! pSHvector ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szCantAddress, LONG2SHORTOBJID ( oObjId  ) ) );
    RETURN ( NULL );
  }
  if ( nTypeTag >= 0 &&
       nTypeTag != ObjId2TypeTag ( pSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    fnUnexpectedTypeTag ( lpszFile, lpszProc, nLine, oObjId, -1, nTypeTag );
    RETURN ( NULL );
  }
  if ( pSHvector [ eshSHvectorIdxSize ] -
       pSHvector [ eshSHvectorIdxObjIds ] <= eshSHvectorIdxFirstObjId ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szNoValues, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ) ) );
    RETURN ( NULL );
  }
  nGlobalTouched++;
  RETURN ( & pSHvector [ eshSHvectorIdxFirstObjId +
		         pSHvector [ eshSHvectorIdxObjIds ] ] );
} /* fnObjId2ValPtr */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
void DLLEXPORT		fnInvalidateAllCaches	( void )
{
  PROCEDURE	( fnInvalidateAllCaches );

  /* Caches of splob: */
  oTypeTagCache			= NULLOBJID;
  nTypeTagCache			= (SHTYPETAG) NULLTYPETAG;

  /* Caches of splobmisc: */
  nFlushModeCache		= flushGet;
  fnPeekHandleDestroyAll ();

  /* Caches of splobstruct: */
  oGlobalPackageDescr		= NULLOBJID;
  oGlobalStructDescr		= NULLOBJID;
  oGlobalStructSlotDescr	= NULLOBJID;
  oGlobalPkgCommonLisp		= NULLOBJID;
  oGlobalSymNil			= NULLOBJID;
  oGlobalSymT			= NULLOBJID;
  oGlobalPkgKeyword		= NULLOBJID;
  oGlobalSymKeywordRead		= NULLOBJID;
  oGlobalSymKeywordWrite	= NULLOBJID;
  oGlobalSymKeywordReadWrite	= NULLOBJID;

  /* Caches of splobclos: */
  oGlobalClassDescr		= NULLOBJID;
  oGlobalClassSlotDescr		= NULLOBJID;
  oGlobalClassDirSlotDescr	= NULLOBJID;
  oGlobalClassEffSlotDescr	= NULLOBJID;
  oGlobalMethodDescr		= NULLOBJID;

  /* Caches of splobroot: */
  oFreeListCache		= NULLOBJID;
  oRootLockQueueCache		= NULLOBJID;
  oRootLockCache		= NULLOBJID;
  nGCcounterCache		= -1;
  oUsersCache			= NULLOBJID;
  oMachsCache			= NULLOBJID;
  oSessionsCache		= NULLOBJID;

  /* Caches of splobbtree: */
  oBTreeSearchByObjIdCache		= NULLOBJID;
  oBTreeSearchByObjIdKeyCache		= NULLOBJID;
  oBTreeSearchByObjIdKeyFoundCache	= NULLOBJID;
  oBTreeSearchByObjIdDataCache		= NULLOBJID;
  nBTreeSearchByObjIdResultCache	= btreeNotFound;

  oBTreeCountCache		= NULLOBJID;
  nBTreeCountCache		= 0;

  RETURN ( VOID );
} /* fnInvalidateAllCaches */

/* -------------------------------------------------------------------------
| Low-low-low-level atomic locking
 ------------------------------------------------------------------------- */
LPOBJID			fnAtomicLock		( OBJID oToLock,
						  OBJID oLockBy,
						  LPCSTR lpszFile,
						  LPCSTR lpszProc,
						  int nLine )
{
  LPOBJID	lpSHvector;
  char		szLockedBy [ 128 ];
  psint		nLock;

  PROCEDURE	( fnAtomicLock );

  INITIALIZEPLOB;

  ASSERT ( boundp ( oToLock ) );
  ASSERT ( ! immediatep ( oToLock ) );

  if ( ! ObjId_is_valid ( oToLock ) ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szInvalidObjId, LONG2SHORTOBJID ( oToLock ),
	       LONG2SHORTOBJID ( oGlobalMinObjId ),
	       LONG2SHORTOBJID ( oGlobalMaxObjId ) ) );
    RETURN ( (LPOBJID) NULL );
  }

  if ( immediatep ( oLockBy ) ) {
    oLockBy	= NULLOBJID;
  }

  if ( oLockBy != NULLOBJID && ! ObjId_is_valid ( oLockBy ) ) {
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Ignore oLockBy.",
	        "Objid %d of oLockBy is out of range %d .. %d.",
	        LONG2SHORTOBJID ( oLockBy ),
		LONG2SHORTOBJID ( oGlobalMinObjId ),
		LONG2SHORTOBJID ( oGlobalMaxObjId ) ) );
    oLockBy	= NULLOBJID;
  }

  lpSHvector	= (LPOBJID) SH_key_to_address ( oToLock );
  if ( lpSHvector == NULL ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szCantAddress, LONG2SHORTOBJID ( oToLock ) ) );
    RETURN ( (LPOBJID) NULL );
  }

  nLock	= lpSHvector [ eshSHvectorIdxLock ];
  if ( ( (unsigned int) nLock & (unsigned int) nAtomicLockSetMask ) != 0 ) {
    /* There is already an atomic lock set onto the object: */
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( szClobberLock, szAlreadyLocked,
	        fnPrintObject ( oToLock, (LPSTR) NULL, 0 ) ) );
    lpSHvector [ eshSHvectorIdxLock ] = 0;
  }

  nLock	= SH_set_lock ( oToLock );
  if ( ( (unsigned int) nLock & (unsigned int) nAtomicLockSetMask ) == 0 ) {
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( szClobberLock, szLockFailed,
		fnPrintObject ( oToLock, (LPSTR) NULL, 0 ) ) );
    lpSHvector	= (LPOBJID) NULL;
  }

  RETURN ( lpSHvector );
} /* fnAtomicLock */

/* ----------------------------------------------------------------------- */
BOOL			fnAtomicUnlock		( OBJID oLocked,
						  OBJID oLockedBy,
						  LPCSTR lpszFile,
						  LPCSTR lpszProc,
						  int nLine )
{
  LPOBJID	lpSHvector;
  OBJID		oLockedByObjId;
  char		szLockedByObjId [ 128 ], szLockedBy [ 128 ];
  BOOL		bUnlocked;
  psint		nLock;

  PROCEDURE	( fnAtomicUnlock );

  INITIALIZEPLOB;

  ASSERT ( boundp ( oLocked ) && ! immediatep ( oLocked ) );

  if ( ! ObjId_is_valid ( oLocked ) ) {
    _ERROR ( lpszFile, lpszProc, nLine, 
	     ( szInvalidObjId, LONG2SHORTOBJID ( oLocked ),
	       LONG2SHORTOBJID ( oGlobalMinObjId ),
	       LONG2SHORTOBJID ( oGlobalMaxObjId ) ) );
    RETURN ( FALSE );
  }

  if ( immediatep ( oLockedBy ) )
    oLockedBy	= NULLOBJID;

  if ( oLockedBy != NULLOBJID && ! ObjId_is_valid ( oLockedBy ) ) {
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Ignore oLockedBy.",
	        "Objid %d of oLockedBy is out of range %d .. %d.",
	        LONG2SHORTOBJID ( oLockedBy ),
		LONG2SHORTOBJID ( oGlobalMinObjId ),
		LONG2SHORTOBJID ( oGlobalMaxObjId ) ) );
    oLockedBy	= NULLOBJID;
  }

  lpSHvector	= (LPOBJID) SH_key_to_address ( oLocked );
  if ( lpSHvector == NULL ) {
    _ERROR ( lpszFile, lpszProc, nLine,
	     ( szCantAddress, LONG2SHORTOBJID ( oLocked ) ) );
    RETURN ( FALSE );
  }

  bUnlocked	= TRUE;
  nLock		= lpSHvector [ eshSHvectorIdxLock ];
  if ( ( (unsigned int) nLock & (unsigned int) nAtomicLockSetMask ) != 0 ) {
    lpSHvector [ eshSHvectorIdxLock ]	= 0;
  } else {
    /* There is no atomic lock set at all onto the object: */
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Ignore atomic unlock",
	        "It looks as if there is no atomic lock set at all on %s.",
	        fnPrintObject ( oLocked, (LPSTR) NULL, 0 ) ) );
    bUnlocked	= FALSE;
  }

  RETURN ( bUnlocked );
} /* fnAtomicUnlock */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
	        fnClientObjectCanModify, "c-sh-can-modify",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  OBJID		oObjId;
  BOOL		bResult;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  bResult	= (BOOL) SH_can_modify ( oObjId );

  UnstoreSession ();
  RETURN ( bResult );
} EndFunction ( fnClientObjectCanModify );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnServerDbClose, "c-sh-close",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bWithGarbageCollection ) ) )
{
  OBJID	oHeap = NULLOBJID, oDeleted = NULLOBJID;
  int	nSessions = 0, nObjects = 0;
  struct stableheap_statistics	Statistics;
  FILE	* pLogFile;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( VOID );
    }
  }

  if ( StableHeap_is_open ) {
    if ( oShortObjIdHeap != NULLOBJID ) {
      oHeap	= SHORT2LONGOBJID ( oShortObjIdHeap );
      oDeleted	= fnSessionDelete ( oHeap );
    }
    if ( ! boundp ( oDeleted ) || oHeap != oDeleted ) {
      /* Looks as if the session wasn't found in the session table;
	 since this is a hint that the closer passed a oShortObjIdHeap
	 not representing a session, set the oGlobalSession to
	 NULLOBJID: */
      oGlobalSession	= NULLOBJID;
    }
    nSessions	= gfnCount ( Sessions () );
    if ( nSessions <= 0 ) {
      LOG (( "Closed database directory `%s',\n"
	     "       now %d client(s).",
	     szGlobalDirectory, nSessions ));
      fnStabilise ( FALSE );
      oGlobalSession	= NULLOBJID;
      if ( boundp ( oHeap ) ) {
	fnDestroyObject ( oHeap, TRUE );
      }
      if ( bWithGarbageCollection ) {
	memset ( &Statistics, 0, sizeof ( Statistics ) );
	SH_statistics ( &Statistics );
	nObjects	= Statistics.number_of_objects;
	fnGC ();
	pLogFile	= fnLogOpen ();
	if ( pLogFile != NULL ) {
	  memset ( &Statistics, 0, sizeof ( Statistics ) );
	  SH_statistics ( &Statistics );
	  fprintf ( pLogFile, "       "
		    "Garbage collection resulted in"
		    " %d - %d = %d objects.\n",
		    nObjects,
		    nObjects - Statistics.number_of_objects,
		    Statistics.number_of_objects );
	  fflush ( pLogFile );
	}
      }
      SH_stabilise ();
      SH_close ();
      oGlobalMinObjId	= NULLOBJID;
      oGlobalMaxObjId	= NULLOBJID - 1;
      nGlobalTouched	= 0;
      oGlobalLastObjId	= NULLOBJID;
      fnInvalidateAllCaches ();
      time ( &tGlobalClose );
    } else {
      /* 1997/09/04 HK: Don't write Logoff messages any longer, it is
         too annoying when using (with-session ...): */
      /*
      LOG (( "Logoff from database directory %s,\n"
	     "       now %d client(s).",
	     szGlobalDirectory, nSessions ));
      */
      oGlobalSession	= NULLOBJID;
      fnDestroyObject ( oHeap, TRUE );
      /* 1997/09/04 HK: Don't stablize any longer, it takes too long
         when using (with-session ...): */
      /* fnStabilise ( TRUE ); */
    }
  }
  UnstoreSession ();
  RETURN ( VOID );
} EndFunction ( fnServerDbClose );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientDbConfiguration, "c-sh-configuration",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( FIXNUM, value_out, pnFlags )
		  and
		  argument ( FIXNUM, value_out, pnMinimumKey )
		  and
		  argument ( FIXNUM, value_out, pnMaximumKey )
		  and
		  argument ( FIXNUM, value_out,
			     pnKeyAlignment ) ) )
{
  struct stableheap_configuration	Configuration;
  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }
  ASSERT ( StableHeap_is_open );

  memset ( &Configuration, 0, sizeof ( Configuration ) );
  SH_configuration ( &Configuration );

  if ( pnFlags != NULL )
    *pnFlags		= Configuration.configuration_flags;
  if ( pnMinimumKey != NULL )
    *pnMinimumKey	= LONG2SHORTOBJID ( Configuration.minimum_key );
  if ( pnMaximumKey != NULL )
    *pnMaximumKey	= LONG2SHORTOBJID ( Configuration.maximum_key );
  if ( pnKeyAlignment != NULL )
    *pnKeyAlignment	= LONG2SHORTOBJID ( Configuration.key_alignment );

  UnstoreSession ();
  RETURN ( TRUE );
} EndFunction ( fnClientDbConfiguration );

/* ----------------------------------------------------------------------- */
static OBJID	fnRecycleObject	( LPCLASSINFO lpClassInfo,
				  int nSlots,
				  SHTYPETAG eTypeTagValues,
				  FIXNUM nNumberOfValues )
{
  OBJID			oVectorFreeList, oObjId;
  LPOBJID		lpSHvector, lpSHvectorFreeList;

  PROCEDURE		( fnRecycleObject );

  oObjId	= NULLOBJID;
  if ( lpClassInfo != NULL && ( lpClassInfo->nTypeFlags & typeRecycleP ) &&
       /* Recycling not yet implemented for object creation with
	  variable sized objects: */
       ! ( lpClassInfo->nTypeFlags &
	   ( typeVarSizeObjIdP | typeVarSizeValueP ) ) ) {

    oVectorFreeList	= GetRootFreeList ();

    if ( immediatep ( oVectorFreeList ) ) {
      lpSHvectorFreeList	= (LPOBJID) NULL;
      oVectorFreeList      	= NULLOBJID;
    } else {
      lpSHvectorFreeList	= AtomicLock ( oVectorFreeList, NULLOBJID );
      if ( ObjId2TypeTag ( lpSHvectorFreeList
			   [ eshSHvectorIdxTypeTag ] ) != eshVectorTag ||
	  lpSHvectorFreeList [ eshSHvectorIdxObjIds ] +
	  eshSHvectorIdxFirstObjId - eshSHvectorIdxFirstData -
	  eshVectorObjIdSize < ( lpClassInfo->nTypeTag >> nTagBits ) ) {
	AtomicUnlock ( oVectorFreeList, NULLOBJID );
	lpSHvectorFreeList	= (LPOBJID) NULL;
	oVectorFreeList		= NULLOBJID;
      }
    }
    if ( boundp ( oVectorFreeList ) ) {
      oObjId	=
	lpSHvectorFreeList [ eshVectorObjIdSize +
			     Cooked2RawIndex ( lpClassInfo->nTypeTag >>
					       nTagBits ) ];
      if ( boundp ( oObjId ) ) {
	AtomicLock ( oObjId, oVectorFreeList );
	lpSHvector	= (LPOBJID) SH_key_to_address ( oObjId );
	if ( ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] ) !=
	     eshZombieTag ) {
	  CERROR (( "Make it a zombie.",
		    "Found non-zombie object %s in free list.",
		    fnPrintObject ( oObjId, (LPSTR) NULL, 0 ) ));
	  fnMakeZombie ( oObjId, lpSHvector );
	}
	lpSHvectorFreeList [ eshVectorObjIdSize +
			     Cooked2RawIndex ( lpClassInfo->nTypeTag >>
					       nTagBits ) ]	=
	  lpSHvector [ eshSHvectorIdxLockedBy ];
	makunbound ( lpSHvector [ eshSHvectorIdxLockedBy ] );
	AtomicUnlock ( oObjId, oVectorFreeList );
      }
      AtomicUnlock ( oVectorFreeList, NULLOBJID );
    }
  }

  RETURN ( oObjId );
} /* fnRecycleObject */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnCreateObject	( SHTYPETAG nTypeTag,
					  FIXNUM nSlots,
					  SHTYPETAG eTypeTagValues,
					  FIXNUM nNumberOfValues )
{
  OBJID			oObjId;
  SHORTOBJID		oShortObjId;
  LPCLASSINFO		lpClassInfo;
  int			n, nValues;
  register LPOBJID	lpSHvector;
  LPFNMETHOD		lpfnMethod;

  PROCEDURE		( fnCreateObject );

  INITIALIZEPLOB;

  oObjId       	= NULLOBJID;
  lpClassInfo	= (LPCLASSINFO) FindClassInfo ( nTypeTag );

  if ( lpClassInfo == NULL ) {
    ERROR (( "Type tag %d is unknown to PLOB.", nTypeTag ));
    RETURN ( oObjId );
  }

  if ( lpClassInfo->nTypeFlags & typeNotAllocatableP ) {
    ERROR (( "Instances of class %s (type tag 0x%X) can't be"
	     " allocated.", lpClassInfo->lpszTypeName,
	     lpClassInfo->nTypeTag ));
    RETURN ( oObjId );
  }

  if ( lpClassInfo->nTypeFlags & typeNotYetImplemented ) {
    ERROR (( "Sorry, instances of class %s (type tag 0x%X) not yet"
	     " implemented.", lpClassInfo->lpszTypeName,
	     lpClassInfo->nTypeTag ));
    RETURN ( oObjId );
  }

  n		=
    AlignBitsToWords ( lpClassInfo->nFixSizeObjId ) +
    eshSHvectorIdxFirstData - eshSHvectorIdxFirstObjId;
  nSlots	=
    ( lpClassInfo->nTypeFlags & typeVarSizeObjIdP ) ?
    n + nSlots : n;

  n		=
    AlignBitsToWords ( lpClassInfo->nFixSizeValue );
  nValues	=
    ( lpClassInfo->nTypeFlags & typeVarSizeValueP ) ?
    n + fnTypeTagSizeValue ( 1, &eTypeTagValues, &nNumberOfValues ) : n;
  oObjId	= NULLOBJID;

  if ( lpClassInfo->nTypeFlags & typeRecycleP ) {
    oObjId	=
      fnRecycleObject ( lpClassInfo, nSlots, eTypeTagValues, nValues );
  }

  if ( ! boundp ( oObjId ) ) {
    oObjId		=
      SH_create_object ( nSlots + nValues + eshSHvectorIdxFirstObjId );
    if ( oObjId != NULLOBJID ) {
      /* Currently the POSTORE objids start at oGlobalMaxObjId going
	 down by the POSTORE key aligment (i.e. 8) for each new allocated
	 object; this fact can be used to make the testing for valid keys
	 work very exact by setting oGlobalMinKey to the last objid
	 given back by SH_create_object: */
      if ( oGlobalLastObjId != NULLOBJID ) {
	/* Check if the created objid has not been created in a
	   prior call: */
	if ( oGlobalMinObjId <= oObjId && oObjId <= oGlobalMaxObjId ) {
	  ERROR (( "Created unexpected objid %d in range %d .. %d",
		   LONG2SHORTOBJID ( oObjId ),
		   LONG2SHORTOBJID ( oGlobalMinObjId ),
		   LONG2SHORTOBJID ( oGlobalMaxObjId ) ));
	}
	oGlobalMinObjId	= MIN ( oGlobalMinObjId, oObjId );
	oGlobalMaxObjId	= MAX ( oGlobalMaxObjId, oObjId );
      } else {
	fnSetGlobalMinMaxObjId ();
      }
      oGlobalLastObjId	= oObjId;
    } else {
      oObjId			= NULLOBJID;
    }
  }
  if ( boundp ( oObjId ) ) {
    lpSHvector	= AtomicLock ( oObjId, oObjId );
    if ( lpSHvector != NULL ) {
      lpSHvector [ eshSHvectorIdxObjIds ]	= nSlots;
      lpSHvector [ eshSHvectorIdxTypeTag ]	= TypeTag2ObjId ( nTypeTag );
      makunbound ( lpSHvector [ eshSHvectorIdxLockedBy ] );
      AtomicUnlock ( oObjId, oObjId );
      oShortObjId	= LONG2SHORTOBJID ( oObjId );
      fnServerPlobdReply ( (void(*)()) fnServerDbCreateObject,
			   &oShortObjId );
      lpfnMethod	= _FindMethod ( nTypeTag, gfnInitializeInstance );
      if ( lpfnMethod != NULL ) {
	if ( ! (BOOL) ( * lpfnMethod ) ( oObjId, lpSHvector, lpClassInfo ) ) {
	  /* If InitializeInstance returns with FALSE, initialization
	     failed: */
	  fnDestroyObject ( oObjId, TRUE );
	  oObjId	= NULLOBJID;
	}
      }
    } else {
      ERROR (( szCantAddress, LONG2SHORTOBJID ( oObjId ) ));
      fnDestroyObject ( oObjId, TRUE );
      oObjId	= NULLOBJID;
    }
  }
  if ( boundp ( oObjId ) ) {
    nGlobalTouched++;
  }

  RETURN ( oObjId );
} /* fnCreateObject */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnServerDbCreateObject, "c-sh-create-object",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTag )
		  and
		  argument ( FIXNUM, value_in, nExtraReferences )
		  and
		  argument ( SHTYPETAG, value_in, eTypeTagValues )
		  and
		  argument ( FIXNUM, value_in, nExtraValues ) ) )
{
  SHORTOBJID	oCreated;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );
  oCreated	=
    LONG2SHORTOBJID ( fnCreateObject ( nTypeTag, nExtraReferences,
				       eTypeTagValues, nExtraValues ) );
  UnstoreSession ();
  RETURN ( oCreated );
} EndFunction ( fnServerDbCreateObject );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerDbCreateObjects, "c-sh-create-objects",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTag )
		  and
		  argument ( FIXNUM, value_in, nExtraReferences )
		  and
		  argument ( SHTYPETAG, value_in, eTypeTagValues )
		  and
		  argument ( FIXNUM, value_in, nExtraValues )
		  and
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_out, pObjIds ) ) )
{
  FIXNUM	nCreated = 0;
  OBJID		oCreated;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( nCreated );
    }
  }
  ASSERT ( StableHeap_is_open );

  for ( /* nCreated = 0 */; nCreated < nObjIds; nCreated++ ) {
    oCreated	= fnCreateObject ( nTypeTag, nExtraReferences,
				   eTypeTagValues, nExtraValues );
    if ( boundp ( oCreated ) ) {
      pObjIds [ nCreated ]	= Long2ShortObjId ( oCreated );
    } else {
      break;
    }
  }

  UnstoreSession ();
  RETURN ( nCreated );
} EndFunction ( fnServerDbCreateObjects );

/* ----------------------------------------------------------------------- */
static BOOL	fnMakeZombie	( OBJID oObjId, LPOBJID lpSHvector )
{
  BOOL		bMutated;
  SHTYPETAG	nTypeTag;
  register int	i, n;

  PROCEDURE	( fnMakeZombie );

  nTypeTag	= ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] );
  n		= lpSHvector [ eshSHvectorIdxSize ];
  bMutated	= (BOOL)
    ( nTypeTag != eshZombieTag && n > eshSHvectorIdxFirstData );
  if ( bMutated ) {
    AtomicLock ( oObjId, oObjId );
    lpSHvector [ eshSHvectorIdxTypeTag ]	=
      TypeTag2ObjId ( eshZombieTag );
    lpSHvector [ eshSHvectorIdxFirstData ]	=
      Fixnum2ObjId ( nTypeTag );
    for ( i = eshSHvectorIdxFirstData + 1; i < n; i++ ) {
      makunbound ( lpSHvector [ i ] );
    }
    AtomicUnlock ( oObjId, oObjId );
  }
  RETURN ( bMutated );
} /* fnMakeZombie */

/* ----------------------------------------------------------------------- */
void DLLEXPORT		fnDestroyObject		( OBJID oObjId,
						  BOOL bKill )
{
  static const char	szIgnoreDestroy []	= "Ignore destroy request.";

  LPOBJID	lpSHvector, lpSHvectorFreeList;
  SHTYPETAG	nTypeTag;
  LPCLASSINFO	lpClassInfo;
  OBJID		oVectorFreeList;

  PROCEDURE	( fnDestroyObject );

  if ( immediatep ( oObjId ) ) {
    CERROR (( szIgnoreDestroy, "Can't destroy immediate object %s.",
	      fnPrintObject ( oObjId, (LPSTR) NULL, 0 ) ));
    RETURN ( VOID );
  }

  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector );
  nTypeTag	= ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] );
  if ( nTypeTag == eshZombieTag ) {
    CERROR (( szIgnoreDestroy,
	      "Can't destroy object %s\n"
	      "       because it is already destroyed.",
	      fnPrintObject ( oObjId, (LPSTR) NULL, 0 ) ));
  } else if ( boundp ( lpSHvector [ eshSHvectorIdxLockedBy ] ) ) {
    char	szLockedBy [ 128 ];
    CERROR (( szIgnoreDestroy,
	      "Can't destroy object %s\n"
	      "       because it is locked by\n"
	      "       object %s.",
	      fnPrintObject ( oObjId, (LPSTR) NULL, 0 ),
	      PrintObject ( lpSHvector [ eshSHvectorIdxLockedBy ],
			    szLockedBy ) ));
  } else if ( gfnDestroy ( oObjId, bKill ) ) {
    if ( fnMakeZombie ( oObjId, lpSHvector ) ) {
      lpClassInfo	= (LPCLASSINFO) FindClassInfo ( nTypeTag );
      if ( ( lpClassInfo->nTypeFlags & typeRecycleP ) &&
	   /* Recycling not yet implemented for object creation with
	      variable sized objects: */
	   ! ( lpClassInfo->nTypeFlags &
	       ( typeVarSizeObjIdP | typeVarSizeValueP ) ) ) {
	oVectorFreeList		= GetRootFreeList ();
	if ( immediatep ( oVectorFreeList ) ) {
	  oVectorFreeList      	= NULLOBJID;
	} else {
	  lpSHvectorFreeList	= AtomicLock ( oVectorFreeList, oObjId );
	  if ( ObjId2TypeTag ( lpSHvectorFreeList
			       [ eshSHvectorIdxTypeTag ] ) != eshVectorTag ||
	       lpSHvectorFreeList [ eshSHvectorIdxObjIds ] +
	       eshSHvectorIdxFirstObjId - eshSHvectorIdxFirstData -
	      eshVectorObjIdSize < ( 1 << ( nTypeTagBits - nTagBits ) ) ) {
	    AtomicUnlock ( oVectorFreeList, oObjId );
	    oVectorFreeList	= NULLOBJID;
	  }
	}
	if ( ! boundp ( oVectorFreeList ) ) {
	  oVectorFreeList	=
	    fnCreateObject ( (SHTYPETAG) eshVectorTag,
			     1 << ( nTypeTagBits - nTagBits ), 
			     NULLTYPETAG, 0 );
	  fnSetRootFreeList ( oVectorFreeList );
	  lpSHvectorFreeList	= AtomicLock ( oVectorFreeList, oObjId );
	}
	lpSHvector [ eshSHvectorIdxLockedBy ]			=
	  lpSHvectorFreeList [ eshVectorObjIdSize +
			       Cooked2RawIndex ( nTypeTag >> nTagBits ) ];
	lpSHvectorFreeList [ eshVectorObjIdSize +
			     Cooked2RawIndex ( nTypeTag >> nTagBits ) ]	=
	  oObjId;
	AtomicUnlock ( oVectorFreeList, oObjId );
	oObjId							= NULLOBJID;
      }
    }
    if ( oObjId != NULLOBJID )
      SH_destroy_object ( oObjId );
  }

  RETURN ( VOID );
} /* fnDestroyObject */

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnServerObjectDestroy, "c-sh-destroy-object",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  OBJID		oObjId;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( VOID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  fnDestroyObject ( oObjId, FALSE );

  UnstoreSession ();
  RETURN ( VOID );
} EndFunction ( fnServerObjectDestroy );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnServerObjectObjIdSize, "c-sh-objid-size",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{ 
  OBJID			oObjId;
  register LPOBJID	lpSHvector;
  FIXNUM		nSize;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (FIXNUM) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector );
  nSize	= lpSHvector [ eshSHvectorIdxObjIds ] +
    eshSHvectorIdxFirstObjId - eshSHvectorIdxFirstData;

  UnstoreSession ();
  RETURN ( nSize );
} EndFunction ( fnServerObjectObjIdSize );

/* ----------------------------------------------------------------------- */
static BOOL	fnEnumSessionKill	( LPVOID	lpUserData,
					  OBJID		oBTree,
					  OBJID		oKey,
					  OBJID		oData,
					  OBJID		oBTreePage,
					  int		nIndex )
{
  PROCEDURE	( fnEnumSessionKill );
  fnDestroyObject ( oData, TRUE );
  RETURN ( TRUE );
} /* fnEnumSessionKill */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnServerDbOpen, "c-sh-open",
		( argument ( CONST_STRING, vector_in, szDirectory )
		  and
		  argument ( CONST_STRING, vector_in, szUserName )
		  and
		  argument ( CONST_STRING, vector_in, szDescription )
		  and
		  argument ( FIXNUM, value_in, nMinAddrInK )
		  and
		  argument ( OBJID, value_out, poObjIdMin )
		  and
		  argument ( OBJID, value_out, poObjIdMax ) ) )
{
  static const char	szErrMsg []		=
    "Client %s@%s:\n"
    "       %s database directory `%s' failed.\n"
    "       Reason: %s";
  static const char	szLoginFailed []	=
    "Login to database failed.";
  static const char	szForbiddenChars []	= "./\\";

  enum {
    nSleep	 = 5 /* Seconds */
  };

  struct stableheap_configuration	Config;
  BOOL			bServer, bFirstOpen, bDone;
  ADMINP		eAdminP;
  OBJID			oUser, oMachine, oDescription, oHeap, oSessions;
  LPSTR			lpszPrompt;
  int			i;
  int			nClientAddr [ 4 ];
  char			szClientName [ MAXNETNAMELEN + 1 ], szReason [ 256 ];
  uid_t			nUid = (uid_t) -1;

  INITIALIZEPLOB;

  eAdminP	= eaAdminFalse;
  bServer	= (BOOL) ( fnPlobdGetPort () >= nMasterPort );
  if ( oGlobalSession == NULLOBJID ) {
    if ( CATCHERROR ) {
      /* In case of errors, assume an unfriendly intruder; so make a
         very uninformational error message and wait for some seconds
         before returning: */
      if ( bServer && eAdminP != eaAdminTrue ) {
	fnStoreError ( errLvlError, szLoginFailed );
	if ( nSleep > 0 ) {
	  sleep ( nSleep );
	}
      }
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }

  /* Look who is visiting us: */
  fnGetClientCred ( fnServerPlobdRequest (), szUserName, &nUid,
		    (gid_t *) NULL, (short *) NULL, (gid_t *) NULL,
		    nClientAddr, szClientName, sizeof ( szClientName ) );

  bFirstOpen	= (BOOL) ( ! StableHeap_is_open );

  if ( bFirstOpen ) {
    if ( bServer ) {
      /* This is the first client running in server mode; check if the
	 directory name contains no forbidden characters: */
      for ( i = 0; szDirectory [ i ]; i++ ) {
	if ( strchr ( szForbiddenChars, szDirectory [ i ] ) ) {
	  /* To make sure that the client uses no "../.." or "/"
	     tricks to reach a maybe unallowed directory, the
	     directory name is checked if it contains path-changing
	     characters. If the PLOB administrator allows access to a
	     non-direct subdirectory, [s]he should put a symbolic link
	     to that directory into the PLOB database root directory: */
	  ERROR (( "Client %s@%s:\n"
		   "       Passed directory name `%s'\n"
		   "       contains one of the forbidden\n"
		   "       characters out of the set `%s'",
		   szUserName, szClientName, szDirectory, szForbiddenChars ));
	  UnstoreSession ();
	  RETURN ( NULLOBJID );
	}
      }
    }
    bDone	= ( SH_open_at_addr ( szDirectory, fnPLOBerrorCallback,
				      fnPLOBsaveCallback,
				      fnPLOBrestoreCallback,
				      (PFNUSERSTABILISE)
				      fnPLOBstabiliseCallback,
				      (caddr_t) ( nMinAddrInK * 1024 ) ) ) ?
      TRUE : FALSE;
    if ( bDone ) {
      strncpy ( szGlobalDirectory, szDirectory, MAX_FNAME );
      fnSetGlobalMinMaxObjId ();
      memset ( &Config, 0, sizeof ( Config ) );
      SH_configuration ( &Config );
      if ( poObjIdMin != NULL )
	*poObjIdMin		= Config.minimum_key;
      if ( poObjIdMax != NULL )
	*poObjIdMax		= Config.maximum_key;
      nGlobalKeyAlignment	= Config.key_alignment;
      if ( nGlobalKeyAlignment != nRequiredKeyAlignment ) {
	ERROR (( "This version of PLOB won't work with the POSTORE"
		 " version linked in here; PLOB relies on a POSTORE key"
		 " alignment of %d; the current POSTORE key alignment is %d.",
		 nRequiredKeyAlignment, nGlobalKeyAlignment ));
	UnstoreSession ();
	RETURN ( NULLOBJID );
      }
      nGlobalTouched		= 0;
      fnInvalidateAllCaches ();
    }
    lpszPrompt	= "Opening";
  } else {
    /* This is not the first client; check if this client wanted to
       open the directory already opened by the first client: */
    if ( strncmp ( szGlobalDirectory, szDirectory, MAX_FNAME ) != 0 ) {
      ERROR (( "Client %s@%s:\n"
	       "       Request to connect to the directory\n"
	       "       %s,\n"
	       "       but the server is already connected to directory\n"
	       "       %s.",
	       szUserName, szClientName, szDirectory, szGlobalDirectory ));
      UnstoreSession ();
      RETURN ( NULLOBJID );
    }
    bDone	= TRUE;
    lpszPrompt	= "Login to";
  }

  if ( bDone ) {
    oUser	= fnUserInsertByName ( szUserName, (int) nUid );
    oMachine	= fnMachInsertByAddr ( nClientAddr, unbound );
    eAdminP	= fnAdminP ( oUser, oMachine );
    switch ( eAdminP ) {
    case eaNoAdmin:
      fnAdminSet ( oUser, oMachine );
      LOG (( "Made %s@%s the PLOB! system administrator.",
	     gfnNameOf ( oUser, (LPINT) NULL ),
	     gfnNameOf ( oMachine, (LPINT) NULL ) ));
      INFO (( "Congratulations, you've become the PLOB! system\n"
	      "       administrator on database directory `%s'",
	      szDirectory ));
      break;
    case eaAdminTrue:
      break;
    default:
      break;
    }
    if ( fnLoginP ( oUser, oMachine, szReason, sizeof ( szReason ) ) ) {
      oDescription	= ( szDescription && szDescription [ 0 ] ) ?
	make_string ( szDescription ) : NULLOBJID;
      oHeap		= make_heap ( oUser, oMachine, oDescription );
      StoreSession ( oHeap );
      if ( bFirstOpen ) {
	oSessions	= Sessions ();
	if ( btreep ( oSessions ) ) {
	  /* Kill all sessions, since there can't be any connection
	     to the client (the connections are always transient): */
	  fnBTreeMapAll ( NULLOBJID, oSessions, fnEnumSessionKill, NULL );
	  fnBTreeClear ( NULLOBJID, oSessions );
	}
      }
      fnSessionInsert ( oHeap );
      oHeap	= LONG2SHORTOBJID ( oHeap );
      /* 1997/09/04 HK: Don't write Login messages any longer, it is
         too annoying when using (with-session ...): */
      if ( bFirstOpen ) {
	int	nDatabaseVersion;
	char	szDatabaseVersion [ 16 ];
	nDatabaseVersion	= fnServerGetVersion ( oHeap, esvDatabase );
	LOG (( "Client %s@%s, now %d client(s):\n"
	       "       %s database directory `%s' version %s succeeded.\n"
	       "       Reason: %s",
	       szUserName, szClientName, gfnCount ( Sessions () ),
	       lpszPrompt, szDirectory,
	       GetVersionString ( nDatabaseVersion, szDatabaseVersion ),
	       szReason ));
      }
    } else {
      if ( bFirstOpen ) {
	/* Close the heap again: */
	fnServerDbClose ( NULLOBJID, FALSE );
      }
      if ( bServer ) {
	LOG (( szErrMsg, szUserName, szClientName, lpszPrompt,
	       szDirectory, szReason ));
	fnStoreError ( errLvlError, szLoginFailed );
	if ( nSleep > 0 ) {
	  sleep ( nSleep );
	}
      } else {
	ERROR (( szErrMsg, szUserName, szClientName, lpszPrompt,
		 szDirectory, szReason ));
      }
      UnstoreSession ();
      RETURN ( NULLOBJID );
    }
  } else {
    oHeap		= NULLOBJID;
  }

  if ( bFirstOpen && oHeap != NULLOBJID ) {
    tGlobalClose	= tGlobalZero;
    fnTypeAddBuiltInClasses ( oHeap );
  }

  UnstoreSession ();
  RETURN ( oHeap );
} EndFunction ( fnServerDbOpen );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnServerObjectFlush, "c-sh-flush-object",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  OBJID		oObjId;
  BOOL		bDone = TRUE;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (BOOL) eshGeneralError );
    }
  }

  if ( StableHeap_is_open && oShortObjId != NULLOBJID ) {
    oObjId	= Short2LongObjId ( oShortObjId );
    gfnFlush ( oObjId );
  }

  RETURN ( bDone );
} EndFunction ( fnServerObjectFlush );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerObjectReadAtIndex, "c-sh-read-index",
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
		  argument ( INTEGER, value_out, pnValue )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTag ) ) )
{
  OBJID			oObjId, oHeap;
  FIXNUM		nRawIndex;
  register LPOBJID	lpSHvector;
  SHTYPETAG		nTypeTag;
  SHLOCK		nLockOld;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );

  if ( nExpectingTypeTag != NULLTYPETAG &&
       nExpectingTypeTag !=
       ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    UNEXPECTED_TYPE_TAG ( oObjId, -1, nExpectingTypeTag );
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  nRawIndex	= Cooked2RawIndex ( nIndex );
  if ( nIndex < 0 ||
       nRawIndex >= eshSHvectorIdxFirstObjId +
       lpSHvector [ eshSHvectorIdxObjIds ] ) {
    ERROR (( szCantIndex, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ), nIndex ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  nLockOld	= fnLockInsert ( oHeap, eshLockElementRead,
				 oObjId, nIndex, (PHPEEK) NULL );
  if ( (int) nLockOld >= 0 ) {
    nTypeTag	= typetagof ( lpSHvector [ nRawIndex ] );
    if ( pnValue != NULL ) {
      *pnValue		= ( immediatep ( nTypeTag ) ) ?
	fnObjId2Immediate ( lpSHvector [ nRawIndex ], nTypeTag ) :
	LONG2SHORTOBJID ( lpSHvector [ nRawIndex ] );
    }
    if ( pnTypeTag != NULL ) {
      *pnTypeTag	= nTypeTag;
    }
    nGlobalTouched++;
  }

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectReadAtIndex );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerObjectReadAtIndices, "c-sh-read-indices",
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
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( int, nObjIds ),
			     vector_out, pObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_out, pnTypeTags ) ) )
{
  OBJID		oObjId, oHeap, oRead;
  LPOBJID	lpSHvector;
  SHTYPETAG	nTypeTag;
  SHLOCK	nLockOld;
  int		nSlots, nSlotsClipped, s, nTotalRead = 0;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );
  if ( nExpectingTypeTag != NULLTYPETAG &&
       nExpectingTypeTag !=
       ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    UNEXPECTED_TYPE_TAG ( oObjId, -1, nExpectingTypeTag );
    RETURN ( eshGeneralError );
  }

  nSlots	= lpSHvector [ eshSHvectorIdxObjIds ] -
    ( eshSHvectorIdxFirstData - eshSHvectorIdxFirstObjId );
  nSlotsClipped	= MIN ( nSlots - nIndex, nObjIds );
  if ( nSlotsClipped > 0 ) {
    if ( nIndex < 0 || nIndex >= nSlots ) {
      ERROR (( szCantIndex, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ),
	       nIndex ));
      UnstoreSession ();
      RETURN ( eshGeneralError );
    }
    nTotalRead	= nSlotsClipped;
    oHeap	= Short2LongObjId ( oShortObjIdHeap );
    nLockOld	= fnLockInsert ( oHeap, eshLockVectorRead, oObjId, -1,
				 (PHPEEK) NULL );
    if ( (int) nLockOld < 0 ) {
      UnstoreSession ();
      RETURN ( nLockOld );
    }
    for ( s = 0; s < nSlotsClipped; s++ ) {
      oRead	= lpSHvector [ eshSHvectorIdxFirstData + nIndex + s ];
      nTypeTag	= typetagof ( oRead );
      if ( pObjIds != NULL ) {
	pObjIds [ s ]		= ( immediatep ( nTypeTag ) ) ?
	  fnObjId2Immediate ( oRead, nTypeTag ) :
	  LONG2SHORTOBJID ( oRead );
      }
      if ( pnTypeTags != NULL ) {
	pnTypeTags [ s ]	= nTypeTag;
      }
    }
  }

  UnstoreSession ();
  RETURN ( nTotalRead );
} EndFunction ( fnServerObjectReadAtIndices );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerObjectReadObjId, "c-sh-read-objid",
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
		  argument ( INTEGER, value_out, pnObjId ) ) )
{
  SHTYPETAG	nTypeTag;
  SHLOCK	nLockOld;
  OBJID		oObjId;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nTypeTag	= eshObjIdTag;
  nLockOld	=
    fnServerObjectReadAtIndex ( oShortObjIdHeap, oShortObjId,
				oExpectingClass, nExpectingTypeTag,
				nIndex, pnObjId, &nTypeTag );
  if ( immediatep ( nTypeTag ) ) {
    oObjId	= Short2LongObjId ( oShortObjId );
    UNEXPECTED_TYPE_TAG ( oObjId, nIndex, eshObjIdTag );
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectReadObjId );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnServerObjectReadValues, "c-sh-read-values",
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
		  argument ( SHTYPETAG, value_in, nElementTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInElements )
		  and
		  argument ( SHTYPETAG, value_out, pnElementTypeTag )
		  and
		  argument ( FIXNUM, value_out, pnSizeInElements )
		  and
		  argument ( VECTOR ( void,
				      fnTypeTagSizeValue(1,&nElementTypeTag,
							 &nSizeInElements ) ),
			     vector_out, pBuffer ) ) )
{
  LPCLASSINFO	pClassInfo;
  int		nAlignment, nBitsPerElement;
  OBJID		oObjId, oHeap;
  LPOBJID	lpSHvector;
  SHLOCK	nLockOld;
  int		nSlots, nValues, nTotalRead = 0;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  if ( pnElementTypeTag != NULL ) {
    *pnElementTypeTag	= (SHTYPETAG) NULLTYPETAG;
  }
  if ( pnSizeInElements != NULL ) {
    *pnSizeInElements	= 0;
  }

  oObjId		= Short2LongObjId ( oShortObjId );

  pClassInfo		= (LPCLASSINFO) FindClassInfo ( nElementTypeTag );
  ASSERT ( pClassInfo != NULL );
  nBitsPerElement	= pClassInfo->nFixSizeValue;
  ASSERT ( nBitsPerElement > 0 );

  nAlignment		=
    ( nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerElement;
  if ( nAlignment > 0 && nIndex % nAlignment != 0 ) {
    char	szObject [ 256 ];
    ERROR (( szInvalidAlignment,
	     nIndex, PrintObject ( oObjId, szObject ), nAlignment ));
    RETURN ( eshGeneralError );
  }

  ASSERT ( pBuffer != NULL );
  lpSHvector		= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );
  if ( nExpectingTypeTag != NULLTYPETAG &&
       nExpectingTypeTag !=
       ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    UNEXPECTED_TYPE_TAG ( oObjId, -1, nExpectingTypeTag );
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  nLockOld	= fnLockInsert ( oHeap, eshLockVectorRead, oObjId, -1,
				 (PHPEEK) NULL );
  if ( (int) nLockOld < 0  ) {
    UnstoreSession ();
    RETURN ( nLockOld );
  }

  memset ( pBuffer, 0,
	   AlignBitsToWords ( nSizeInElements * nBitsPerElement ) *
	   nSizeOfPostoreWord );
  nSlots	= lpSHvector [ eshSHvectorIdxObjIds ];
  nValues	= ( ( lpSHvector [ eshSHvectorIdxSize ] - nSlots -
		      eshSHvectorIdxFirstObjId ) *
		    nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerElement;
  if ( nIndex < 0 || nIndex >= nValues ) {
    ERROR (( szCantIndex, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ), nIndex ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }
  nValues	= MIN ( nValues - nIndex, nSizeInElements );
  if ( nValues > 0 ) {
    nTotalRead	= nValues;
    memcpy ( pBuffer,
	     & lpSHvector [ nSlots + eshSHvectorIdxFirstObjId +
			    AlignBitsToWords ( nIndex * nBitsPerElement ) ],
	     AlignBitsToWords ( nValues * nBitsPerElement ) *
	     nSizeOfPostoreWord );
    if ( pnElementTypeTag != NULL ) {
      *pnElementTypeTag	= nElementTypeTag;
    }
    if ( pnSizeInElements != NULL ) {
      *pnSizeInElements	= nValues;
    }
  }
  nGlobalTouched++;

  UnstoreSession ();
  RETURN ( nTotalRead );
} EndFunction ( fnServerObjectReadValues );

/* ----------------------------------------------------------------------- */
static BOOL	fnEnumSessionFlush	( LPVOID	lpUserData,
					  OBJID		oBTree,
					  OBJID		oKey,
					  OBJID		oData,
					  OBJID		oBTreePage,
					  int		nIndex )
{
  PROCEDURE	( fnEnumSessionFlush );
  gfnFlush ( oKey );
  RETURN ( TRUE );
} /* fnEnumSessionKill */

/* ----------------------------------------------------------------------- */
static void	fnStabilise	( BOOL	bSHstabilise )
{
  static int	nTouched	= -1;
  OBJID		oSessions;

  PROCEDURE	( fnStabilise );

  if ( StableHeap_is_open && nTouched != nGlobalTouched ) {
    /* Flush all open sessions */
    oSessions	= Sessions ();
    if ( btreep ( oSessions ) ) {
      fnBTreeMapAll ( NULLOBJID, oSessions, fnEnumSessionFlush, NULL );
    }
    if ( bSHstabilise ) {
      nGlobalCallCallbacks++;
      nTouched	= nGlobalTouched;
      SH_stabilise ();
      nTouched	= nGlobalTouched;
      nGlobalCallCallbacks--;
    }
  }
} /* fnStabilise */

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnServerDbStabilise, "c-sh-stabilise",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( VOID );
    }
  }

  fnStabilise ( TRUE );
  LOG (( "Stabilised database directory `%s'", szGlobalDirectory ));

  UnstoreSession ();
  RETURN ( VOID );
} EndFunction ( fnServerDbStabilise );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientDbStatistics, "c-sh-statistics",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( FIXNUM, value_out, pnMaximumSpace )
		  and
		  argument ( FIXNUM, value_out, pnAllocatedSpace )
		  and
		  argument ( FIXNUM, value_out,
			     pnUnallocatedSpace )
		  and
		  argument ( FIXNUM, value_out,
			     pnUnusedAllocatedSpace )
		  and
		  argument ( FIXNUM, value_out,
			     pnAllocatedManagementSpace )
		  and
		  argument ( FIXNUM, value_out,
			     pnNumberOfObjects ) ) )
{
  struct stableheap_statistics	Statistics;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }
  ASSERT ( StableHeap_is_open );

  memset ( &Statistics, 0, sizeof ( Statistics ) );
  SH_statistics ( &Statistics );

  if ( pnMaximumSpace != NULL )
    *pnMaximumSpace		= Statistics.maximum_space;
  if ( pnAllocatedSpace != NULL )
    *pnAllocatedSpace		= Statistics.allocated_space;
  if ( pnUnallocatedSpace != NULL )
    *pnUnallocatedSpace		= Statistics.unallocated_space;
  if ( pnUnusedAllocatedSpace != NULL )
    *pnUnusedAllocatedSpace	= Statistics.unused_allocated_space;
  if ( pnAllocatedManagementSpace != NULL )
    *pnAllocatedManagementSpace	= Statistics.allocated_management_space;
  if ( pnNumberOfObjects != NULL )
    *pnNumberOfObjects		= Statistics.number_of_objects;

  UnstoreSession ();
  RETURN ( TRUE );
} EndFunction ( fnClientDbStatistics );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHTYPETAG,
	        fnServerObjectTypeTag, "c-sh-type-tag",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  SHTYPETAG	eTypeTag;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshUnboundTag );
    }
  }
  ASSERT ( StableHeap_is_open );

  eTypeTag	= typetagof ( SHORT2LONGOBJID ( oShortObjId ) );

  UnstoreSession ();
  RETURN ( eTypeTag );
} EndFunction ( fnServerObjectTypeTag );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnServerObjectValueSize, "c-sh-value-size",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  FIXNUM	nSize;
  OBJID		oObjId;
  psint	FAR *	lpSHvector;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );
  nSize	= lpSHvector [ eshSHvectorIdxSize ] -
    lpSHvector [ eshSHvectorIdxObjIds ] - eshSHvectorIdxFirstObjId;

  UnstoreSession ();
  RETURN ( nSize );
} EndFunction ( fnServerObjectValueSize );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientObjectValueSize, "c-sh-value-size",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  FIXNUM	nSize;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  nSize	= fnServerObjectValueSize ( oShortObjIdHeap, oShortObjId );

  UnstoreSession ();
  RETURN ( nSize );
} EndFunction ( fnClientObjectValueSize );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerObjectWriteAtIndex, "c-sh-write-index",
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
		  argument ( FIXNUM, value_in, nValue )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagValue ) ) )
{
  OBJID		oObjId, oHeap;
  LPOBJID	lpSHvector;
  FIXNUM	nRawIndex, nValueOld, nValueNew;
  SHLOCK	nLockOld;
  SHTYPETAG	nTypeTagValueNew = nTypeTagValue;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );
  if ( nExpectingTypeTag != NULLTYPETAG &&
       nExpectingTypeTag !=
       ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    UNEXPECTED_TYPE_TAG ( oObjId, -1, nExpectingTypeTag );
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  nRawIndex	= Cooked2RawIndex ( nIndex );
  if ( nIndex < 0 ||
       nRawIndex >= eshSHvectorIdxFirstObjId +
       lpSHvector [ eshSHvectorIdxObjIds ] ) {
    ERROR (( szCantIndex, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ), nIndex ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  nLockOld	= fnLockInsert ( oHeap, eshLockElementWrite,
				 oObjId, nIndex, (PHPEEK) NULL );
  if ( (int) nLockOld >= 0 ) {
    /* 1998/10/21 HK: Debug: */
    nValueOld	= lpSHvector [ nRawIndex ];
    nValueNew	= fnImmediate2ObjId ( nValue, &nTypeTagValueNew );
    if ( boundp ( nValueNew ) && ! immediatep ( nValueNew ) &&
	 ! ObjId_is_valid ( nValueNew ) ) {
      char	szElement [ 256 ], szObject [ 256 ], szElementOld [ 256 ];
      char	szTypeTag [ 256 ], szTypeTagNew [ 256 ];
      ERROR (( szWriteInvObjId, nValueNew,
	       PrintObject ( nValueNew, szElement ),
	       nValue,
	       nTypeTagValue,
	       PrintObject ( TypeTag2ObjId ( nTypeTagValue ), szTypeTag ),
	       nTypeTagValueNew,
	       PrintObject ( TypeTag2ObjId ( nTypeTagValueNew ),
			     szTypeTagNew ),
	       LONG2SHORTOBJID ( oGlobalMinObjId ),
	       LONG2SHORTOBJID ( oGlobalMaxObjId ),
	       PrintObject ( oObjId, szObject ),
	       nIndex,
	       PrintObject ( nValueOld, szElementOld ) ));
      UnstoreSession ();
      RETURN ( eshGeneralError );
    }
    fnServerPlobdReply ( (void(*)()) fnServerObjectWriteAtIndex, &nLockOld );
    if ( nValueOld != nValueNew && gfnObjectStateChanged ( oHeap, oObjId ) ) {
	lpSHvector [ nRawIndex ]	= (OBJID) nValueNew;
	nGlobalTouched++;
    }
  }
  
  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectWriteAtIndex );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerObjectWriteAtIndices, "c-sh-write-indices",
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
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( int, nObjIds ),
			     vector_in, pObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_in, pnTypeTags ) ) )
{
  OBJID		oObjId, oHeap;
  LPOBJID	lpSHvector;
  SHLOCK	nLockOld;
  int		nSlots, nSlotsClipped, s, nTotalWritten = 0;
  OBJID		oObjIdOld, oObjIdNew;
  BOOL		bObjectStateChanged = FALSE;
  SHTYPETAG	eTypeTagNew;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );
  if ( nExpectingTypeTag != NULLTYPETAG &&
       nExpectingTypeTag !=
       ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    UNEXPECTED_TYPE_TAG ( oObjId, -1, nExpectingTypeTag );
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  nSlots	= lpSHvector [ eshSHvectorIdxObjIds ] -
    ( eshSHvectorIdxFirstData - eshSHvectorIdxFirstObjId );
  nSlotsClipped	= MIN ( nSlots - nIndex, nObjIds );
  if ( nSlotsClipped > 0 ) {
    if ( nIndex < 0 || nIndex >= nSlots ) {
      ERROR (( szCantIndex, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ),
	       nIndex ));
      UnstoreSession ();
      RETURN ( eshGeneralError );
    }
    nTotalWritten	= nSlotsClipped;
    oHeap	= Short2LongObjId ( oShortObjIdHeap );
    nLockOld	= fnLockInsert ( oHeap, eshLockVectorWrite, oObjId, -1,
				 (PHPEEK) NULL );
    if ( (int) nLockOld < 0 ) {
      UnstoreSession ();
      RETURN ( nLockOld );
    }
    /* Scan through the buffer and look for invalid objids passed: */
    for ( s = 0; s < nSlotsClipped; s++ ) {
      eTypeTagNew	= pnTypeTags [ s ];
      if ( eTypeTagNew != eshIgnoreSlotTag ) {
	oObjIdNew	= fnImmediate2ObjId ( pObjIds [ s ], &eTypeTagNew );
	if ( boundp ( oObjIdNew ) && ! immediatep ( oObjIdNew ) &&
	     ! ObjId_is_valid ( oObjIdNew ) ) {
	  char	szElement [ 256 ], szObject [ 256 ], szElementOld [ 256 ];
	  char	szTypeTag [ 256 ], szTypeTagNew [ 256 ];
	  oObjIdOld	= lpSHvector [ eshSHvectorIdxFirstData + nIndex + s ];
	  ERROR (( szWriteInvObjId, oObjIdNew,
		   PrintObject ( oObjIdNew, szElement ),
		   pnTypeTags [ s ],
		   PrintObject ( TypeTag2ObjId ( pnTypeTags [ s ] ),
				 szTypeTag ),
		   eTypeTagNew,
		   PrintObject ( TypeTag2ObjId ( eTypeTagNew ), szTypeTagNew ),
		   LONG2SHORTOBJID ( oGlobalMinObjId ),
		   LONG2SHORTOBJID ( oGlobalMaxObjId ),
		   PrintObject ( oObjId, szObject ),
		   nIndex + s,
		   PrintObject ( oObjIdOld, szElementOld ) ));
	  UnstoreSession ();
	  RETURN ( eshGeneralError );
	}
      }
    }
    fnServerPlobdReply ( (void(*)()) fnServerObjectWriteAtIndices,
			 &nTotalWritten );
    for ( s = 0; s < nSlotsClipped; s++ ) {
      eTypeTagNew	= pnTypeTags [ s ];
      if ( eTypeTagNew != eshIgnoreSlotTag ) {
	oObjIdOld	= lpSHvector [ eshSHvectorIdxFirstData + nIndex + s ];
	oObjIdNew	= fnImmediate2ObjId ( pObjIds [ s ], &eTypeTagNew );
	if ( oObjIdOld != oObjIdNew ) {
	  if ( ! bObjectStateChanged ) {
	    bObjectStateChanged	= TRUE;
	    if ( ! gfnObjectStateChanged ( oHeap, oObjId ) ) {
	      nTotalWritten	= s;
	      break;
	    }
	  }
	  lpSHvector [ eshSHvectorIdxFirstData + nIndex + s ]	= oObjIdNew;
	}
      }
    }
  }

  UnstoreSession ();
  RETURN ( nTotalWritten );
} EndFunction ( fnServerObjectWriteAtIndices );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnServerObjectWriteObjId, "c-sh-write-objid",
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
		  argument ( SHORTOBJID, value_in, oShortObjIdWrite ) ) )
{
  SHLOCK	nLockOld;

  INITIALIZEPLOB;
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
				 nIndex, oShortObjIdWrite, eshShortObjIdTag );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerObjectWriteObjId );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerObjectWriteValues, "c-sh-write-values",
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
		  argument ( SHTYPETAG, value_in, nElementTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInElements )
		  and
		  argument ( VECTOR ( void,
				      fnTypeTagSizeValue(1,&nElementTypeTag,
							 &nSizeInElements ) ),
			     vector_in,
			     pBuffer ) ) )
{
  LPCLASSINFO	pClassInfo;
  int		nAlignment, nBitsPerElement;
  OBJID		oObjId, oHeap;
  LPOBJID	lpSHvector;
  SHLOCK	nLockOld;
  int		nSlots, nValues, nTotalWritten = 0;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId		= Short2LongObjId ( oShortObjId );

  pClassInfo		= (LPCLASSINFO) FindClassInfo ( nElementTypeTag );
  ASSERT ( pClassInfo != NULL );
  nBitsPerElement	= pClassInfo->nFixSizeValue;
  ASSERT ( nBitsPerElement > 0 );

  nAlignment		=
    ( nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerElement;
  if ( nAlignment > 0 && nIndex % nAlignment != 0 ) {
    char	szObject [ 256 ];
    ERROR (( szInvalidAlignment,
	     nIndex, PrintObject ( oObjId, szObject ), nAlignment ));
    RETURN ( eshGeneralError );
  }

  ASSERT ( pBuffer != NULL );
  lpSHvector		= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );
  if ( nExpectingTypeTag != NULLTYPETAG &&
       nExpectingTypeTag !=
       ObjId2TypeTag ( lpSHvector [ eshSHvectorIdxTypeTag ] ) ) {
    UNEXPECTED_TYPE_TAG ( oObjId, -1, nExpectingTypeTag );
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  nSlots	= lpSHvector [ eshSHvectorIdxObjIds ];
  nValues	= ( ( lpSHvector [ eshSHvectorIdxSize ] - nSlots -
		      eshSHvectorIdxFirstObjId ) *
		    nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerElement;
  nValues	= MIN ( nValues - nIndex, nSizeInElements );
  if ( nValues > 0 ) {
    if ( nIndex < 0 || nIndex >= nValues ) {
      ERROR (( szCantIndex, fnPrintObject ( oObjId, (LPSTR) NULL, 0 ),
	       nIndex ));
      UnstoreSession ();
      RETURN ( eshGeneralError );
    }
    nTotalWritten	= nValues;
    oHeap	= Short2LongObjId ( oShortObjIdHeap );
    nLockOld	= fnLockInsert ( oHeap, eshLockVectorWrite, oObjId, -1,
				 (PHPEEK) NULL );
    if ( (int) nLockOld < 0 ) {
      UnstoreSession ();
      RETURN ( nLockOld );
    }
    fnServerPlobdReply ( (void(*)()) fnServerObjectWriteValues,
			 &nTotalWritten );
    if ( memcmp ( & lpSHvector [ nSlots + eshSHvectorIdxFirstObjId +
			         AlignBitsToWords ( nIndex *
						    nBitsPerElement ) ],
		  pBuffer,
		  AlignBitsToWords ( nValues * nBitsPerElement ) *
		  nSizeOfPostoreWord ) != 0 &&
	 gfnObjectStateChanged ( oHeap, oObjId ) ) {
      memcpy ( & lpSHvector [ nSlots + eshSHvectorIdxFirstObjId +
			      AlignBitsToWords ( nIndex * nBitsPerElement ) ],
	       pBuffer,
	       AlignBitsToWords ( nValues * nBitsPerElement ) *
	       nSizeOfPostoreWord );
      nGlobalTouched++;
    }
  }

  UnstoreSession ();
  RETURN ( nTotalWritten );
} EndFunction ( fnServerObjectWriteValues );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
