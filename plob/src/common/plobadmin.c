/* -------------------------------------------------------------------------
| Module	plobadmin.c
| Author	Heiko Kirschke
| Date		1998/02/04
| Description	PLOB administration functions.
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
#if !WIN32
#include	<unistd.h>
#include	<sys/resource.h>
#include	<dirent.h>
#endif
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>
#include	<time.h>
#if WIN32
#include	<direct.h>
#include	<process.h>
#else
#include	<sys/time.h>
#include	<sys/utsname.h>
#endif
#include	<sys/types.h>
#include	<rpc/rpc.h>
#include	<rpc/svc.h>
#include	<rpc/auth.h>
#if WIN32
#include	<rpc/auth_uni.h>
#else
#if LINUX
#include	<rpc/auth_unix.h>
#elif !HPUX
#include	<rpc/auth_sys.h>
#endif /* LINUX */
#include	<rpc/auth_des.h>
#endif /* WIN32 */
#include	<signal.h>

#if !LINUX&&!WIN32
#include	<ucontext.h>
#endif

#if LINUX
#include	<asm/sigcontext.h>
#elif !WIN32
#include	<siginfo.h>
#endif

#if !WIN32
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>
#include	<netdb.h>
#endif

#if !LINUX&&!WIN32&&!HPUX
#include	<netdir.h>
#include	<netconfig.h>
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
#include	"plobclos.h"
#include	"ploblock.h"
#include	"plobheap.h"
#include	"plobbtree.h"
#include	"plobroot.h"
#include	"plobadmin.h"

#define		RPCNOTYPES
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Types
 ------------------------------------------------------------------------- */
typedef struct {
  int	nMatchPort;
  char 	szMatchPort [ MAX_FNAME ];
  int	nUnassigned;
  char	szAssigned [ 256 ];
}	SEARCHENTRY, * PSEARCHENTRY;
/* ----------------------------------------------------------------------- */
typedef BOOL	( * PFNSCANENTRY )	( LPVOID	pClosure,
					  LPCSTR	pszDirectory );

/* ----------------------------------------------------------------------- */
DLLEXPORTVAR const char	szLocalName []		= "local";
DLLEXPORTVAR const char	szStablestore []	= "stablestore";
DLLEXPORTVAR const char	szLocalhost	[]	= "localhost";
DLLEXPORTVAR const char	szTcp []		= "tcp";

DLLEXPORTVAR const char	szPortFilename []	= "port.inf";
DLLEXPORTVAR const char	szPidFilename []	= "pid.inf";

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitCommonAdminModule	( void )
{
  PROCEDURE	( fnInitCommonAdminModule );

  RETURN ( VOID );
} /* fnInitCommonAdminModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitCommonAdminModule	( void )
{
  PROCEDURE	( fnDeinitCommonAdminModule );

  RETURN ( VOID );
} /* fnDeinitCommonAdminModule */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		fnDatabaseP		( LPCSTR	pszDirectory )
{
  BOOL	bDatabaseP	= FALSE;
  FILE	* pFile		= (FILE *) NULL;
  char	szPath [ MAX_FNAME ];

  PROCEDURE ( fnDatabaseP );
  INITIALIZEPLOB;

  sprintf ( szPath, "%s/%s", pszDirectory, szStablestore );
  pFile		= fopen ( szPath, szStreamRead );
  bDatabaseP	= (BOOL) ( pFile != NULL );
  if ( bDatabaseP ) {
    fclose ( pFile );
    pFile	= (FILE *) NULL;
  }

  RETURN ( bDatabaseP );
} /* fnDatabaseP */

/* ----------------------------------------------------------------------- */
static LPCSTR	fnMakeSentinel	( LPSTR		pszSentinel,
				  size_t	nSentinel,
				  LPCSTR	pszDirectory )
{
  char	szSentinel [ MAX_FNAME ];

  PROCEDURE	( fnMakeSentinel );

  sprintf ( szSentinel, "//%s/%s", fnGetHost (), pszDirectory );
  strncpy ( pszSentinel, szSentinel, nSentinel );

  RETURN ( pszSentinel );
} /* fnMakeSentinel */

/* ----------------------------------------------------------------------- */
static void	fnRemoveInfo	( LPCSTR	pszDirectory,
				  LPCSTR	pszFilename )
{
  char	szInfoPath [ MAX_FNAME ];

  PROCEDURE	( fnRemoveInfo );

  sprintf ( szInfoPath, "%s/%s", pszDirectory, pszFilename );
  remove ( szInfoPath );

  RETURN ( VOID );
} /* fnRemoveInfo */

/* ----------------------------------------------------------------------- */
static int	fnReadInfo	( LPCSTR	pszDirectory,
				  LPCSTR	pszFilename )
{
  int	nInfo	= -1;
  FILE	* pFileInfo	= (FILE *) NULL;
  char	szInfoPath [ MAX_FNAME ], szLine [ 128 ], szToken [ 128 ];
  char	szSentinel [ MAX_FNAME ];

  PROCEDURE	( fnReadInfo );

  sprintf ( szInfoPath, "%s/%s", pszDirectory, pszFilename );
  pFileInfo	= fopen ( szInfoPath, szStreamRead );
  if ( pFileInfo != NULL ) {
    fnMakeSentinel ( szSentinel, sizeof ( szSentinel ), pszDirectory );
    while ( pFileInfo != NULL && nInfo < 0 && ! feof ( pFileInfo ) ) {
      szLine [ 0 ]	= '\0';
      fgets ( szLine, sizeof ( szLine ), pFileInfo );
      szToken [ 0 ]	= '\0';
      nInfo		= -1;
      if ( sscanf ( szLine, "%s%d", szToken, &nInfo ) < 2 ||
	   szToken [ 0 ] == '#' ) {
	nInfo	= -1;
      } else if ( strncmp ( szSentinel, szToken,
			    sizeof ( szSentinel ) ) != 0 ) {
	LOG (( "Removed malformed %s file.", szInfoPath ));
	nInfo		= -1;
	fclose ( pFileInfo );
	pFileInfo	= (FILE *) NULL;
	fnRemoveInfo ( pszDirectory, pszFilename );
	break;
      }
    }
    if ( pFileInfo != NULL ) {
      fclose ( pFileInfo );
      pFileInfo	= (FILE *) NULL;
    }
  }

  RETURN ( nInfo );
} /* fnReadInfo */

/* ----------------------------------------------------------------------- */
static int	fnWriteInfo	( const char *	pszDirectory,
				  const char *	pszFilename,
				  int		nInfo )
{
  FILE		* pFileInfo	= (FILE *) NULL;
  char		szInfoPath [ MAX_FNAME ], szLine [ 128 ], szToken [ 128 ];
  char		szTime [ 80 ], szSentinel [ MAX_FNAME ];
  time_t	Time;

  PROCEDURE	( fnWriteInfo );

  sprintf ( szInfoPath, "%s/%s", pszDirectory, pszFilename );
  pFileInfo	= fopen ( szInfoPath, szStreamWrite );
  if ( pFileInfo != NULL ) {
    fnMakeSentinel ( szSentinel, sizeof ( szSentinel ), pszDirectory );
    time ( &Time );
    strftime ( szTime, sizeof ( szTime ), szTimeFormat, localtime ( &Time ) );
    fprintf ( pFileInfo,
	      "# File `%s' generated at %s by %s\n"
	      "# for host `%s' directory `%s'. Do not remove this file.\n"
	      "# Do not copy it into another PLOB database directory.\n"
	      "%s %d\n",
	      pszFilename, szTime, fnGetUser(),  fnGetHost (),
	      pszDirectory, szSentinel, nInfo );
    fflush ( pFileInfo );
    fclose ( pFileInfo );
    pFileInfo	= (FILE *) NULL;
  } else {
    nInfo	= -1;
  }

  RETURN ( nInfo );
} /* fnWriteInfo */

/* ----------------------------------------------------------------------- */
static int	fnScanDirectories	( LPVOID	pClosure,
					  PFNSCANENTRY	pfnScanEntry )
{
  int			nScanned	= 0;

  PROCEDURE	( fnScanDirectories );

#if WIN32
  {
    WIN32_FIND_DATA	FindFileData;
    HANDLE		hDirectory	=
      FindFirstFileEx ( "*", FindExInfoStandard, &FindFileData,
			FindExSearchLimitToDirectories,
			NULL, 0 );
    BOOL		bFound	= ( hDirectory != INVALID_HANDLE_VALUE );
    if ( bFound ) {
      for ( bFound = TRUE; bFound;
	    bFound = FindNextFile ( hDirectory, &FindFileData ) ) {
	if ( ( FindFileData.dwFileAttributes &
	       FILE_ATTRIBUTE_DIRECTORY ) != 0 &&
	     fnDatabaseP ( FindFileData.cFileName ) ) {
 	  if ( ! ( *pfnScanEntry ) ( pClosure, FindFileData.cFileName ) ) {
	    break;
	  }
	  nScanned++;
	}
      }
      FindClose ( hDirectory );
    }
  }
#else
  {
    DIR			* pDirectory;
    struct dirent	* pEntry;

    pDirectory	= opendir ( "." );
    if ( pDirectory != NULL ) {
      for ( pEntry = readdir ( pDirectory );
	    pDirectory != NULL && pEntry != NULL;
	    pEntry = readdir ( pDirectory ) ) {
	if ( fnDatabaseP ( pEntry->d_name ) ) {
 	  if ( ! ( *pfnScanEntry ) ( pClosure, pEntry->d_name ) ) {
	    break;
	  }
	  nScanned++;
	}
      }
      if ( pDirectory != NULL ) {
	closedir ( pDirectory );
	pDirectory	= (DIR *) NULL;
      }
    }
  }
#endif

  RETURN ( nScanned );
} /* fnScanDirectories */

/* ----------------------------------------------------------------------- */
static void	fnInitSearchEntry	( PSEARCHENTRY	pEntry,
					  int		nRpcPort )
{
  PROCEDURE	( fnInitSearchEntry );

  memset ( pEntry, 0, sizeof ( *pEntry ) );
  pEntry->nMatchPort		= nRpcPort;
  pEntry->szAssigned [ 0 ]	= '\0';
  RETURN ( VOID );
} /* fnInitSearchEntry */

/* ----------------------------------------------------------------------- */
static BOOL	fnMatchPort	( LPVOID	pClosure,
				  LPCSTR	pszDirectory )
{
  BOOL		bContinue	= TRUE;
  PSEARCHENTRY	pEntry		= (PSEARCHENTRY) pClosure;
  int		nRpcPort;
  char		szPortPath [ MAX_FNAME ];

  PROCEDURE ( fnMatchPort );

  nRpcPort	= fnReadInfo ( pszDirectory, szPortFilename );
  if ( nRpcPort < 0 ) {
    pEntry->nUnassigned++;
  } else {
    pEntry->szAssigned [ nRpcPort ]	= '1';
    if ( nRpcPort == pEntry->nMatchPort ) {
      if ( pEntry->szMatchPort [ 0 ] == '\0' ) {
	strncpy ( pEntry->szMatchPort, pszDirectory,
		  sizeof ( pEntry->szMatchPort ) );
      } else {
	sprintf ( szPortPath, "%s/%s", pEntry->szMatchPort,
		  szPortFilename );
	remove ( szPortPath );
	sprintf ( szPortPath, "%s/%s", pszDirectory, szPortFilename );
	remove ( szPortPath );
	LOG (( "Detected directories %s"
	       "       and %s having both port %d.",
	       pEntry->szMatchPort, pszDirectory, nRpcPort ));
      }
    }
  }

  RETURN ( bContinue );
} /* fnMatchPort */

/* ----------------------------------------------------------------------- */
static BOOL	fnAssignDirectory	( LPVOID	pClosure,
					  LPCSTR	pszDirectory )
{
  PSEARCHENTRY	pEntry	= (PSEARCHENTRY) pClosure;
  int		nRpcPort, l;

  nRpcPort	= fnReadInfo ( pszDirectory, szPortFilename );
  if ( nRpcPort < 0 ) {
    l				= strlen ( pEntry->szAssigned );
    /* This assert should only fail if there are more than 255
       database directories: */
    ASSERT ( l < sizeof ( pEntry->szAssigned ) );
    pEntry->szAssigned [ l ]	= '1';
    fnWriteInfo ( pszDirectory, szPortFilename, l );
  }

  RETURN ( TRUE );
} /* fnAssignDirectory */

/* ----------------------------------------------------------------------- */
int			fnStorePid		( LPCSTR	pszDirectory )
{
  int	nPID	= 0;

  PROCEDURE	( fnStorePid );
  INITIALIZEPLOB;

  nPID	= fnServerGetPID ();
  fnWriteInfo ( pszDirectory, szPidFilename, nPID );

  RETURN ( nPID );
} /* fnStorePid */

/* ----------------------------------------------------------------------- */
void			fnUnstorePid		( LPCSTR	pszDirectory )
{
  PROCEDURE	( fnUnstorePid );
  INITIALIZEPLOB;

  fnRemoveInfo ( pszDirectory, szPidFilename );

  RETURN ( VOID );
} /* fnStorePid */

/* ----------------------------------------------------------------------- */
LPSTR			fnGetDirectoryByPort	( LPSTR		pszDirectory,
						  size_t	nDirectory,
						  int		nRpcPort )
{
  BOOL		bFound;
  SEARCHENTRY	SearchEntry;

  PROCEDURE	( fnGetDirectoryByPort );
  INITIALIZEPLOB;

  fnInitSearchEntry ( &SearchEntry, nRpcPort );
  fnScanDirectories ( &SearchEntry, fnMatchPort );

  bFound	=  (BOOL) ( SearchEntry.szMatchPort [ 0 ] != '\0' );
  if ( ! bFound && SearchEntry.nUnassigned > 0 ) {
    /* Assign directories. */
    fnScanDirectories ( &SearchEntry, fnAssignDirectory );
    /* Do a 2nd search. */
    fnInitSearchEntry ( &SearchEntry, nRpcPort );
    fnScanDirectories ( &SearchEntry, fnMatchPort );
    bFound	=  (BOOL) ( SearchEntry.szMatchPort [ 0 ] != '\0' );
  }

  if ( bFound && pszDirectory != NULL ) {
    strncpy ( pszDirectory, SearchEntry.szMatchPort, nDirectory );
  }

  RETURN ( ( bFound ) ? pszDirectory : (LPSTR) NULL );
}/* fnGetDirectoryByPort */

/* ----------------------------------------------------------------------- */
static int	fnGetPidByClientPort	( PCLIENT	*ppClient,
					  LPCSTR	pszHost,
					  LPCSTR	pszTransport,
					  int		nRpcPort,
					  int	nTimeout /* seconds */ )
{
  int			nPID	= -1;
  fnServerGetPID_rets	clnt_res;
  struct timeval	Timeout;

  PROCEDURE	( fnGetPidByClientPort );
  INITIALIZEPLOB;

  memset ( &Timeout, 0, sizeof ( Timeout ) );
  Timeout.tv_sec	= nTimeout;

  if ( *ppClient == NULL ) {
    *ppClient =
      clnt_create ( (char *) pszHost,
		    fnPlobdGetPortOffset () + nRpcPort,
		    fnPlobdGetVersion (), (char *) pszTransport );
  }
  if ( *ppClient != NULL ) {
    if ( clnt_call ( *ppClient, NULLPROC,
		     (xdrproc_t) xdr_void, (caddr_t) NULL,
		     (xdrproc_t) xdr_void, (caddr_t) NULL,
		     Timeout ) == RPC_SUCCESS ) {
      memset ( &clnt_res, 0, sizeof ( clnt_res ) );
      if ( clnt_call ( *ppClient, fnRpc_fnServerGetPID, xdr_void,
		       (caddr_t) NULL, (xdrproc_t) xdr_fnServerGetPID_rets,
		       (caddr_t) &clnt_res, Timeout ) == RPC_SUCCESS ) {
	nPID	= clnt_res.ReturnValue;
	xdr_free ( (xdrproc_t) xdr_fnServerGetPID_rets, (char *) &clnt_res );
      }
    }
    if ( nPID > 0 ) {
      auth_destroy ( (*ppClient)->cl_auth );
      clnt_destroy ( *ppClient );
      *ppClient	= (PCLIENT) NULL;
    }
  }

  RETURN ( nPID );
} /* fnGetPidByClientPort */

/* ----------------------------------------------------------------------- */
int			fnGetPortByDirectory	( LPCSTR	pszDirectory )
{
  int		nRpcPort	= -1;
  SEARCHENTRY	SearchEntry;

  PROCEDURE	( fnGetPortByDirectory );
  INITIALIZEPLOB;

  nRpcPort	= fnReadInfo ( pszDirectory, szPortFilename );
  if ( nRpcPort < 0 && fnDatabaseP ( pszDirectory ) ) {
    fnInitSearchEntry ( &SearchEntry, nRpcPort );
    fnScanDirectories ( &SearchEntry, fnMatchPort );
    /* Assign directories. */
    fnScanDirectories ( &SearchEntry, fnAssignDirectory );
    /* Do a 2nd search. */
    nRpcPort	= fnReadInfo ( pszDirectory, szPortFilename );
  }

  RETURN ( nRpcPort );
} /* fnGetPortByDirectory */

/* ----------------------------------------------------------------------- */
int			fnGetPidByPort		( LPCSTR	pszHost,
						  LPCSTR	pszTransport,
						  int		nRpcPort,
						  int	nTimeout /* sec */ )
{
  int		nPID	= -1;
  PCLIENT	pClient	= (PCLIENT) NULL;

  PROCEDURE	( fnGetPidByPort );
  INITIALIZEPLOB;

  nPID	= fnGetPidByClientPort ( &pClient, pszHost, pszTransport,
				 nRpcPort, nTimeout );
  if ( pClient != NULL ) {
    auth_destroy ( pClient->cl_auth );
    clnt_destroy ( pClient );
    pClient	= (PCLIENT) NULL;
  }
    
  RETURN ( nPID );
} /* fnGetPidByPort */

/* ----------------------------------------------------------------------- */
int DLLEXPORT		fnStartLocalServer	( LPCSTR	pszDirectory,
						  GETACTION	eAction,
						  int	nRpcPortServer )
{
#if ! WIN32
  static const char	szFormatCommand []	=
    "( %s %s %s </dev/null >/dev/null & )";
#endif
  static const char	szErrShellStartup []		=
    "Could not start new daemon process for database directory `%s'\n"
    "       Command line: %s, return code %d\n";
  static const char	szErrCheckStartup []		=
    "Starting new daemon process for database directory `%s',\n"
    "       rpc port %d failed.\n";

  int	nRpcPortPassive = -1, nRpcPortActive = -1, nRpcPort = -1;
  int	nPID = -1, nReturnCode = 0, i;
  char	szCommand [ 256 ];

  PROCEDURE	( fnStartLocalServer );

  nRpcPortPassive	= fnGetPortByDirectory ( pszDirectory );
  if ( nRpcPortPassive < 0 &&
       ( (unsigned int) eAction &
	 ( (unsigned int) eCreateDatabase | (unsigned int) eStartServer ) ) !=
       ( (unsigned int) eCreateDatabase | (unsigned int) eStartServer ) ) {
    /* Directory does not exist; ask if database should be created: */
    CERROR (( "Try to create it.",
	      "Could not open database directory %s", pszDirectory ));
    eAction	= (GETACTION)
      ( (unsigned int) eAction |
	(unsigned int) eCreateDatabase | (unsigned int) eStartServer );
  }

  /* Now look if there is a daemon running on szDirectory.  If I am
     not the daemon ... */
  if ( ( (unsigned int) eAction &
	 ( (unsigned int) eGetPortActive |
	   (unsigned int) eStartServer ) ) != 0 ) {
    if ( nRpcPortPassive >= 0 ) {
      /* ... check if there's another one running on this machine: */
      nPID	= ( nRpcPortPassive == nRpcPortServer ) ?
	getpid () :
	fnGetPidByPort ( szLocalhost, szTcp, nRpcPortPassive, 2 );
    }
    if ( nPID > 0 ) {
      /* Found a server: */
      nRpcPortActive	= nRpcPortPassive;
    } else {
      nRpcPortActive	= -1;
      if ( ( (unsigned int) eAction & (unsigned int) eStartServer ) != 0 ) {
	/* Daemon is not running for szDirectory, so spawn one now. */
	fnUnstorePid ( pszDirectory );
#if WIN32
	if ( spawnl ( _P_DETACH, szPlobd, szPlobd, szOptionDirectory,
		      pszDirectory, NULL ) == -1 ) {
	  sprintf ( szCommand, "%s %s %s",
		    szPlobd, szOptionDirectory, pszDirectory );
	  nReturnCode	= 1;
	}
#else
	sprintf ( szCommand, szFormatCommand, szPlobd,
		  szOptionDirectory, pszDirectory );
	nReturnCode	= system ( szCommand );
#endif
	if ( nReturnCode != 0 ) {
	  /* There was an error at daemon startup: */
	  ERROR (( szErrShellStartup, pszDirectory,
		   szCommand, nReturnCode ));
	} else {
	  if ( nRpcPortPassive < 0 ) {
	    /* Look for the rpc port id of the created database: */
	    for ( i = 0; i < 10 && nRpcPortPassive < 0;
		  sleep ( ( i < 5 ) ? 2 : 4 ), i++ ) {
	      nRpcPortPassive	= fnReadInfo ( pszDirectory, szPortFilename );
	    }
	  }
	  if ( nRpcPortPassive >= 0 ) {
	    /* Look if the startup was successfull: */
	    PCLIENT	pClient	= (PCLIENT) NULL;
	    for ( i = 0, nPID = -1; i < 10 && nPID <= 0;
		  sleep ( ( i < 5 ) ? 2 : 4 ), i++ ) {
	      nPID	=
		fnGetPidByClientPort ( &pClient, szLocalhost,
				       szTcp, nRpcPortPassive, 2 );
	    }
	    if ( pClient != NULL ) {
	      auth_destroy ( pClient->cl_auth );
	      clnt_destroy ( pClient );
	      pClient	= (PCLIENT) NULL;
	    }
	    if ( nPID <= 0 ) {
	      ERROR (( szErrCheckStartup, pszDirectory, nRpcPortPassive ));
	    } else {
	      nRpcPortActive	= nRpcPortPassive;
	    }
	  } else {
	    ERROR (( "Could not create database directory `%s'",
		     pszDirectory ));
	  }
	}
      }
    }
  }

  nRpcPort	=  ( ( (unsigned int) eAction &
		       ( (unsigned int) eGetPortPassive |
			 (unsigned int) eGetPortActive ) ) ==
		     eGetPortActive ) ?
    nRpcPortActive : nRpcPortPassive;

  RETURN ( nRpcPort );
} /* fnStartLocalServer */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
