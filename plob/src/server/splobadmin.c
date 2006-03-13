/* -------------------------------------------------------------------------
| Module	splobadmin.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Copyright	(C) 1998 Heiko Kirschke
| Date		1998/02/04
| Description	PLOB administration functions.
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
#include	"splobstruct.h"
#include	"splobclos.h"
#include	"sploblock.h"
#include	"splobheap.h"
#include	"splobbtree.h"
#include	"splobregex.h"
#include	"splobroot.h"
#include	"splobadmin.h"

#define		RPCNOTYPES
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */
#if defined(SIGHUP)&&defined(SIGUSR1)
enum {
  nSignalRestart	= SIGHUP,
  nSignalReset		= SIGUSR1
};
#define	HANDLE_SIGNALS 1
#else
#define	HANDLE_SIGNALS 0
#endif

/* -------------------------------------------------------------------------
| Error message formats
 ------------------------------------------------------------------------- */
static const char	szFormatRestart []	=
"PLOB daemon %s on %s. %d active session(s).";

static const char	szUnknownUser []	=
UNREADABLE_OBJECT_PREFIX "unknown user" UNREADABLE_OBJECT_SUFFIX;

/* -------------------------------------------------------------------------
| Signal handling
 ------------------------------------------------------------------------- */
#if HANDLE_SIGNALS
static struct sigaction	SigActionHup, SigActionUsr1, SigActionTerm;
#endif /* HANDLE_SIGNALS */

/* ----------------------------------------------------------------------- */
static void	fnRestart	( int	nExitCode )
{
  RPCPORT	nRpcPort;
  char		szPort [ 16 ];

  PROCEDURE	( fnRestart );

  /* Restart the daemon: */
  nRpcPort	= fnPlobdGetPort ();
  if ( nRpcPort >= nMasterPort ) {
    if ( szGlobalDirectory [ 0 ] != '\0' ) {
      fnUnstorePid ( szGlobalDirectory );
    }
    svc_unregister ( fnPlobdGetPortOffset () + nRpcPort,
		     fnPlobdGetVersion () );
    if ( nRpcPort > nMasterPort ) {
      sprintf ( szPort, "%d", nRpcPort );
#if WIN32
      if ( spawnlp ( _P_DETACH, szPlobd, szPlobd,
		     szOptionPort, szPort,
		     szOptionRestarted, NULL ) != -1 ) {
	exit ( 0 );
      }
#else
      execlp ( szPlobd, szPlobd, szOptionPort, szPort,
	       szOptionRestarted, NULL );
#endif
    } else {
#if WIN32
      if ( spawnlp ( _P_DETACH, szPlobd, szPlobd, szOptionRestarted,
		     NULL ) != -1 ) {
	exit ( 0 );
      }
#else
      execlp ( szPlobd, szPlobd, szOptionRestarted, NULL );
#endif
    }
    LOG (( "Restarting %s failed, exiting.", szPlobd ));
    exit ( 1 );
  }
  RETURN ( VOID );
} /* fnRestart */

/* ----------------------------------------------------------------------- */
static void	fnReset		( int	nExitCode )
{
  PROCEDURE	( fnReset );

  /* Reset the daemon: */
  if ( StableHeap_is_open ) {
    fnAdminSet ( unbound, unbound );
    LOG (( "Resetted daemon on SIGUSR1" ));
  }

  RETURN ( VOID );
} /* fnReset */

#if HANDLE_SIGNALS

/* ----------------------------------------------------------------------- */
static void	fnSigHup	( int		sig,
#if LINUX
				  struct sigcontext_struct	sc,
#else
				  siginfo_t	* sip,
#endif
				  void		* uap )
{
  int	nSessions	= 0;

  PROCEDURE	( fnSigHup );

  if ( StableHeap_is_open ) {
    nSessions	= gfnCount ( Sessions () );
  }
  LOG (( szFormatRestart, "restart", "signal", nSessions ));
  fnRestart ( nSignalRestart );

  RETURN ( VOID );
} /* fnSigHup */

/* ----------------------------------------------------------------------- */
static void	fnSigUsr1	( int		sig,
#if LINUX
				  struct sigcontext_struct	sc,
#else
				  siginfo_t	* sip,
#endif
				  void		* uap )
{
  PROCEDURE	( fnSigUsr1 );
  fnReset ( nSignalReset );
  RETURN ( VOID );
} /* fnSigUsr1 */

#endif /* HANDLE_SIGNALS */

/* ----------------------------------------------------------------------- */
static void	fnSigTerm	( int		sig
#if !WIN32
				  ,
#if LINUX
				  struct sigcontext_struct	sc,
#else
				  siginfo_t	* sip,
#endif
				  void		* uap
#endif /* !WIN32 */
				  )
{
  PROCEDURE	( fnSigTerm );

  if ( szGlobalDirectory [ 0 ] != '\0' ) {
    fnUnstorePid ( szGlobalDirectory );
  }
  exit ( sig );

  RETURN ( VOID );
} /* fnSigTerm */

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeAdminModule		( void )
{
#if HANDLE_SIGNALS
  struct sigaction	SigAction;
#endif /* HANDLE_SIGNALS */

  PROCEDURE	( fnInitializeAdminModule );

  /* The restart on nSignalRestart will only work when the PLOB daemon
     has been initialized, i.e. when at least one client request has
     dropped in: */

#if HANDLE_SIGNALS
  memset ( &SigAction, 0, sizeof ( SigAction ) );
#if LINUX
  SigAction.sa_handler		= fnSigHup;
  SigAction.sa_flags		= SA_RESTART;
#else
  SigAction.sa_sigaction	= fnSigHup;
  SigAction.sa_flags		= SA_SIGINFO;
#endif
  if ( sigaction ( nSignalRestart, &SigAction, &SigActionHup ) < 0 ) {
    int	nErrNo	= errno;
    LOG (( "Installing signal restart handler failed with errno %d.",
	   nErrNo ));
  }
  memset ( &SigAction, 0, sizeof ( SigAction ) );
#if LINUX
  SigAction.sa_handler		= fnSigUsr1;
  SigAction.sa_flags		= SA_RESTART;
#else
  SigAction.sa_sigaction	= fnSigUsr1;
  SigAction.sa_flags		= SA_SIGINFO;
#endif
  if ( sigaction ( nSignalReset, &SigAction, &SigActionUsr1 ) < 0 ) {
    int	nErrNo	= errno;
    LOG (( "Installing signal reset handler failed with errno %d.",
	   nErrNo ));
  }
#endif /* HANDLE_SIGNALS */
#if WIN32
  if ( signal ( SIGTERM, fnSigTerm ) == SIG_ERR ) {
    int	nErrNo	= errno;
    LOG (( "Installing SIGTERM handler failed with errno %d.",
	   nErrNo ));
  }
#else
  memset ( &SigAction, 0, sizeof ( SigAction ) );
#if LINUX
  SigAction.sa_handler		= fnSigTerm;
  SigAction.sa_flags		= SA_RESTART;
#else
  SigAction.sa_sigaction	= fnSigTerm;
  SigAction.sa_flags		= SA_SIGINFO;
#endif /* LINUX */
  if ( sigaction ( SIGTERM, &SigAction, &SigActionTerm ) < 0 ) {
    int	nErrNo	= errno;
    LOG (( "Installing SIGTERM handler failed with errno %d.",
	   nErrNo ));
  }
#endif /* WIN32 */

  RETURN ( VOID );
} /* fnInitializeAdminModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeAdminModule	( void )
{
  RPCPORT	nRpcPort;

  PROCEDURE	( fnDeinitializeAdminModule );

#if WIN32
  {
    struct svc_req	* pRequest = fnServerPlobdRequest ();
    if ( pRequest != NULL ) {
      shutdown ( pRequest->rq_xprt->xp_sock, SD_BOTH );
    }
  }
#endif /* WIN32 */

  nRpcPort	= fnPlobdGetPort ();
  if ( nRpcPort >= nMasterPort ) {
    svc_unregister ( fnPlobdGetPortOffset () + nRpcPort, fnPlobdGetPort () );
  }

  RETURN ( VOID );
} /* fnDeinitializeAdminModule */

/* ----------------------------------------------------------------------- */
typedef BOOL	( * PFNREPLY ) ( SHORTOBJID	oShortObjIdHeap,
				 BOOL		bForce );

/* ----------------------------------------------------------------------- */
static BOOL	fnExitOrRestart	( void		(*pfnAction)(int),
				  LPCSTR	pszAction,
				  PFNREPLY	pfnReply,
				  SHORTOBJID	oShortObjIdHeap,
				  BOOL		bForce )
{
  int		nSessions	= 0;
  int		nClientAddr [ 4 ];
  char		szClientName [ MAXNETNAMELEN + 1 ];
  uid_t		nUid = (uid_t) -1;
  OBJID		oHeap, oUser, oMachine;
  BOOL		bDone = FALSE;

  PROCEDURE	( fnExitOrRestart );
  ASSERT ( StableHeap_is_open );

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  oUser		= fnHeapUser ( oHeap );
  oMachine	= fnHeapMachine ( oHeap );

  /* Look who is visiting us: */
  fnGetClientCred ( fnServerPlobdRequest (), gfnNameOf ( oUser, (LPINT) NULL ),
		    &nUid, (gid_t *) NULL, (short *) NULL, (gid_t *) NULL,
		    nClientAddr, szClientName, sizeof ( szClientName ) );

  if ( fnAdminP ( oUser, oMachine ) != eaAdminTrue ) {
    ERROR (( "Only the admin user may %s the daemon.", pszAction ));
    RETURN ( FALSE );
  }
  nSessions	= gfnCount ( Sessions () );
  if ( nSessions > 1 && ! bForce ) {
    ERROR (( "There are %d sessions active; only a maximum of\n"
	     "       one session may be active to %s the daemon.",
	     nSessions, pszAction ));
    RETURN ( FALSE );
  }
  LOG (( szFormatRestart, pszAction, "administrator request", nSessions ));
  bDone	= TRUE;
  fnServerPlobdReply ( (void(*)()) pfnReply, &bDone );
  (*pfnAction) ( (int) bForce );
  RETURN ( TRUE );
} /* fnExitOrRestart */

/* ----------------------------------------------------------------------- */
BOOL	fnGetClientCred	( struct svc_req	* pRequest,
			  LPCSTR		pszUserName,
			  uid_t			* pnUid,
			  gid_t			* pnGid,
			  short			* pnGidMax,
			  gid_t			* pnGidList,
			  int			pnClientAddr [ 4 ],
			  LPSTR			pszClientName,
			  size_t		nClientName )
{
  static const char	szReceivedAuth []	=
    "Client %s@%s:\n"
    "       Received a %s authentication, expected at least %s.";
  static const char	szAuthNone []		= "AUTH_NONE";
  static const char	szAuthSys []		= "AUTH_SYS";
  static const char	szAuthDes []		= "AUTH_DES";

  uid_t			nUid = (uid_t) 0;
  gid_t			nGid = (gid_t) 0;
  short			nGidMax = 0;
  gid_t			nGidList [ 32 ];
  int			i, nClientAddr [ 4 ];
  char			szClientAddr [ 32 ],
    szClientName [ MAXNETNAMELEN + 1 ];
  struct netconfig	* pConf = (struct netconfig *) NULL;
#if SOLARIS
  struct netbuf		* pAddr = (struct netbuf *) NULL;
#else
  struct sockaddr_in	* pAddr = (struct sockaddr_in *) NULL;
#endif
  struct hostent	* pHost = (struct hostent *) NULL;
  struct authunix_parms	* pCredSys = (struct authunix_parms *) NULL;
  struct authdes_cred	* pCredDes = (struct authdes_cred *) NULL;
  unsigned long		nAddr;
  LPCSTR		pszTrimmedUser	=
    ( pszUserName != (LPCSTR) NULL && pszUserName [ 0 ] != '\0' ) ?
    pszUserName : szUnknownUser;
  BOOL			bDone	= TRUE;

  PROCEDURE	( fnGetClientCred );
  INITIALIZEPLOB;

  memset ( nGidList, 0, sizeof ( nGidList ) );
  memset ( nClientAddr, 0, sizeof ( nClientAddr ) );
  szClientName [ 0 ]	= '\0';

  if ( pRequest == NULL ) {

    /* Running in local mode, without TCP/IP */
    strncpy ( szClientName, szLocalName, nClientName );

  } else {

#if SOLARIS
    pConf	= getnetconfigent ( pRequest->rq_xprt->xp_netid );
    ASSERT ( pConf != NULL );
    pAddr	= svc_getrpccaller ( pRequest->rq_xprt );
    ASSERT ( pAddr != NULL );
    memset ( nClientAddr, 0, sizeof ( nClientAddr ) );
    sscanf ( taddr2uaddr ( pConf, pAddr ), "%d%*c%d%*c%d%*c%d",
	     & nClientAddr [ 0 ], & nClientAddr [ 1 ],
	     & nClientAddr [ 2 ], & nClientAddr [ 3 ] );
    freenetconfigent ( pConf );
    sprintf ( szClientAddr, "%d.%d.%d.%d",
	      nClientAddr [ 0 ], nClientAddr [ 1 ],
	      nClientAddr [ 2 ], nClientAddr [ 3 ] );
    nAddr	= inet_addr ( szClientAddr );
#else
    pAddr	= svc_getcaller ( pRequest->rq_xprt );
    nAddr	= pAddr->sin_addr.s_addr;
    strncpy ( szClientAddr, inet_ntoa ( pAddr->sin_addr ),
	      sizeof ( szClientAddr ) );
    sscanf ( szClientAddr, "%d%*c%d%*c%d%*c%d",
	     & nClientAddr [ 0 ], & nClientAddr [ 1 ],
	     & nClientAddr [ 2 ], & nClientAddr [ 3 ] );
#endif

    pHost	= gethostbyaddr ( (char*) &nAddr, sizeof ( nAddr ), AF_INET );
    strncpy ( szClientName,
	      ( pHost != NULL && pHost->h_name != NULL) ?
	      pHost->h_name : szClientAddr,
	      sizeof ( szClientName ) );
    /* I can't believe it: On NT, pHost->h_name has a trailing ^M! */
    for ( i = strlen ( szClientName ) - 1;
	  i >= 0 && szClientName [ i ] < ' '; i-- ) {
      szClientName [ i ]	= '\0';
    }

    switch ( pRequest->rq_cred.oa_flavor ) {
    case AUTH_NONE:
#if HASAUTH == AUTH_NONE
      /* Expect no specific authentication: */
#elif HASAUTH == AUTH_SYS
      /* Expect at least a UNIX-style authentication: */
      ERROR (( szReceivedAuth, pszTrimmedUser, szClientName,
	       szAuthNone, szAuthSys ));
      RETURN ( FALSE );
#elif HASAUTH == AUTH_DES
      /* Expect at least a DES-style authentication: */
      ERROR (( szReceivedAuth, pszTrimmedUser, szClientName,
	       szAuthNone, szAuthDes ));
      RETURN ( FALSE );
#else
#error Unknown HASAUTH specified.
#endif
      break;
#if defined(AUTH_SYS)
    case AUTH_SYS:
#elif defined(AUTH_UNIX)
    case AUTH_UNIX:
#else
#error Cant decide on AUTH_SYS or AUTH_UNIX.
#endif
#if HASAUTH == AUTH_NONE || HASAUTH == AUTH_SYS
      /* Expect no specific or a UNIX-style authentication: */
#else
      /* Expect at least a DES-style authentication: */
      ERROR (( szReceivedAuth,  pszTrimmedUser, szClientName,
	       szAuthSys, szAuthDes ));
      RETURN ( FALSE );
#endif
      pCredSys	= (struct authunix_parms *) pRequest->rq_clntcred;
      ASSERT ( pCredSys != NULL );
      nUid	= pCredSys->aup_uid;
      nGid	= pCredSys->aup_gid;
      nGidMax	= pCredSys->aup_len;
      if ( nGidMax > 0 && pCredSys->aup_gids != NULL ) {
	memcpy ( nGidList, pCredSys->aup_gids,
		 nGidMax * sizeof ( nGidList [ 0 ] ) );
      }
      break;
#if defined(AUTH_DES)&&SOLARIS
      /* Only SOLARIS has a working AUTH_DES authentication: */
    case AUTH_DES:
      pCredDes	= (struct authdes_cred *) pRequest->rq_clntcred;
      ASSERT ( pCredDes != NULL );
      ASSERT ( pCredDes->adc_fullname.name != NULL );
      if ( ! authdes_getucred ( pCredDes, &nUid, &nGid,
				&nGidMax, nGidList ) ) {
	ERROR (( "Client %s@%s:\n"
		 "       Reading authentication credentials failed.",
		 pszTrimmedUser, szClientName ));
	RETURN ( FALSE );
      }
      break;
#endif
    default:
      ERROR (( "Client %s@%s:\n"
	       "       Received unknown authentication flavor %d.",
	       pszTrimmedUser, szClientName, pRequest->rq_cred.oa_flavor ));
      RETURN ( FALSE );
    }
  }

  if ( pnUid != NULL ) {
    *pnUid	= nUid;
  }

  if ( pnGid != NULL ) {
    *pnGid	= nGid;
  }

  if ( pnGidMax != NULL ) {
    *pnGidMax	= nGidMax;
  }

  if ( pnGidList != NULL ) {
    memcpy ( pnGidList, nGidList, sizeof ( pnGidList [ 0 ] ) * nGidMax );
  }

  if ( pnClientAddr != NULL ) {
    memcpy ( pnClientAddr, nClientAddr, sizeof ( nClientAddr ) );
  }

  if ( pszClientName != NULL ) {
    strncpy ( pszClientName, szClientName, nClientName );
  }
  RETURN ( bDone );
} /* fnGetClientCred */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		 fnServerGetPortByDirectory,
		 "c-sh-get-port-by-directory",
		( argument ( CONST_STRING, vector_in, szDirectory )
		  and
		  argument ( GETACTION, value_in, eAction ) ) )
{
  int	nRpcPort = -1;

  INITIALIZEPLOB;
  if ( StoreSession ( NULLOBJID ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( -1 );
    }
  }
  
  nRpcPort	=
    fnStartLocalServer ( szDirectory, eAction, fnPlobdGetPort () );

  UnstoreSession ();
  RETURN ( nRpcPort );
} EndFunction ( fnServerGetPortByDirectory );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerGetPID, "c-sh-get-pid",
		( voidArgument ) )
{
  FIXNUM	nPID	= 0;

  INITIALIZEPLOB;

  nPID	= (int) getpid ();

  RETURN ( nPID );
} EndFunction ( fnServerGetPID );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM, /* Returns server's PID */
		fnServerGetDirectory, "c-sh-get-directory",
		( argument ( FIXNUM, value_in, nDirectory )
		  and
		  argument ( STRING ( nDirectory ),
			     vector_out, szDirectory ) ) )
{
  FIXNUM	nPID	= 0;
  char		szBuffer [ 512 ];

  INITIALIZEPLOB;

  nPID	= fnServerGetPID ();
  getcwd ( szBuffer, sizeof ( szBuffer ) );
  strncpy ( szDirectory, szBuffer, nDirectory );
  szDirectory [ nDirectory - 1 ]	= '\0';

#if WIN32
  {
    int	i;
    for ( i = 0; szDirectory [ i ] != '\0'; i++ ) {
      if ( szDirectory [ i ] == '\\' ) {
	szDirectory [ i ]	= '/';
      }
    }
  }
#endif

  RETURN ( nPID );
} EndFunction ( fnServerGetDirectory );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnServerExit, "c-sh-exit",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bForceExit ) ) )
{
  BOOL	bDone;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }

  bDone	= fnExitOrRestart ( exit, "exit", fnServerExit,
			    oShortObjIdHeap, bForceExit );

  UnstoreSession ();
  RETURN ( bDone );
} EndFunction ( fnServerExit );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnServerDbReset, "c-sh-reset",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bForceReset ) ) )
{
  BOOL	bDone;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }

  bDone	= fnExitOrRestart ( fnReset, "reset", fnServerDbReset,
			    oShortObjIdHeap, bForceReset );

  UnstoreSession ();
  RETURN ( bDone );
} EndFunction ( fnServerDbReset );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnServerRestart, "c-sh-restart",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bForceRestart ) ) )
{
  BOOL	bDone;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }

  bDone	= fnExitOrRestart ( fnRestart, "restart", fnServerRestart,
			    oShortObjIdHeap, bForceRestart );

  UnstoreSession ();
  RETURN ( bDone );
} EndFunction ( fnServerRestart );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnServerSuspend, "c-sh-suspend",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( CONST_STRING, vector_in, szReason ) ) )
{
  SHORTOBJID	oSuspendedBy = NULLOBJID;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }

  if ( oShortObjIdHeap != NULLOBJID && ! bGlobalSuspended ) {
    OBJID	oHeap, oUser, oMachine;
    time_t	Time;
    char	szTime [ 64 ];
 
    ASSERT ( StableHeap_is_open );
    oHeap	= Short2LongObjId ( oShortObjIdHeap );
    oUser	= fnHeapUser ( oHeap );
    oMachine	= fnHeapMachine ( oHeap );
    if ( fnAdminP ( oUser, oMachine ) != eaAdminTrue ) {
      ERROR (( "Only the admin user may suspend the daemon." ));
      RETURN ( NULLOBJID );
    }
    LOG (( "Suspended by %s.%s%s",
	   fnPrintObject ( oHeap, (LPSTR) NULL, 0 ),
	   ( szReason != NULL && szReason [ 0 ] != '\0' ) ? "\n       Reason: " : szEmpty,
	   ( szReason != NULL && szReason [ 0 ] != '\0' ) ? szReason : szEmpty ));
    time ( &Time );
    strftime ( szTime, sizeof ( szTime ), szTimeFormat, localtime ( &Time ) );
    sprintf ( szGlobalSuspendedMsg, "Suspended by %s at %s.%s%s",
	      fnPrintObject ( oHeap, (LPSTR) NULL, 0 ), szTime,
	      ( szReason != NULL && szReason [ 0 ] != '\0' ) ? " Reason: " : szEmpty,
	      ( szReason != NULL && szReason [ 0 ] != '\0' ) ? szReason : szEmpty );
    fflush ( NULL );
    SH_stabilise ();
    bGlobalSuspended	= TRUE;
    oGlobalSuspendedBy	= oHeap;
  }
  oSuspendedBy	= LONG2SHORTOBJID ( oGlobalSuspendedBy );

  UnstoreSession ();
  RETURN ( oSuspendedBy );
} EndFunction ( fnServerSuspend );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnServerResume, "c-sh-resume",
		( voidArgument ) )
{
  SHORTOBJID	oSuspendedBy;

  INITIALIZEPLOB;
  if ( StoreSession ( NULLOBJID ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }

  oSuspendedBy			= LONG2SHORTOBJID ( oGlobalSuspendedBy );
  bGlobalSuspended		= FALSE;

  LOG (( "Resume server suspended by %s",
	 fnPrintObject ( oGlobalSuspendedBy, (LPSTR) NULL, 0 ) ));

  oGlobalSuspendedBy		= NULLOBJID;
  szGlobalSuspendedMsg [ 0 ]	= '\0';

  RETURN ( oSuspendedBy );
} EndFunction ( fnServerResume );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
