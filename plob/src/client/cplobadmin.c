/* -------------------------------------------------------------------------
| Module	cplobadmin.c
| Author	Heiko Kirschke
| Copyright	(C) 1998 Heiko Kirschke
| Date		1998/02/04
| Description	PLOB administration functions.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<signal.h>
#if	!WIN32
#include	<unistd.h>
#endif
#if HASSIGINFO
#include	<siginfo.h>
#include	<ucontext.h>
#endif
#include	<sys/types.h>
#if LINUX
#include	<asm/sigcontext.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobnumber.h"
#include	"cplobsequ.h"
#include	"cplobstruct.h"
#include	"cplobclos.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobroot.h"
#include	"cplobadmin.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */
static const char	szNoConnection []	=
"Looks as if no connection to the server could be established.";

/* ----------------------------------------------------------------------- */
#if HASSIGINFO

static struct sigaction	SigActionTerm;

#endif /* HASSIGINFO */

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

  exit ( sig );

  RETURN ( VOID );
} /* fnSigTerm */

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeAdminModule	( void )
{
  PROCEDURE	( fnInitializeAdminModule );

  RETURN ( VOID );
} /* fnInitializeAdminModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeAdminModule	( void )
{
#if HASSIGINFO
  struct sigaction	SigAction;
#endif /* HASSIGINFO */

  PROCEDURE	( fnDeinitializeAdminModule );

#if WIN32
  signal ( SIGTERM, fnSigTerm );
#elif HASSIGINFO
  memset ( &SigAction, 0, sizeof ( SigAction ) );
  SigAction.sa_sigaction	= fnSigTerm;
  SigAction.sa_flags		= SA_SIGINFO;
  sigaction ( SIGTERM, &SigAction, &SigActionTerm );
#endif /* HASSIGINFO */

  RETURN ( VOID );
} /* fnDeinitializeAdminModule */

/* ----------------------------------------------------------------------- */
typedef BOOL ( * PFNACTION ) ( LPCSTR, SHORTOBJID, BOOL );
static BOOL	fnRemoteCommand	( LPCSTR	pszURL,
				  GETACTION	eAction,
				  PFNACTION	pfnAction,
				  LPCSTR	pszAction,
				  BOOL		bForce )
{
  PCLIENT	pClient = (PCLIENT) NULL;
  SHORTOBJID	oHeap;
  char		szDescription [ 256 ];
  char		szHost [ MAX_FNAME ];
  int		nAuth = -1;
  AUTH		* pAuth = (AUTH *) NULL;
  BOOL		bDone	= FALSE;

  PROCEDURE	( fnRemoteCommand );
  ASSERT ( pszURL != (LPCSTR) NULL );
  ASSERT ( pszAction != (LPCSTR) NULL );

  strcpy ( szDescription, pszAction );
  oHeap		= fnOpen ( pszURL, eAction, szDescription );
  if ( oHeap != NULLOBJID ) {
    pClient	= fnClientPlobd ();
    if ( pClient != NULL ) {
      fnSplitURL ( pszURL, szHost, (LPSTR) NULL, (LPSTR) NULL );
      pAuth	= fnCreateAuth ( szHost, &nAuth );
      if ( pAuth != NULL ) {
	auth_destroy ( pClient->cl_auth );
	pClient->cl_auth	= pAuth;
      }
      bDone		= ( pfnAction != NULL ) ?
	( *pfnAction ) ( pszURL, oHeap, bForce ) : TRUE;
      if ( nAuth != AUTH_NONE ) {
	pAuth		= authnone_create ();
	auth_destroy ( pClient->cl_auth );
	pClient->cl_auth	= pAuth;
      }
      fnClientDbClose ( oHeap, FALSE );
    } else {
      char	szContinue [ 128 ];
      sprintf ( szContinue, "Ignore %s request to PLOB! server.",
		pszAction );
      CERROR (( szContinue, szNoConnection ));
    }
  }
  RETURN ( bDone );
} /* fnRemoteCommand */

/* ----------------------------------------------------------------------- */
AUTH *	fnCreateAuth	( LPCSTR	pszServer,
			  LPINT		pnAuth )
{
  static const char	szAuthSysCont []	=
    "Create a AUTH_NONE authentication instead.";
  static const char	szAuthSysFailed []	=
    "Creating the AUTH_SYS authentication failed.";

  static const char	szAuthDesCont []	=
    "Create a AUTH_SYS authentication instead.";
  static const char	szAuthDesFailed []	=
    "Creating the AUTH_DES authentication failed. Check if\n"
    "       `%s' names a host in your\n"
    "       net domain and that keyserv is running on your local\n"
    "       client machine. Check if calling the shell command\n"
    "       `keylogin' might help.";

  char			szServerName [ MAXNETNAMELEN + 1 ];
  int			nAuth = -1;
  AUTH *		pAuth = (AUTH *) NULL;

  PROCEDURE	( fnCreateAuth );
  ASSERT ( pszServer != (LPCSTR) NULL );

#if HASAUTH == AUTH_NONE
  /* Do nothing: */
  nAuth	= AUTH_NONE;
  pAuth	= (AUTH *) NULL;
#elif ( HASAUTH == AUTH_SYS ) || ( IRIX && HASAUTH >= AUTH_SYS )
  /* Create a UNIX-style authentication: */
  pAuth	= authunix_create_default ();
  if ( pAuth != NULL ) {
    nAuth	= AUTH_SYS;
  } else {
    CERROR (( szAuthSysCont, szAuthSysFailed ));
    nAuth	= AUTH_NONE;
    pAuth	= authnone_create ();
  }
#elif HASAUTH == AUTH_DES
  /* Create a DES authentication. IRIX has no DES authentication: */
  if ( ! host2netname ( szServerName, 
			( strcmp ( pszServer, "localhost" ) == 0 ) ?
			NULL :  pszServer,
			NULL ) ) {
    strncpy ( szServerName, pszServer, sizeof ( szServerName ) );
  }
  pAuth	= authdes_seccreate ( szServerName, 60, NULL, NULL );
  if ( pAuth != NULL ) {
    nAuth	= AUTH_DES;
  } else {
    CERROR (( szAuthDesCont, szAuthDesFailed, pszServer ));
    pAuth	= authunix_create_default ();
    if ( pAuth != NULL ) {
      nAuth	= AUTH_SYS;
    } else {
      CERROR (( szAuthSysCont, szAuthSysFailed ));
      nAuth	= AUTH_NONE;
      pAuth	= authnone_create ();
    }
  }
#else
#error Unknown HASAUTH specified.
#endif
  if ( pnAuth != NULL ) {
    *pnAuth	= nAuth;
  }
  RETURN ( pAuth );
} /* fnCreateAuth */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientCreateDatabase, "c-sh-create-database",
		( argument ( CONST_STRING, vector_in, szURL ) ) )
{
  INITIALIZEPLOB;

  RETURN ( fnRemoteCommand ( szURL, (GETACTION)
			     ( (unsigned int) eGetPortActive |
			       (unsigned int) eCreateDatabase |
			       (unsigned int) eStartServer ),
			     (PFNACTION) NULL,
			     "create database", FALSE ) );
} EndFunction ( fnClientCreateDatabase );

/* ----------------------------------------------------------------------- */
static BOOL	fnClientServerExit	( LPCSTR	pszURL,
					  SHORTOBJID	oHeap,
					  BOOL		bForce )
{
  RETURN ( fnServerExit ( oHeap, bForce ) );
} /* fnClientServerExit */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientExit, "c-sh-exit",
		( argument ( CONST_STRING, vector_in, szURL )
		  and
		  argument ( BOOL, value_in, bForceExit ) ) )
{
  INITIALIZEPLOB;

  RETURN ( fnRemoteCommand ( szURL, eGetPortActive,
			     fnClientServerExit,
			     "exit", bForceExit ) );
} EndFunction ( fnClientExit );

/* ----------------------------------------------------------------------- */
static BOOL	fnClientServerReset	( LPCSTR	pszURL,
					  SHORTOBJID	oHeap,
					  BOOL		bForce )
{
  RETURN ( fnServerDbReset ( oHeap, bForce ) );
} /* fnClientServerReset */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientDbReset, "c-sh-reset",
		( argument ( CONST_STRING, vector_in, szURL )
		  and
		  argument ( BOOL, value_in, bForceReset ) ) )
{
  INITIALIZEPLOB;

  RETURN ( fnRemoteCommand ( szURL, eGetPortActive, fnClientServerReset,
			     "reset", bForceReset ) );
} EndFunction ( fnClientDbReset );

/* ----------------------------------------------------------------------- */
static BOOL	fnClientServerRestart	( LPCSTR	pszURL,
					  SHORTOBJID	oHeap,
					  BOOL		bForce )
{
  BOOL	bRestarted	= FALSE;
  int	nRpcPortCurr	= -1;

  nRpcPortCurr	= (int) fnPlobdGetPort ();
  bRestarted	= fnServerRestart ( oHeap, bForce );
  if ( bRestarted ) {
    /* Wait until the connection is re-established: */
    char		szHost [ MAX_FNAME ];
    char		szProtocol [ MAX_FNAME ];
    char		szDirectory [ MAX_FNAME ];
    PCLIENT		pClient	= NULL;
    struct timeval	Timeout;
    int			i;
    BOOL		bPing;

    fnSplitURL ( pszURL, szHost, szProtocol, szDirectory );
    memset ( &Timeout, 0, sizeof ( Timeout ) );
    Timeout.tv_sec	= 4;
    sleep ( 2 );
    for ( i = 0, bPing = TRUE; i < 10 && bPing;
	  sleep ( ( i < 5 ) ? 2 : 4 ), i++ ) {
      if ( pClient == NULL ) {
	pClient	= clnt_create ( (char *) szHost,
				fnPlobdGetPortOffset () + nRpcPortCurr,
				fnPlobdGetVersion (),
				(char *) szProtocol );
      }
      if ( pClient != NULL ) {
	bPing	= ( clnt_call ( pClient, NULLPROC,
				(xdrproc_t) xdr_void, (caddr_t) NULL,
				(xdrproc_t) xdr_void, (caddr_t) NULL,
				Timeout ) != RPC_SUCCESS );
      }
      if ( ! bPing ) {
	break;
      }
    }
    if ( pClient != NULL ) {
      auth_destroy ( pClient->cl_auth );
      clnt_destroy ( pClient );
      pClient	= (PCLIENT) NULL;
    }
  }

  RETURN ( bRestarted );
} /* fnClientServerRestart */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientRestart, "c-sh-restart",
		( argument ( CONST_STRING, vector_in, szURL )
		  and
		  argument ( BOOL, value_in, bForceRestart ) ) )
{
  INITIALIZEPLOB;

  RETURN ( fnRemoteCommand ( szURL, eGetPortActive, fnClientServerRestart,
			     "restart", bForceRestart ) );
} EndFunction ( fnClientRestart );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
