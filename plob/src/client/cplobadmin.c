/* -------------------------------------------------------------------------
| Module	cplobadmin.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/02/04
| Description	PLOB administration functions.
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB client header file:
|		Macros and functions for usage by the PLOB client
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
#include	"cplobregex.h"
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
  BOOL	bDone = FALSE;

  INITIALIZEPLOB;

  bDone = ( fnStartRemoteServer ( szURL, (GETACTION)
				  ( (unsigned int) eGetPortActive |
				    (unsigned int) eCreateDatabase |
				    (unsigned int) eStartServer ) ) >= 0 );

  RETURN ( bDone );
} EndFunction ( fnClientCreateDatabase );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientExit, "c-sh-exit",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bForceExit ) ) )
{
  BOOL	bDone	= FALSE;

  INITIALIZEPLOB;

  fnHeapCloseAllExcept1 ( oShortObjIdHeap );
  bDone	= fnServerExit ( oShortObjIdHeap, bForceExit );
  fnClientDestroy ( NULL );

  RETURN ( bDone );

} EndFunction ( fnClientExit );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientDbReset, "c-sh-reset",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bForceReset ) ) )
{
  INITIALIZEPLOB;

  RETURN ( fnServerDbReset ( oShortObjIdHeap, bForceReset ) );

} EndFunction ( fnClientDbReset );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientRestart, "c-sh-restart",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bForceRestart ) ) )
{
  BOOL	bRestarted	= FALSE;
  int	nRpcPortCurr	= -1;

  INITIALIZEPLOB;

  nRpcPortCurr	= (int) fnPlobdGetPort ();
  fnHeapCloseAllExcept1 ( oShortObjIdHeap );
  bRestarted	= fnServerRestart ( oShortObjIdHeap, bForceRestart );
  fnClientDestroy ( NULL );
  if ( bRestarted ) {
    /* Wait until the connection is re-established: */
    PCLIENT		pClient	= NULL;
    struct timeval	Timeout;
    int			i;
    BOOL		bPing;

    memset ( &Timeout, 0, sizeof ( Timeout ) );
    Timeout.tv_sec	= 4;
    sleep ( 2 );
    for ( i = 0, bPing = TRUE; i < 10 && bPing;
	  sleep ( ( i < 5 ) ? 2 : 4 ), i++ ) {
      if ( pClient == NULL ) {
	pClient	= clnt_create ( fnClientPlobdHost (),
				fnPlobdGetPortOffset () + nRpcPortCurr,
				fnPlobdGetVersion (),
				fnClientPlobdTransport () );
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
} EndFunction ( fnClientRestart );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientSuspend, "c-sh-suspend",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( CONST_STRING, vector_in, szReason ) ) )
{
  SHORTOBJID	oSuspendedBy;

  INITIALIZEPLOB;

  oSuspendedBy	= fnServerSuspend ( oShortObjIdHeap, szReason );

  RETURN ( oSuspendedBy );
} EndFunction ( fnClientSuspend );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientResume, "c-sh-resume",
		( voidArgument ) )
{
  SHORTOBJID	oSuspendedBy;

  INITIALIZEPLOB;

  oSuspendedBy	= fnServerResume ();

  RETURN ( oSuspendedBy );
} EndFunction ( fnClientResume );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
