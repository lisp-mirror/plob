/* -------------------------------------------------------------------------
| Module	lplobadmin.c
| Author	Heiko Kirschke
| Copyright	(C) 1998 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB administration functions.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobintern.h"
#include	"lplobmisc.h"
#include	"lplobtype.h"
#include	"lplobnumber.h"
#include	"lplobsequ.h"
#include	"lplobstruct.h"
#include	"lplobclos.h"
#include	"lploblock.h"
#include	"lplobheap.h"
#include	"lplobbtree.h"
#include	"lplobroot.h"
#include	"lplobadmin.h"

#define		RPCNOTYPES
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| RPC dummies
 ------------------------------------------------------------------------- */
RPCPORT			fnPlobdGetPortOffset	( void )
{
  return (RPCPORT) 0;
} /* fnPlobdGetPortOffset */

RPCPORT			fnPlobdGetPort	( void )
{
  return (RPCPORT) ( (int) nMasterPort - 1 );
} /* fnPlobdGetPort */

RPCPORT			fnPlobdSetPort	( RPCPORT	lPortNew )
{
  return fnPlobdGetPort ();
} /* fnPlobdSetPort */

RPCVERSION		fnPlobdGetVersion	( void )
{
  return (RPCVERSION) 0;
} /* fnPlobdGetVersion */

RPCVERSION		fnPlobdSetVersion	( RPCVERSION	lVersionNew )
{
  return fnPlobdGetVersion ();
} /* fnPlobdSetVersion */

struct svc_req *	fnServerPlobdRequest	( void )
{
  return (struct svc_req *) NULL;
} /* fnServerPlobdRequest */

struct svc_req *	fnServerPlobdReply
	( void (*pfnCalled)(), void * pReturnValue )
{
  return (struct svc_req *) NULL;
} /* fnServerPlobdReply */

bool_t xdr_fnServerGetPID_rets(XDR * pXdr, fnServerGetPID_rets* pReturn)
{
  return (bool_t) 0;
} /* xdr_fnServerGetPID_rets */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientCreateDatabase, "c-sh-create-database",
		( argument ( CONST_STRING, vector_in, szURL ) ) )
{
  char	szDirectory [ MAX_FNAME ];
  BOOL	bDone;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  fnSplitURL ( szURL, (LPSTR) NULL, (LPSTR) NULL, szDirectory );
  RETURN ( SH_create_database ( szDirectory, fnPLOBerrorCallback ) );
} EndFunction ( fnClientCreateDatabase );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientExit, "c-sh-exit",
		( argument ( CONST_STRING, vector_in, szURL )
		  and
		  argument ( BOOL, value_in, bForceExit ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  /* Do nothing, otherwise LISP would terminate ... */
  RETURN ( TRUE );
} EndFunction ( fnClientExit );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientDbReset, "c-sh-reset",
		( argument ( CONST_STRING, vector_in, szURL )
		  and
		  argument ( BOOL, value_in, bForceReset ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerDbReset ( NULLOBJID, bForceReset ) );
} EndFunction ( fnClientDbReset );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientRestart, "c-sh-restart",
		( argument ( CONST_STRING, vector_in, szURL )
		  and
		  argument ( BOOL, value_in, bForceRestart ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  /* Do nothing, makes no sense for local client ... */
  RETURN ( TRUE );
} EndFunction ( fnClientRestart );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
