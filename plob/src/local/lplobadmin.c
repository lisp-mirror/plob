/* -------------------------------------------------------------------------
| Module	lplobadmin.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
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
#include	"plobsequ.h"
#include	"lplobstruct.h"
#include	"lplobclos.h"
#include	"lploblock.h"
#include	"lplobheap.h"
#include	"lplobbtree.h"
#include	"plobregex.h"
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
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
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
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
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
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bForceRestart ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  /* Do nothing, makes no sense for local client ... */
  RETURN ( TRUE );
} EndFunction ( fnClientRestart );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientSuspend, "c-sh-suspend",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( CONST_STRING, vector_in, szReason ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerSuspend ( oShortObjIdHeap, szReason ) );
} EndFunction ( fnClientSuspend );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientResume, "c-sh-resume",
		( voidArgument ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerResume () );

} EndFunction ( fnClientResume );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
