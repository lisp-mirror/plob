/* -------------------------------------------------------------------------
| Module	cplobroot.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB client source code.
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
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
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
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeRootModule		( void )
{
  PROCEDURE	( fnInitializeRootModule );

  RETURN ( VOID );
} /* fnInitializeRootModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeRootModule	( void )
{
  PROCEDURE	( fnDeinitializeRootModule );

  RETURN ( VOID );
} /* fnDeinitializeRootModule */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbWriteRoot, "c-sh-write-root",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  SHORTOBJID	oShortObjIdHeapNew;
  PHEAPCACHE	pHeapCache;
  TRACTID	nTractId;

  INITIALIZEPLOB;
  /* Flush all objects ... */
  fnCacheFlush ( NULLOBJID, NULLOBJID );
  /* ... and invalidate all caches: */
  fnInvalidateAllCaches ();
  oShortObjIdHeapNew	= fnServerDbWriteRoot ( oShortObjIdHeap, oShortObjId );
  if ( oShortObjId == NULLOBJID && oShortObjIdHeapNew != NULLOBJID ) {
    /* The root object has been re-formatted, so all cached data
       becomes invalid.  Destroy the client's object cache ... */
    fnCacheDestroy ( NULLOBJID );
    /* ... and reload the session: */
    pHeapCache	= fnCacheCreate ( oShortObjIdHeapNew );
    ASSERT ( pHeapCache != NULL );
    if ( bGlobalDoCaching ) {
      nTractId	= fnServerDbTransactionP ( oShortObjIdHeapNew, NULLTRACTID );
      pHeapCache->onTractId	= ( nTractId == NULLTRACTID ) ?
	unbound : Fixnum2ObjId ( nTractId );
    }
  }
  RETURN ( oShortObjIdHeapNew );
} EndFunction ( fnClientDbWriteRoot );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientGetVersion, "c-sh-get-version",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( GETVERSION, value_in, eWhat ) ) )
{
  FIXNUM	nVersion	= 0;

  INITIALIZEPLOB;

  switch ( eWhat ) {
  case esvClientCcode:
    nVersion	= PlobVersion;
    break;
  default:
    if ( fnClientPlobd () != NULL ) {
      nVersion	= fnServerGetVersion ( oShortObjIdHeap, eWhat );
    }
    break;
  }

  RETURN ( nVersion );
} EndFunction ( fnClientGetVersion );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
