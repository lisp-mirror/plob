/* -------------------------------------------------------------------------
| Module	lplobheap.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB local source code.
 ------------------------------------------------------------------------- */
#include	<errno.h>
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

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientTransactionBegin, "c-sh-begin-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionBegin ( oShortObjIdHeap, bIgnoreError ) );
} EndFunction ( fnClientTransactionBegin );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientTransactionCancel, "c-sh-cancel-transaction",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionCancel ( oShortObjIdHeap, bIgnoreError ) );
} EndFunction ( fnClientTransactionCancel );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientTransactionEnd, "c-sh-end-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bIgnoreError ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionEnd ( oShortObjIdHeap, bIgnoreError ) );
} EndFunction ( fnClientTransactionEnd );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnClientTransactionFlush, "c-sh-flush-transaction",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  fnServerTransactionFlush ( oShortObjIdHeap );
  RETURN ( VOID );
} EndFunction ( fnClientTransactionFlush );

/* ----------------------------------------------------------------------- */
BeginFunction ( TRACTID,
	        fnClientDbTransactionP, "c-sh-in-transaction-p",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( TRACTID, value_in, nTractId ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerDbTransactionP ( oShortObjIdHeap, nTractId ) );
} EndFunction ( fnClientDbTransactionP );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
