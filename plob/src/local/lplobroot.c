/* -------------------------------------------------------------------------
| Module	lplobroot.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB local client source code.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
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

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbWriteRoot, "c-sh-write-root",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerDbWriteRoot ( oShortObjIdHeap, oShortObjId ) );
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
  UNSTORESESSION ();

  switch ( eWhat ) {
  case esvClientCcode:
    nVersion	= PlobVersion;
    break;
  default:
    nVersion	= fnServerGetVersion ( oShortObjIdHeap, eWhat );
    break;
  }

  RETURN ( nVersion );
} EndFunction ( fnClientGetVersion );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
