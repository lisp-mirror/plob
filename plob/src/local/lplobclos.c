/* -------------------------------------------------------------------------
| Module	plob.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB local source code.
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
#include	"lplobsequ.h"
#include	"lplobstruct.h"
#include	"lploblock.h"
#include	"lplobclos.h"
#include	"lplobheap.h"
#include	"lplobbtree.h"
#include	"lplobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbCreateInstance, "c-sh-create-instance",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in,
			     oShortObjIdClassDescr ) ) )
{
  SHORTOBJID	oInstance = NULLOBJID;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  fnServerDbCreateInstances ( oShortObjIdHeap, oShortObjIdClassDescr,
			1, (u_int *) &oInstance, (u_int *) NULL,
			(u_int *) NULL );
  RETURN ( oInstance );
} EndFunction ( fnClientDbCreateInstance );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientInstanceWriteWrapper,
		"c-sh-write-instance-wrapper",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdClassDescr ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerInstanceWriteWrapper ( oShortObjIdHeap, oShortObjIdInstance,
				      oShortObjIdClassDescr ) );
} EndFunction ( fnClientInstanceWriteWrapper );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientInstanceWriteData,
		"c-sh-write-instance-data",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdData ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerInstanceWriteData ( oShortObjIdHeap, oShortObjIdInstance,
				   oShortObjIdData ) );
} EndFunction ( fnClientInstanceWriteData );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
