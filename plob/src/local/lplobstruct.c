/* -------------------------------------------------------------------------
| Module	lplobstruct.c
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
#include	"lploblock.h"
#include	"lplobheap.h"
#include	"lplobbtree.h"
#include	"lplobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbCreateStructure, "c-sh-create-structure",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in,
			     oShortObjIdStructDescr ) ) )
{
  SHORTOBJID	oStructure = NULLOBJID;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  fnServerDbCreateStructures ( oShortObjIdHeap, oShortObjIdStructDescr,
			 1, (u_int *) &oStructure, (u_int *) NULL );

  RETURN ( oStructure );
} EndFunction ( fnClientDbCreateStructure );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
