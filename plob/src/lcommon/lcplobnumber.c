/* -------------------------------------------------------------------------
| Module	lcplobnumber.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB local client source code.
 ------------------------------------------------------------------------- */

#include	<limits.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lcplob.h"
#include	"lcplobintern.h"
#include	"lcplobmisc.h"
#include	"lcplobtype.h"
#include	"lcplobnumber.h"
#include	"lcplobsequ.h"
#include	"lcplobstruct.h"
#include	"lcplobclos.h"
#include	"lcploblock.h"
#include	"lcplobheap.h"
#include	"lcplobbtree.h"
#include	"lcplobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( DOUBLE_FLOAT,
		fnShortToSingleFloat, "c-short-to-single-float",
		( argument ( SHORTOBJID, value_in, oShortFloat ) ) )
{
  INITIALIZEPLOB;
  RETURN ( (DOUBLE_FLOAT)
	   OBJID2SINGLEFLOAT ( SHORT2LONGOBJID ( oShortFloat ) ) );
} EndFunction ( fnShortToSingleFloat );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
