/* -------------------------------------------------------------------------
| Module	lcplobmisc.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1993,1994 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB local client functions
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
#include	"trmalloc.h"
#include	"hash.h"
#include	"postore.h"
#include	"lcplob.h"
#include	"lcplobff.h"
#include	"lcplobintern.h"
#include	"lcplobnumber.h"
#include	"lcplobmisc.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( POINTER,
		 fnLISPmalloc, "c-malloc",
		 ( argument ( FIXNUM, value_in, nSizeInBytes ) ) )
{
  POINTER	pMemory	= NULL;

  INITIALIZEPLOB;

  pMemory	= Malloc ( nSizeInBytes );

  RETURN ( pMemory );
} EndFunction ( fnLISPmalloc );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnLISPfree, "c-free",
		( argument ( POINTER, value_in, pMemory ) ) )
{
  INITIALIZEPLOB;

  if ( pMemory != NULL ) {
    Free ( pMemory );
  }
} EndFunction ( fnLISPfree );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
