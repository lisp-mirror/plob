/* -------------------------------------------------------------------------
| Module	cplobsequ.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB client source code.
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

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitializeSequModule	( void )
{
  PROCEDURE	( fnInitializeSequModule );

  RETURN ( VOID );
} /* fnInitializeSequModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitializeSequModule	( void )
{
  PROCEDURE	( fnDeinitializeSequModule );

  RETURN ( VOID );
} /* fnDeinitializeSequModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
