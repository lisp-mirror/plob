/* ----------------------------------------------------------------------------
| Module	splobff.c
| Author	Heiko Kirschke
| Copyright	(C) 1993-1998 Heiko Kirschke
| Date		1998/04/14 Created
| Description	Foreign function interface to LISP
 --------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<string.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobff.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeFfModule		( void )
{
  PROCEDURE	( fnInitializeFfModule );

  RETURN ( VOID );
} /* fnInitializeFfModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeFfModule	( void )
{
  PROCEDURE	( fnDeinitializeFfModule );

  RETURN ( VOID );
} /* fnDeinitializeFfModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
