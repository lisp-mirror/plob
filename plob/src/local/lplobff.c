/* -------------------------------------------------------------------------
| Module	lplobff.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1997 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB source code for Allegro foreign function relocation.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobff.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
