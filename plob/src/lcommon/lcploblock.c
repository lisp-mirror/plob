/* -------------------------------------------------------------------------
| Module	lcploblock.c
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

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lcplob.h"
#include	"lcplobintern.h"
#include	"lcplobmisc.h"
#include	"lcplobtype.h"
#include	"lcplobnumber.h"
#include	"lcplobroot.h"
#include	"lcploblock.h"
#include	"lcplobbtree.h"
#include	"lcplobheap.h"
#include	"lcplobsequ.h"
#include	"lcplobclos.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
