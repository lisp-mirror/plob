/* -------------------------------------------------------------------------
| Module	lcplobadmin.c
| Author	Heiko Kirschke
| Copyright	(C) 1998 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB administration functions.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
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
#include	"lcplobsequ.h"
#include	"lcplobstruct.h"
#include	"lcplobclos.h"
#include	"lcploblock.h"
#include	"lcplobheap.h"
#include	"lcplobbtree.h"
#include	"lcplobroot.h"
#include	"lcplobadmin.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/