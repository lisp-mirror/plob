/* -------------------------------------------------------------------------
| Module	lplobmisc.c
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

#include	"global.h"
#include	"hash.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobff.h"
#include	"lplobintern.h"
#include	"lplobnumber.h"
#include	"lplobmisc.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BOOL		fnLISPsuspendCallback		( SHORTOBJID oLockBy,
						  SHORTOBJID oToLock,
						  LPCSTR lpszReason )
{
  PROCEDURE	( fnLISPsuspendCallback );
  RETURN ( FALSE );
} /* fnLISPsuspendCallback */

/* ----------------------------------------------------------------------- */
BOOL		fnLISPwakeupCallback		( SHORTOBJID oLockBy,
						  SHORTOBJID oToLock,
						  LPCSTR lpszReason )
{
  PROCEDURE	( fnLISPwakeupCallback );
  RETURN ( FALSE );
} /* fnLISPwakeupCallback */

/* ----------------------------------------------------------------------- */
BOOL		fnLISPmapClassInfoCallback	( SHTYPETAG nTypeTag,
						  LPCSTR lpszTypeName,
						  int nObjIdSize,
						  int nValueSize )
{
  PROCEDURE	( fnLISPmapClassInfoCallback );
  RETURN ( FALSE );
} /* fnLISPmapClassInfoCallback */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
