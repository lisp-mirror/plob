/* -------------------------------------------------------------------------
| Module	lcplobtype.c
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
BeginFunction ( FIXNUM,
		fnClientDbTypeTagName, "c-sh-type-string",
		( argument ( SHTYPETAG, value_in, nTypeTag )
		  and
		  argument ( STRING ( nBuffer ), vector_out, pszBuffer )
		  and
		  argument ( FIXNUM, value_in, nBuffer ) ) )
{
  LPCSTR	pszTypeName;
  FIXNUM	nTypeName;

  INITIALIZEPLOB;

  pszTypeName	= fnServerDbTypeTagName ( nTypeTag );
  if ( pszTypeName == (LPCSTR) NULL ) {
    pszTypeName	= szEmpty;
  }
  nTypeName	= strlen ( pszTypeName );
  nTypeName	= MIN ( nTypeName, nBuffer );
  strncpy ( pszBuffer, pszTypeName, nTypeName );
  if ( nTypeName < nBuffer ) {
    pszBuffer [ nTypeName ]	= '\0';
  }
  RETURN ( nTypeName );
} EndFunction ( fnClientDbTypeTagName );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
