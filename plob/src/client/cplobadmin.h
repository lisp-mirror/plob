/* -------------------------------------------------------------------------
| Module	cplobadmin.h
| Author	Heiko Kirschke
| Copyright	(C) 1998 Heiko Kirschke
| Date		1998/02/04
| Description	PLOB administration functions.
 ------------------------------------------------------------------------- */

#include	<rpc/rpc.h>
#include	<rpc/auth.h>
#if WIN32
#include	<rpc/auth_uni.h>
#else
#if LINUX
#include	<rpc/auth_unix.h>
#elif !HPUX
#include	<rpc/auth_sys.h>
#endif /* LINUX */
#include	<rpc/auth_des.h>
#endif /* WIN32 */

#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif

#include	"plobadmin.h"

/* -------------------------------------------------------------------------
| Types
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
AUTH *	fnCreateAuth	( LPCSTR	pszServer,
			  LPINT		pnAuth );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
