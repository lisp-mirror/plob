/* -------------------------------------------------------------------------
| Module	cplobff.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1997 Heiko Kirschke
| Date		1997/03/11
| Description	PLOB source code for Allegro foreign function relocation.
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobff.h"

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

/* ----------------------------------------------------------------------- */
void		fnLISPserrorCallback	( ERRLVL	eLevel,
					  LPCSTR	pszProcedure,
					  LPCSTR	pszErrorMsg )
{
  char	szBuffer [ 2048 ];

  PROCEDURE	( fnLISPserrorCallback );
  INITIALIZEPLOB;

  sprintf ( szBuffer,
	    "%s from server at executing %s:\n"
	    "       %s",
	    ppszErrorLevel2String [ (ERRORLEVEL) eLevel - errMin ],
	    pszProcedure, pszErrorMsg );
  if ( eLevel == errLvlCError ) {
    eLevel	= errLvlWarn;
  }
  fnLISPerrorCallback ( eLevel, "Acknowledged.", szBuffer );

  RETURN ( VOID );
} /* fnLISPserrorCallback */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
