/**
 * 1998/02/18 HK: Created.
 */

#include	<stdio.h>

#include	"u2win32.h"
#include	"unistd.h"

/* ------------------------------------------------------------ */
#define	INITIALIZE_MODULE()	\
((__bInitializeModule__)?fnInitializeUnistdModule(),TRUE:FALSE)

/* ------------------------------------------------------------ */
static BOOL	__bInitializeModule__	= TRUE;

/* ------------------------------------------------------------ */
static void	fnInitializeUnistdModule	()
{
  if ( __bInitializeModule__ ) {
    __bInitializeModule__	= FALSE;
  }
}

/* ------------------------------------------------------------ */
size_t	fnGetpagesize	( void )
{
  static size_t	nPagesize	= -1;
  SYSTEM_INFO	SystemInfo;

  INITIALIZE_MODULE ();

  if ( (int) nPagesize < 0 ) {
    GetSystemInfo ( &SystemInfo );
    /*
      nPagesize	= SystemInfo.dwPageSize;
    */
    nPagesize	= SystemInfo.dwAllocationGranularity;
  }
  return nPagesize;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
