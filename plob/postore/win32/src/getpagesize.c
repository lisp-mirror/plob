/**
 * 2002-04-16 Heiko Kirschke. Echo the page size of this system.
 */
#include	<windows.h>
#include	<errno.h>

/* ------------------------------------------------------------ */
void	main	( int		argc,
		  char *	argv [] )
{
  SYSTEM_INFO	SystemInfo;
  GetSystemInfo ( &SystemInfo );
  printf ( "page size is %lu = 0x%lX\n",
	   SystemInfo.dwAllocationGranularity,
	   SystemInfo.dwAllocationGranularity );
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
