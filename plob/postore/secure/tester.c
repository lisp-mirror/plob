#include	<stdio.h>

#include "ValidateHost.c"

main()
{
	int	status ;

	status = XXValidHost() ;
	if ( status < 0 )
	{
		printf( "This host is not authorised to use the stable store\n" ) ;
		exit( -1 ) ;
	}
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
