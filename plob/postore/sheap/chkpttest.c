#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include "sstore.h"
#include "sheapif.h"


void error( const char *s)
{
	printf( "OOPS: %s\n",s ) ;
	exit( -1 ) ;
}

void save(void)
{
}

void restore(void)
{
}

static void wfault()
{
	error( "found an unexpected write fault" ) ;
}

main( argc,argv )
int argc ; char **argv ;
{
	if ( argc != 2 )
	{
		error( "usage: checkpoint test 'dirname'" ) ;
	} else
	{
		psint i,sstore ;

		sstore = SS_open( argv[1],error,save,restore,wfault ) ;
		if ( sstore == ( psint ) 0 )
			error( "cannot access the stable store" ) ;
		for ( i = ( psint ) 0 ; i < ( psint ) 100 ; i++ )
		{
			SS_stabilise() ;
		}
		SS_close();
	}
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
