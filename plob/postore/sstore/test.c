#include	<stdio.h>
#include	"sstore.h"
main()
{
	int	s ;
	root_pg rp ;


	printf( "%d\n",sizeof( root_pg ) ) ;
	
	print_it( PT1PAGES ) ;
	print_it( PT2SIZE ) ;
	print_it( LEFTOVERS ) ;
	print_it( VASPACESIZE ) ;
	print_it( VASPACEPAGES ) ;
	print_it( DATABLOCKSTART ) ;
}

print_it( d )
int	d ;
{
	printf( "%d\n",d ) ;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
