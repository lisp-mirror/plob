#include	<stdio.h>
#include	"sstore.h"

extern	 psint *getPteAddr() ;
char	*disk_vaddr = ( char * )9000 ;

main()
{
	char	s[ 100 ] ;
	psint	p,q,r ;

	while ( p != -1 )
	{
		printf( "Enter a number :- " ) ;

		gets( s ) ;
		p = ( psint ) atoi( s ) ;

		q = getPtePteOffset( p ) ;
		
		printf( "p is %d ; q is %d\n",p,q ) ;
	}
}


/* Given a page return the address of its associated pte */
static psint *getPteAddr( p )
psint	p ;
{

	p >>= PAGEPWROF2 ;			/* Get relative pageNo. */
	p -= ( psint ) 2 ;			/* ignore 1st two pages */
	
	return( ( psint * )( ( char * ) disk_vaddr + ( ( psint ) 2 << PAGEPWROF2 ) ) - PT1PAGES + p ) ;
}

/* Given a page return its associated pte */
#define getPte( p )	( *getPteAddr( p ) )


/* Given a data page return the offset into pte array for this pages pte page */
static psint getPtePteOffset( p )
psint	p ;
{
	psint	*pteAddr ;

	pteAddr = getPteAddr( p ) ;

	p = ( psint ) ( ( char * ) pteAddr - ( char * ) disk_vaddr ) ;
	p >>= PAGEPWROF2 ;			/* Get relative pageNo. */
	return( p - ( psint ) 2 ) ;		/* ignore 1st two pages */
}


/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
