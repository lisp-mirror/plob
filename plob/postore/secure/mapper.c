#include "stdio.h"
#include "ValidateHost.c"

main()
{
	int next ;
	char line[ 8192 ] ;

	next = getchar() ;
	while ( next != EOF )
	{
		int nchars = 0 ;

		while ( next != EOF && next != '\n' )	/* read the next line - discard the newline */
		{
			line[ nchars++ ] = ( char ) next ;
			next = getchar() ;
		}
		next = getchar() ;			/* to init the next loop - discards '\n' */
		line[ nchars ] = ( char ) 0 ;		/* null terminate */
		while( nchars-- > 0 )			/* null out any spaces */
		{
			if ( line[ nchars ] == ' ' || line[ nchars ] == '\t' ) line[ nchars ] = ( char ) 0 ;
		}
#ifdef	NIS
		printf( "%11s\n",XXSencode( line ) ) ;	/* map the 1st null terminated string on the line */
#else
		printf( "%11s\n",XXencode( strtol( line,( char ** ) 0,16 ) ) ) ;	/* map the 1st null terminated string on the line */
#endif	NIS
	}
	return( 0 ) ;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
