#if 1

#define XXValidHost()	0

#else

#include	<sys/syscall.h>
#include	<sys/errno.h>

extern char *crypt() ;

static int XXstrlen( a )
char *a ;
{
	int l ;

	l = 0 ;
	while( *a++ != ( char ) 0 ) l++ ;
	return( l ) ;
}

static int XXstrcmp( a,b )
char *a,*b ;
{
	int r ;

	r = 1 ;
	while( r && *a != ( char ) 0 )
		r = *a++ == *b++ ;
	return( r ) ;
}

static int XXread( f,b,n )
int f,n ; char *b ;
{
	int i ;

	while( n > 0 )
	{
		i = read( f,b,n ) ;
		if ( i < 0 )
		{
			extern int errno ;

			if( errno != EINTR )
			{
				printf( "error reading file of valid keys\n" ) ;
				exit( -1 ) ;
			}
		} else
		{
			if ( i == 0 )
			{
				if ( n != 12 )
				{
					printf( "unexpected eof reading file of valid keys\n" ) ;
					exit( -1 ) ;
				} else return( 0 ) ;
			} else
			{
				n -= i ;
				b += i ;
			}
		}
	}
	return( -1 ) ;
		
}

static char	*XXSencode( domain )
char *domain ;
{
	char	*crypted1,*crypted2 ;
	char	salt[ 3 ] ;
	int	i,len ;
	
	salt[ 2 ] = ( char ) 0 ;
	
	salt[ 1 ] = ( char ) 65 ;
	salt[ 0 ] = ( char ) 90 ;
	
	crypted1 = crypt( domain,salt ) ;

	len = XXstrlen( crypted1 ) ;
	for ( i = 0 ; i < len - 2 ; i++ ) crypted1[ i ] = crypted1[ i + 2 ] ;

	salt[ 1 ] = crypted1[ len - 2 ] ;
	salt[ 0 ] = crypted1[ len - 1 ] ;

	crypted1[ len - 2 ] = ( char ) 0 ;

	crypted2 = crypt( crypted1,salt ) ;

	len = XXstrlen( crypted2 ) ;
	for ( i = 0 ; i < len - 2 ; i++ ) crypted2[ i ] = crypted2[ i + 2 ] ;
	crypted2[ len - 2 ] = ( char ) 0 ;

	return	crypted2 ;
}

static char	*XXencode( id )
long id ;
{
	char	test[ 16 ] ;
	char	*crypted1,*crypted2 ;
	char	salt[ 3 ] ;
	int	i,len ;
	
	salt[ 2 ] = ( char ) 0 ;
	
	salt[ 1 ] = ( char ) 65 ;
	salt[ 0 ] = ( char ) 90 ;
	
	sprintf( test,"%ld",id ) ;

	crypted1 = crypt( test,salt ) ;

	len = XXstrlen( crypted1 ) ;
	for ( i = 0 ; i < len - 2 ; i++ ) crypted1[ i ] = crypted1[ i + 2 ] ;

	salt[ 1 ] = crypted1[ len - 2 ] ;
	salt[ 0 ] = crypted1[ len - 1 ] ;

	crypted1[ len - 2 ] = ( char ) 0 ;

	crypted2 = crypt( crypted1,salt ) ;

	len = XXstrlen( crypted2 ) ;
	for ( i = 0 ; i < len - 2 ; i++ ) crypted2[ i ] = crypted2[ i + 2 ] ;
	crypted2[ len - 2 ] = ( char ) 0 ;

	return	crypted2 ;
}

static int XXGetThis( scn )
int scn ;
{
	return( syscall( scn ) ) ;
}

static int XXValidHost()
{
	int	ok,fd,found = 0,gotdomainname = 0 ;
	char	line[16],*encrypted,*fname,domain[ 1024 ] ;
	extern char *getenv() ;
	
	/* MICK */
	return(0);
	/* END MICK */
	
	fname = getenv( "PSKEYS" ) ;
	if ( fname == ( char * ) 0 )
	{
		printf( "Cannot find the environment variable PSKEYS - Sorry\n" ) ;
		exit( -1 ) ;
	}
	
	fd = open( fname,0 ) ;
	if ( fd < 0 )
	{
		printf( "Cannot open the file of valid keys\n" ) ;
		exit( -1 ) ;
	}

	if ( getdomainname( domain,1023 ) == 0 )	/* read the NIS domain name */
	{
		domain[ 1023 ] = ( char ) 0 ;			/* make sure its null terminated */
		encrypted = XXSencode( domain ) ;
	
		if ( XXstrlen( encrypted ) != 11 )
		{
			printf( "Internal error in XXValidHost\n" ) ;
			exit( -1 ) ;
		}

		ok = 1 ;
		while ( ok )
		{
			ok = XXread( fd,line,12 ) ;
			if ( ok )
			{
				line[ 12 ] = ( char ) 0 ;
				if ( XXstrcmp( encrypted,line ) ) found = 1 ;
			}
		}
	}
	
	close( fd ) ;
	
	if ( found ) return 0 ;
	else
	{
		fd = open( fname,0 ) ;
		if ( fd < 0 )
		{
			printf( "Cannot open the file of valid keys\n" ) ;
			exit( -1 ) ;
		}

		/* MICK */
		/* encrypted = XXencode( XXGetThis( SYS_gethostid ) ) ; */
		/* END MICK */
	
		if ( XXstrlen( encrypted ) != 11 )
		{
			printf( "Internal error in XXValidHost\n" ) ;
			exit( -1 ) ;
		}
	
		ok = 1 ;
		while ( ok )
		{
			ok = XXread( fd,line,12 ) ;
			if ( ok )
			{
				line[ 12 ] = ( char ) 0 ;
				if ( XXstrcmp( encrypted,line ) ) found = 1 ;
			}
		}
		if ( found ) return 0 ; else return -1 ;
	}
}

#endif

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
