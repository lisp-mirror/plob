#include	<sys/types.h>
#include	<stdio.h>
#include	<unistd.h>
#include	<fcntl.h>
main()
{
	int	f,res ;

	f = open( "WOOO",O_WRONLY) ;
	res = write( f,"abcdef",6 ) ;
	printf( "Result %d\n",res ) ;
	res = lseek( f,500*1024*1024,SEEK_SET ) ;
	printf( "Result %d\n",res ) ;
	res = write( f,"abcdef",6 ) ;
	printf( "Result %d\n",res ) ;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
