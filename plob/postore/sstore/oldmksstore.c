/*******************************************************/
/*                                                     */
/* This is a stand alone program that can be used to   */
/* create an instance of the stable store.	       */
/* It is used as follows:                              */
/*        mksstore <path-name>			       */
/* The size of a disk block is found from the file     */
/* "sstore.h".                                         */
/*                                                     */
/*  created  1/ 6/91     DSM			       */
/*                                                     */
/*******************************************************/

#include "sstore.h"					/* interpreter definitions */
#include <sys/file.h>					/* file locking macros */
#include <errno.h>					/* definitions of system error numbers */

extern int errno ;					/* system variable that holds error numbers */
root_pg	root_page ;					/* root page structure used in the store file */

fill_root_page()				/* fill in the root page for the given store size */
{
	psint i,plen ;
	char *s ;

	s = ( char * )( &root_page ) ;				/* initialise the root page to 0s */
	for( i = ( psint ) 0 ; i < BPAGESIZE ; i++ )
		s[ i ] = ( char ) 0 ;

	root_page.sstoreMagic = SSTOREMAGIC ;			/* magic number for stable store */
	root_page.sheapMagic = ( psint ) -1 ;			/* magic number for stable heap - unset value */
	root_page.page_size = BPAGESIZE ;			/* page size in the store file */
	root_page.store_length = DATASPACESIZE ;		/* length of User VA space in bytes */
}

main( argc,argv )
int argc ; char **argv ;
{
	int lfd,fd,status ;
	psint len,slen,maxslen ;
	char sfname[ 81920 ] ;
	char lfname[ 81920 ] ;
	char hostname[ 64 ] ;
	struct flock file_lock ;

	if ( argc != 2 )
	{
		printf( "usage: sstoreformat <path-name>\n" ) ;
		exit( -1 ) ;
	}

	strcpy( lfname,argv[ 1 ] ) ;
	strcat( lfname,LOCKSUFFIX ) ;
	umask( 0 ) ;						/* set all mode bits during a create */
	lfd = open( lfname,O_RDWR | O_SYNC | O_CREAT | O_EXCL,0666 ) ;
	if ( lfd < 0 )
	{
		if ( errno == EEXIST )
		{
			printf( "sstoreformat: the directory %s may already contain an object store\n",argv[ 1 ] ) ;
		} else
		{
			printf( "sstoreformat: failed to create the lockfile %s errno:%d\n",lfname,errno ) ;
		}
		exit( -1 ) ;
	}
	if ( flock( lfd,LOCK_EX | LOCK_NB ) )
	{
		printf( "sstoreformat: cannot lock the store" ) ;
		exit( -1 ) ;
	}
	gethostname( hostname,64 ) ;			/* get our hostname */
	hostname[ 63 ] = ( char ) 0 ;			/* make sure its null terminated */
	if ( write( lfd,hostname,64 ) != 64 )
	{
		printf( "sstoreformat: write error occurred while creating the lockfile: %d\n",errno ) ;
		exit( -1 ) ;
	}

	strcpy( sfname,argv[ 1 ] ) ;
	strcat( sfname,STORESUFFIX ) ;

	umask( 0 ) ;			/* set all mode bits during a create */
	fd = open( sfname,O_RDWR | O_SYNC | O_CREAT | O_EXCL,0666 ) ;
	if ( fd < 0 )
	{
		if ( errno == EEXIST )
		{
			printf( "sstoreformat: the directory %s may already contain an object store\n",argv[ 1 ] ) ;
		} else
		{
			printf( "sstoreformat: failed to create the lockfile %s errno:%d\n",sfname,errno ) ;
		}
		exit( -1 ) ;
	}
	fill_root_page() ;
	if ( write( fd,( char * )( &root_page ),( int )( BPAGESIZE ) ) != ( int )( BPAGESIZE ) ||
	     write( fd,( char * )( &root_page ),( int )( BPAGESIZE ) ) != ( int )( BPAGESIZE ) )
	{
		printf( "sstoreformat: failed to write root pages to %s errno %d\n",sfname,errno ) ;
		exit( -1 ) ;
	}

	flock( lfd,LOCK_UN ) ;
	close( lfd ) ;
	close( fd ) ;
	printf( "sstoreformat: %s has been successfully initialised as a stable store\n",argv[ 1 ] ) ;
}
