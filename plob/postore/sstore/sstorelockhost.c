/********************************************************/
/*                                                    	*/
/*  Program to safely rename the lockhost on which all  */
/*  use of flock mustbe made. This obviates the need to */
/*  rely on SunOS network locking - which doesnt always */
/*  work anyway....                                     */
/*                                                    	*/
/* created 25/10/91    AB                               */
/*                                                    	*/
/********************************************************/

#include	"sys/file.h"		/* file locking macros */
#include	"signal.h"
#include	"sys/types.h"
#include	<sys/mman.h>
#include	<errno.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>

#include	"sstore.h"		/* stable store definitions */

#if defined(Arch_sun5)		/* 1996/09/11 HK */

#include	<unistd.h>
#include	<siginfo.h>
#include	<ucontext.h>
#include	<fcntl.h>

#endif

#if ! defined(F_ULOCK)
#define   F_ULOCK   0   /* Unlock a previously locked section */
#endif
#if ! defined(F_LOCK)
#define   F_LOCK    1   /* Lock a section for exclusive use */
#endif
#if ! defined(F_TLOCK)
#define   F_TLOCK   2   /* Test and lock a section for exclusive use */
#endif
#if ! defined(F_TEST)
#define   F_TEST    3   /* Test section for other processes locks */      
#endif

/* extern char *strcpy() ; */
/* extern char *strcat() ; */

static char	storefname[ 81920 ] ;		/* char * to hold the name of stable store file */
static int	pstorefd ;			/* fd for the psstore */
static psint	store_length ;			/* Size of the store file */
static int	lockfd ;
static char	lockfname[ 81920 ] ;		/* char * to hold the name of the lock file */
static char	lockhost[ 64 ] ;		/* char * to hold the name of the host on which we must be */
static char	thishost[ 64 ] ;		/* char * to hold the name of the host we're on */

static void error( char * s )			/* error handling code */
{
	printf( "renamelockhost: %s\n",s ) ;
	exit( -1 ) ;
}

void main( int argc,char ** argv )
{
	int	status ;
	caddr_t	addr ;
	/* 1998/02/17 HK:
	   extern int errno ;
	*/

	if ( argc != 3 )
	{
		error( "usage: renamelockhost <path-name> <host-name>" ) ;
	}

	strcpy( storefname,argv[ 1 ] ) ;
	strcat( storefname,STORESUFFIX ) ;
						/* open the pstore */
	pstorefd = open( storefname,O_RDWR ) ;
	if ( pstorefd < 0 )
	{
		error( "cannot open the stable store" ) ;
	}

						/* how long is the actual store file */
	store_length = ( psint ) lseek( pstorefd,0,2 ) ;
	lseek( pstorefd,0,0 ) ;			/* reset file pos to the start */
	if ( store_length < ( psint ) 0 )
	{
		error( "unable to determine the length of the stable store" ) ;
	}
	if ( store_length & PAGEINDEX )
	{
		error( "the length of the store file is not a multiple number of virtual pages" ) ;
	}

						/* lock the pstore */
	strcpy( lockfname,argv[ 1 ] ) ;
	strcat( lockfname,LOCKSUFFIX ) ;
	lockfd = open( lockfname,2 ) ;
	if ( lockfd < 0 )
	{
		error( "cannot open the store's lock file" ) ;
	}

#if defined(Arch_sun4)	/* 1996/09/11 HK */
	if ( flock( lockfd,LOCK_EX | LOCK_NB ) )
	{
		if ( errno == EWOULDBLOCK )
		{
			error( "the store is locked by another process" ) ;
		} else
		{
			error( "cannot lock the store" ) ;
		}
	}
#else
	if ( lockf ( lockfd, F_TLOCK, 0L ) )
	{
		if ( errno == EAGAIN )
		{
			error( "the store is locked by another process" ) ;
		} else
		{
			error( "cannot lock the store" ) ;
		}
	}
#endif
				/* get the name of this host - not more than 63 chars */
	gethostname( thishost,64 ) ; thishost[ 63 ] = ( char ) 0 ;
				/* get the name of the host we should be on - not more than 63 chars */
	if ( read( lockfd,lockhost,64 ) < 0 )
	{
		error( "failed to read the name of the existing lockhost" ) ;
	}
	lockhost[ 63 ] = ( char ) 0 ;
	if ( strcmp( lockhost,thishost ) )	/* give up if the two names dont match */
	{
		char buff[ 512 ] ;

		sprintf( buff,"system must be run on host '%s', not '%s'",lockhost,thishost ) ;
		error( buff ) ;
	}

						/* Zap the local machines cached view of the stable store */
	addr = (caddr_t) mmap( 0,store_length,
			       PROT_READ|PROT_EXEC,MAP_SHARED,
			       pstorefd,( off_t ) 0 ) ;
	if ( addr == ( caddr_t ) -1 )
	{
		error( "failed to memory map the stable store file" ) ;
	}
	if ( msync( addr,store_length,MS_INVALIDATE ) != 0 )
	{
		error( "failed to flush the locally cached state of the object store" ) ;
	}
	munmap( addr,store_length ) ;
	close( pstorefd ) ;			/* close the stable store file */

						/* rename the host on which locks can be made */
	strcpy( lockhost,argv[ 2 ] ) ;
	lockhost[ 63 ] = ( char ) 0 ;
	if ( lseek( lockfd,0,0 ) != 0 || write( lockfd,lockhost,64 ) != 64 )
	{
		error( "failed to write the name of the new lockhost" ) ;
	}
#if defined(Arch_sun4)	/* 1996/09/11 HK */
	flock( lockfd,LOCK_UN ) ;
#else
	lockf( lockfd,F_ULOCK,0) ;
#endif
	close( lockfd ) ;			/* close the lock file */
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
