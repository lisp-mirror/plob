/*******************************************************/
/*                                                     */
/* This is a stand alone program that can be used to   */
/* create an instance of the stable store.	       */
/* It is used as follows:                              */
/*        mksstore <path-name>			       */
/* The size of a disk block is found from the file     */
/* "sstore.h".                                         */
/*                                                     */
/*  created   1/ 6/91     DSM			       */
/*  modified 18/ 5/92     DSM			       */
/*  modified 20/ 6/94     GK  re-added host validation */
/*                                                     */
/*******************************************************/

#include <sys/file.h>		/* file locking macros */
#include <errno.h>		/* definitions of system error numbers */
#include	<stdio.h>
#include	<stdlib.h>

#include	<sys/types.h>
#include	<sys/mman.h>

#include "sstore.h"		/* interpreter definitions */

#if defined(Arch_sun5)		/* 1996/09/11 HK */
#include	<unistd.h>
#include	<siginfo.h>
#include	<ucontext.h>
#include	<fcntl.h>
#endif

/* security validation... */
#include "../secure/ValidateHost.c"

/* system variable that holds error numbers */
/* 1998/02/17 HK: */
/* extern int errno ; */

static void fnCreateError ( const char * pszMessage )
{
  puts ( pszMessage );
  exit ( -1 );
}

int main( int argc, char * argv [] )
{
  int lfd,fd,status ;
  psint len,slen,maxslen ;
  char sfname[ 256 ] ;
  char lfname[ 256 ] ;
  char hostname[ 64 ] ;
  /* root page structure used in the store file */
  root_pg	root_page ;

  /* 1998/02/17 HK: Not used anywhere: */
  /* struct flock file_lock ; */

  /* 1998/02/18 HK: Added Stable Store initialization. */
  INITIALIZE_SSTORE ();

  status = XXValidHost() ;
  if ( status < 0 )
    {
      printf( "This host is not authorised to use the stable store\n" ) ;
      exit( -1 ) ;
    }

  if ( argc != 2 )
    {
      printf( "usage: sstoreformat <path-name>\n" ) ;
      exit( -1 ) ;
    }

  SS_create_database ( argv [ 1 ], fnCreateError );
  printf( "sstoreformat:"
	  " %s has been successfully initialised as a stable store\n",
	  argv[ 1 ] ) ;
  return 0;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
