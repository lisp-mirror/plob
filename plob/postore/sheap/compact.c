#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<sys/types.h>
#include	<sys/mman.h>
#include "sstore.h"
#include "sheapif.h"
 
static void error( const char *s )
{
  printf( "compact: %s\n",s ) ;
  exit( -1 ) ;
}
 
static void save(void)
{
}
 
static void restore(void)
{
}
 
static void write_fault( psint svmaddr, psint * context )
{
  error( "unexpected write fault." ) ;
}

static void page_fault( int sig, int code, int*scp,char *addr )
{
  error( "unexpected page fault." ) ;
}

main( int argc,char ** argv )
{
  /* open the stable store */
  SS_compact( argv[ 1 ],error,save,restore,
	      (PFNUSERWRITEFAULT) write_fault,
	      (PFNUSERPAGEFAULT) page_fault );
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
