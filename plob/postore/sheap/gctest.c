#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include "sstore.h"
#include "sheapif.h"

static void error( const char *s )
{
  printf( "sheapgc: %s\n",s ) ;
  exit( -1 ) ;
}

static void save(void)
{
}

static void restore(void)
{
}

static void stabilise(psint * pContext)
{
  error( "had to perform an unexpected stabilise()" ) ;
}

static void print_store_stats( stats )
struct stableheap_statistics *stats ;
{
  printf( "store size : %10d bytes\n",stats->maximum_space ) ;
  printf( "data size  : %10d bytes\n",stats->allocated_space ) ;
  printf( "free space : %10d bytes\n",stats->unallocated_space ) ;
  printf( "index size : %10d objects\n",stats->number_of_objects ) ;

  printf( "the store is %5.1f%% data\n",
	  ( ( double ) stats->allocated_space * ( double ) 100 ) / ( double ) stats->maximum_space ) ;
  printf( "             %5.1f%% free space\n",
	  ( ( double ) stats->unallocated_space * ( double ) 100 ) / ( double ) stats->maximum_space ) ;
  printf( "             %5.1f%% index table\n",
	  ( ( double ) stats->number_of_objects * ( double ) 400 ) / ( double ) stats->maximum_space ) ;
}

main( argc,argv )
int argc ; char **argv ;
{
  if ( argc != 2 )
    {
      error( "directory name for stable heap not specified" ) ;
    } else
      {
	int i,j ;
	psint sheap ;
	struct stableheap_statistics stats ;

	sheap = SH_open( argv[1],error,save,restore,stabilise ) ;
	if ( sheap == ( psint ) 0 )
	  error( "failed to access the stable heap" ) ;

	for ( i = 0 ; i < 10000; i++ )
	  {
	    j = SH_create_object( ( i % 21 ) * 3 + 4 ) ;
#if 1
	    if ( i > 0 && i % 1024 == 0 )
	      {
		printf( "\ngc at i = %d, objid %d \n"
			"statistics before gc:\n",
			i, j );
		SH_statistics( &stats ) ;
		print_store_stats( &stats ) ;
		SH_garbage_collect() ;
		printf( "statistics after  gc:\n" );
		SH_statistics( &stats ) ;
		print_store_stats( &stats ) ;
	      } else
		{
		  /*
		  printf( "new obj at %d\r",j ) ;
		  */
		}
#endif
	  }

	SH_stabilise ();
	SH_close();
      }
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
