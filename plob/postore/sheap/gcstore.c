#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<sys/types.h>
#include	<sys/mman.h>
#include "sstore.h"
#include "sheapif.h"

static void error(const char *s)
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

static void print_store_stats( struct stableheap_statistics * stats )
{
	psint sum_allocated ;
	double convert_to_percentage,mega ;

	sum_allocated = stats->allocated_space + stats->unallocated_space +
			stats->unused_allocated_space + stats->allocated_management_space ;
	convert_to_percentage = ( double ) 100 / ( double ) sum_allocated ;
	mega = ( double )( 1 << 20 ) ;

	printf( "maximum space    : %8.3f Mbytes\n",( double ) stats->maximum_space / mega ) ;
	printf( "allocated space  : %8.3f Mbytes (%5.1f%%)\n",( double ) stats->allocated_space / mega,
						( double ) stats->allocated_space * convert_to_percentage ) ;
	printf( "unallocated space: %8.3f Mbytes (%5.1f%%)\n",( double ) stats->unallocated_space / mega,
						( double ) stats->unallocated_space * convert_to_percentage )  ;
	printf( "unused space     : %8.3f Mbytes (%5.1f%%)\n",( double ) stats->unused_allocated_space / mega,
						( double ) stats->unused_allocated_space * convert_to_percentage ) ; 
	printf( "management space : %8.3f Mbytes (%5.1f%%)\n",( double ) stats->allocated_management_space / mega,
						( double ) stats->allocated_management_space * convert_to_percentage )  ;
	printf( "number of objects: %8d objects\n",stats->number_of_objects ) ;
}

main( int argc,char ** argv )
{
	if ( argc != 2 )
	{
		error( "directory name for stable heap not specified" ) ;
	} else
	{
		psint sheap ;
		struct stableheap_statistics stats ;

		sheap = SH_open( argv[1],error,save,restore,stabilise ) ;
		if ( sheap == ( psint ) 0 )
			error( "failed to access the stable heap" ) ;
		SH_full_garbage_collect() ;
		SH_stabilise() ;
		SH_statistics( &stats ) ;
		print_store_stats( &stats ) ;
		SH_close();
	}
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
