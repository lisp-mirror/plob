#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<sys/types.h>
#include	<sys/mman.h>
#include "sstore.h"
#include "sheapif.h"

static void error( const char *s)
{
	printf( "sheapstats: %s\n",s ) ;
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

static void print_store_config( struct stableheap_configuration * config )
{
	if ( config->configuration_flags != ( psint ) 0 )
	{
		printf( "The following configuration details are specified:\n" ) ;
		if ( config->configuration_flags & KEY_TO_ADDR ) printf( "KEY_TO_ADDR\n" ) ;
		if ( config->configuration_flags & KEY_TO_ADDR_FAILS ) printf( "KEY_TO_ADDR_FAILS\n" ) ;
		if ( config->configuration_flags & KEYS_FIXED_FOR_LIFE ) printf( "KEYS_FIXED_FOR_LIFE\n" ) ;
		if ( config->configuration_flags & NO_KEY_MAPPING ) printf( "NO_KEY_MAPPING\n" ) ;
		if ( config->configuration_flags & KEYS_ARE_ADDRS ) printf( "KEYS_ARE_ADDRS\n" ) ;
		if ( config->configuration_flags & KEYS_ARE_INDIRECT ) printf( "KEYS_ARE_INDIRECT\n" ) ;
		if ( config->configuration_flags & REQUEST_STABILISE ) printf( "REQUEST_STABILISE\n" ) ;
		if ( config->configuration_flags & INCREMENTAL_GC ) printf( "INCREMENTAL_GC\n" ) ;
		if ( config->configuration_flags & KEY_RANGE )
		{
			printf( "KEY_RANGE\n" ) ;
			printf( "minimum key  : %8x\n",config->minimum_key ) ;
			printf( "maximum key  : %8x\n",config->maximum_key ) ;
			printf( "key alignment: %8x\n",config->key_alignment ) ;
		}
	}
}

main( int argc, char **argv )
{
  if ( argc != 2 )
    {
      error( "directory name for stable heap not specified" ) ;
    } else
      {
	psint sheap ;
	struct stableheap_statistics stats ;
	struct stableheap_configuration config ;

	sheap = SH_open( argv[1],error,save,restore,stabilise ) ;
	if ( sheap == ( psint ) 0 )
	  error( "failed to access the stable heap interface" ) ;
	SH_statistics( &stats ) ;
	print_store_stats( &stats ) ;
	SH_configuration( &config ) ;
	print_store_config( &config ) ;
	SH_close();
      }
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
