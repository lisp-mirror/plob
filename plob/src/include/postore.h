 typedef int psint; 
/********************************************************************/
/*                                                                  */
/* INCLUDE FILE FOR THE STABLE STORE IF                             */
/*                                                                  */
/* created  17/11/89      ALB                                       */
/* modified  3/ 4/90      ALB revised interfaces                    */
/* modified 26/ 4/90      ALB revised interfaces - added SS_ prefix */
/*                            for restart clock & ".il" files       */
/* mod      25/05/92      ALB revised interfaces                    */
/*                                                                  */
/********************************************************************/

#define	REAL_ADDRESS		( ( psint ) 0x01 )
#define	SHADOW_PAGING		( ( psint ) 0x02 )
#define	UNLIMITED_SHADOW	( ( psint ) 0x04 )

struct stablestore_configuration
{
	psint configuration_flags ;
	psint page_size ;
} ;

struct stablestore_statistics
{
	psint dummy ;
} ;

#if defined(NOPROTOTYPES)
#define	PROTO( args ) ()
#else
#define	PROTO( args ) args
#endif

typedef void ( * PFNVOID ) PROTO(( void ));
typedef void ( * PFNUSERERROR ) PROTO(( const char * ));
typedef void ( * PFNUSERSTABILISE ) PROTO(( psint * ));
typedef void ( * PFNUSERWRITEFAULT ) PROTO(( psint, psint * ));
typedef void ( * PFNUSERPAGEFAULT ) PROTO(( int, int, int, char * ));

extern void  SS_close PROTO((void));
extern void  SS_stabilise PROTO((void));
extern psint * SS_real_address PROTO((psint pos));
extern psint  SS_read_word PROTO((psint pos));
extern void  SS_read_words PROTO((psint pos,psint *buffer,psint nwords));
extern void  SS_write_word PROTO((psint pos,psint theword));
extern void  SS_write_words PROTO((psint pos,psint *buffer,psint nwords));
extern psint  SS_first_address PROTO((void));
extern psint  SS_last_address PROTO((void));
extern void  SS_set_sheap_version PROTO((psint vnum));
extern psint  SS_get_sheap_version PROTO((void));
extern void  SS_set_restart_clock PROTO((psint tim));
extern psint  SS_get_restart_clock PROTO((void));
extern void  SS_configuration PROTO((struct stablestore_configuration *config));
extern void  SS_statistics PROTO((struct stablestore_statistics *stats));
extern psint  SS_saveVM PROTO((psint a1,psint a2));
extern psint  SS_scratchVM PROTO((psint a1,psint a2));
extern psint  SS_reserveVM PROTO((psint a1,psint a2));
extern void  SS_dontneedVM PROTO((psint a1,psint a2));
extern psint  SS_open PROTO(( const char *dirname,
			      PFNUSERERROR error,
			      PFNVOID save,
			      PFNVOID restore,
			      PFNUSERWRITEFAULT write_fault,
			      PFNUSERPAGEFAULT page_fault ));
extern psint  SS_open_at_addr PROTO(( const char *dirname,
				      PFNUSERERROR error,
				      PFNVOID save,
				      PFNVOID restore,
				      PFNUSERWRITEFAULT write_fault,
				      PFNUSERPAGEFAULT page_fault,
				      caddr_t min_addr ));
/* only available via '.il' library: */
extern psint  SS_Xmemlock PROTO((psint *addr));
extern psint  SS_set_lock PROTO((psint pos));
extern psint  SS_compact PROTO((const char *dirname,
				PFNUSERERROR error,
				PFNVOID save,
				PFNVOID restore,
				PFNUSERWRITEFAULT write_fault,
				PFNUSERPAGEFAULT page_fault ));
extern void  SS_initialize PROTO((void));

/* 1998/02/26 HK: Added SS_create_database: */
extern psint	SS_create_database	PROTO(( const char	* pszDatabase,
						PFNUSERERROR	pfnError));

/* 1998/02/20 HK: Added Stable Store initialization */
extern psint	__bInitializeSStore__	/* = 1 */;
extern void	SS_initialize PROTO((void));

/* 1998/07/07 HK: Added SS_set_signal_handler() for re-establishing
   the current signal handler. */
extern void	SS_set_signal_handler PROTO((void));

/* 1998/02/18 HK: Some macros necessary for Windows/NT. */

/* Put macro INITIALIZE_SSTORE () somewhere in your code before using
   any network function, including calls to gethostname(). This call
   will initialize the Windows/NT socket library. */

#define	INITIALIZE_SSTORE()	\
((__bInitializeSStore__)?(SS_initialize(),1):0)

#if defined(_WIN32)||defined(WIN32)

#include	<win32sig.h>

#else

#if ! defined(TRY_EXCEPTION)
#define TRY_EXCEPTION	{
#endif
#if ! defined(CATCH_EXCEPTION)
#define CATCH_EXCEPTION	}
#endif

#endif
/*******************************************************************/
/*                                                                 */
/*  INCLUDE FILE FOR THE STABLE HEAP IF - marcos et al.            */
/*                                                                 */
/* created   4/12/89      ALB                                      */
/* modified  3/ 4/90      ALB  revised the interfaces              */
/* modified 26/ 7/90      ALB  added configuration stuff           */
/*                                                                 */
/*******************************************************************/

#define	KEY_TO_ADDR		( ( psint ) 0x01 )
#define	KEY_TO_ADDR_FAILS	( ( psint ) 0x02 )
#define	KEYS_FIXED_FOR_LIFE	( ( psint ) 0x04 )
#define	KEY_RANGE		( ( psint ) 0x08 )
#define	NO_KEY_MAPPING		( ( psint ) 0x10 )
#define	KEYS_ARE_ADDRS		( ( psint ) 0x20 )
#define	KEYS_ARE_INDIRECT	( ( psint ) 0x40 )
#define REQUEST_STABILISE	( ( psint ) 0x80 )
#define	INCREMENTAL_GC		( ( psint ) 0x100 )

struct stableheap_configuration
{
	psint	configuration_flags ;
	psint	minimum_key ;
	psint	maximum_key ;
	psint	key_alignment ;
} ;

struct stableheap_statistics
{
	psint	maximum_space ;
	psint	allocated_space ;
	psint	unallocated_space ;
	psint	unused_allocated_space ;
	psint	allocated_management_space ;
	psint	number_of_objects ;
} ;

extern void SH_close PROTO((void));
extern void SH_stabilise PROTO((void));
extern psint *SH_key_to_address PROTO((psint key));
extern psint SH_create_object PROTO((psint size));
extern void SH_destroy_object PROTO((psint key));
extern void SH_garbage_collect PROTO((void));
extern void SH_full_garbage_collect PROTO((void));
extern void SH_incremental_garbage_collect PROTO((void));
extern psint SH_first_object PROTO((void));
extern void SH_read_words PROTO((psint key,psint index,psint *buffer,psint nwords));
extern void SH_write_words PROTO((psint key,psint index,psint *buffer,psint nwords));
extern psint SH_read_byte PROTO((psint key,psint index));
extern void SH_write_byte PROTO((psint key,psint index,unsigned char thebyte));
extern psint SH_read_word PROTO((psint key,psint index));
extern void SH_write_word PROTO((psint key,psint index,psint theword));
extern psint SH_read_key PROTO((psint key,psint index));
extern void SH_write_key PROTO((psint key,psint index,psint theptr));
extern psint SH_can_modify PROTO((psint key));
extern void SH_configuration PROTO((struct stableheap_configuration *config));
extern void SH_statistics PROTO((struct stableheap_statistics *stats));
extern psint fnClientDbRestartClockGet PROTO((void));
extern void fnClientDbRestartClockSet PROTO((psint tim));
extern psint SH_open PROTO(( const char *dirname,
			     PFNUSERERROR error,
			     PFNVOID save,
			     PFNVOID restore,
			     PFNUSERSTABILISE stabilise ));
extern psint SH_open_at_addr PROTO(( const char *dirname,
				     PFNUSERERROR error,
				     PFNVOID save,
				     PFNVOID restore,
				     PFNUSERSTABILISE stabilise,
				     caddr_t min_addr ));
extern psint SH_read_lock PROTO((psint key));
extern psint SH_set_lock PROTO((psint key));
extern void SH_write_lock PROTO((psint key,psint loc));

/* 1998/02/26 HK: Added SH_create_database: */
extern psint	SH_create_database	PROTO(( const char	* pszDatabase,
						PFNUSERERROR	pfnError ));

/* 1998/07/07 HK: Added SH_set_signal_handler() for re-establishing
   the current signal handler. */
extern void	SH_set_signal_handler PROTO((void));

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
