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

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
