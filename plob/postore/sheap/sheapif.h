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
extern psint SH_get_restart_clock PROTO((void));
extern void SH_set_restart_clock PROTO((psint tim));
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
