/********************************************************/
/*                                                    	*/
/*            PINT POT LAYERS 2 and 3                 	*/
/*                                                    	*/
/* LAYER 2 - PHYSICAL STORAGE                         	*/
/* LAYER 3 - SHADOW PAGED STABLE STORAGE              	*/
/*                                                    	*/
/* This module exports the functions SS_close(),	*/
/* SS_dontneedVM(),SS_first_address(),			*/
/* SS_get_restart_clock(),SS_get_sheap_version(),	*/
/* SS_last_address(),SS_open(),SS_read_word(),		*/
/* SS_read_words(),SS_real_address(),SS_reserveVM(),	*/
/* SS_saveonlyVM(),SS_scratchVM(),SS_set_lock(),	*/
/* SS_set_restart_clock(),SS_set_sheap_version(),	*/
/* SS_shadowVM(),SS_stabilise(),SS_write_word(),	*/
/* SS_write_words(),SS_Xmemlock()			*/
/*                                                    	*/
/* created  1/ 7/91    DSM                            	*/
/* mod     25/10/91    AB - flock used inplace of lockf */
/* mod      13/3/92    AB - bug fixes, file extending   */
/*                          writes can find disk full   */
/********************************************************/

#include "sstore.h"				/* stable store definitions */
#include "sys/file.h"				/* file locking macros */
#include "signal.h"
#include "sys/errno.h"
#include "sys/types.h"
#include "sys/mman.h"
#include "errno.h"

/******************************************************/
/* PINT POT LAYER 2 - access to the physical storage  */
/******************************************************/

/* variables required by layer2 of the pintpot - the physical storage */

static char	storefname[ 81920 ] ;		/* char * to hold the name of stable store file */
static int	pstorefd ;			/* fd for the psstore */
static psint	store_length ;			/* Size of the store file */
static int	lockfd ;
static char	lockfname[ 81920 ] ;		/* char * to hold the name of the lock file */
static char	lockhost[ 64 ] ;		/* char * to hold the name of the host on which we must be */
static char	thishost[ 64 ] ;		/* char * to hold the name of the host we're on */
static struct	flock file_lock ;
static int	file_flags ;			/* save status flags for the open store file */

						/* user provided interface variables */
static void ( *user_error )(),( *user_save )(),( *user_restore )(),( *user_write_fault )() ;

static void error( s )				/* error handling code */
char *s ;
{
	char buffer[ 1024 ] ;

	strcpy( buffer,"stable store: " ) ;
	strcat( buffer,s ) ;
	user_error( ( char * ) buffer ) ;
	SS_close() ;
	exit( -1 ) ;
}

static void open_disk( dirname )			/* lock then open the stable store */
char *dirname ;
{
	int	status ;
	extern int errno ;

	if ( dirname == ( char * ) 0 )
	{	
		error( "a null pathname was specified for the stable store" ) ;
	}

	strcpy( storefname,dirname ) ;
	strcat( storefname,STORESUFFIX ) ;
						/* open the pstore */
	pstorefd = open( storefname,O_RDWR ) ;
	if ( pstorefd < 0 )
		error( "cannot open the store" ) ;
	file_flags = fcntl( pstorefd,F_GETFL,0 ) ;	/* remember the status flags */

						/* how long is the actual store file */
	store_length = ( psint ) lseek( pstorefd,0,2 ) ;
	lseek( pstorefd,0,0 ) ;			/* reset file pos to the start */
	if ( store_length < ( psint ) 0 )
		error( "unable to determine the length of the stable store" ) ;
	if ( store_length & PAGEINDEX )
		error( "the length of the store file is not a multiple number of virtual pages" ) ;

						/* lock the pstore */
	strcpy( lockfname,dirname ) ;
	strcat( lockfname,LOCKSUFFIX ) ;
	lockfd = open( lockfname,2 ) ;
	if ( lockfd < 0 )
		error( "cannot open the store's lock file" ) ;

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
				/* get the name of this host - not more than 63 chars */
	gethostname( thishost,64 ) ; thishost[ 63 ] = ( char ) 0 ;
				/* get the name of the host we should be on - not more than 63 chars */
	read( lockfd,lockhost,64 ) ; lockhost[ 63 ] = ( char ) 0 ;
	if ( strcmp( lockhost,thishost ) )	/* give up if the two names dont match */
	{
		char buff[ 512 ] ;

		sprintf( buff,"system must be run on host '%s', not '%s'",lockhost,thishost ) ;
		error( buff ) ;
	}
}

static void close_disk()				/* close then unlock the stable store */
{
	close( pstorefd ) ;			/* close the store file */

	flock( lockfd,LOCK_UN ) ;

	close( lockfd ) ;			/* close the lock file */
}

static void synchronise()
{
	if ( fsync( pstorefd ) != 0 )
		error( "cannot perform 'fsync' on the stable store" ) ;
}

/*********************************************************/
/* PINT POT LAYER 3 - the shadow paging mechanism        */
/*********************************************************/

/* variables required by layer1 of the pint pot - the shadow paging */

static root_pg	root_page,*root,*root1,*root2 ;	/* copy of root page for pstore, vaddr of active root + two actual pages */
caddr_t	disk_vaddr = ( caddr_t ) -1 ;	/* address of the memory mapped stable store */
static psint	*d_start,*d_end,d_length ;	/* range of virtual addresses that support shadowing */
static psint	FLsize,FLmax,*freeList ;
static char	zeroPage[ BPAGESIZE ] ;

static void get_suitable_va()
{
	int	f,zero = 0 ;
	int 	map_res ;
	char	tmpname[ 19 ] ;

	strcpy( tmpname,"/tmp/sstore.XXXXXX" ) ;
	f = mkstemp( tmpname );
	lseek( f,VASPACESIZE,0 ) ;
	write( f,( char * ) &zero,4 ) ;
	disk_vaddr = mmap( ( caddr_t ) 0,VASPACESIZE,PROT_READ|PROT_EXEC,MAP_SHARED,f,( off_t ) 0 ) ;
	if ( disk_vaddr == ( caddr_t ) -1 )
	{
		error( "failed to find contiguous virual memory to map the stable store" ) ;
	}
	map_res = munmap( disk_vaddr,VASPACESIZE ) ;
	if ( map_res != 0 )
		error( "failed to unmap the stable store" ) ;
	
	close( f ) ;
	unlink( tmpname ) ;

	/* Now map in the two root pages */
	if ( mmap( disk_vaddr,2 << PAGEPWROF2,PROT_READ|PROT_EXEC,MAP_SHARED | MAP_FIXED,pstorefd,( off_t ) 0 ) != disk_vaddr )
	{
		error( "failed to map root pages into virtual memory" ) ;
	}
	map_res = msync( disk_vaddr,2 << PAGEPWROF2,MS_INVALIDATE ) ;
	if ( map_res != 0 )
		error( "failed to msync and invalidate the root pages" ) ;
}

static void unmap_disk()				/* dispose of the logical disk mapping */
{
	int umap_res ;

	if ( disk_vaddr != ( caddr_t ) -1 )
	{
		umap_res = munmap( disk_vaddr,VASPACESIZE ) ;
		if ( umap_res != 0 )
			error( "failed to unmap the stable store" ) ;
	}
}


static void FLSet( blockNo )
psint blockNo ;
{
	psint w,b ;

	w = BLWRD( blockNo ) ;
	b = BBIT( blockNo ) ;

	freeList[ w ] |= b ;
}

static void FLClear( blockNo )
psint blockNo ;
{
	psint w,b ;

	w = BLWRD( blockNo ) ;
	b = BBIT( blockNo ) ;

	freeList[ w ] &= ~b ;
}

static psint FLtest( blockNo )
psint blockNo ;
{
	psint w,b ;

	w = BLWRD( blockNo ) ;
	b = BBIT( blockNo ) ;
	return( ( freeList[ w ] & b ) ? PSTRUE : PSFALSE ) ;
}

static psint FLtestSet( blockNo )
psint blockNo ;
{
	psint w,b ;

	w = BLWRD( blockNo ) ;
	b = BBIT( blockNo ) ;

	if ( freeList[ w ] & b )
	{
		return( PSTRUE ) ;
	} else
	{
		freeList[ w ] |= b ;
		return( PSFALSE ) ;
	}
}


static psint FLBitSearch( w,bitMax )
psint	w,bitMax ;
{
	psint	found = PSFALSE ;
	psint	j = ( psint ) 0 ;
	psint	result = ( psint ) 0 ;

	while ( !found && ( j < bitMax ) )
	{
		psint	b ;

		b = ( ( psint ) 1 << j ) ;
		if ( freeList[ w ] & b ) j++ ; else
		{
			found = PSTRUE ;
			result = ( w << 5 ) + j ;		/* w * 32 + j */
		}
	}
	return ( result ) ;
}

static psint FLSearch()
{
	psint	found = ( psint ) 0 ;
	psint	completeWords,finalBits,i ;
	
	completeWords = FLsize >> ( psint ) 5 ;		 /* was div 32 */
	finalBits = FLsize & ( psint ) 31 ;		 /* was rem 32 */
	i = completeWords ;

	while ( found == ( psint ) 0 && i-- > ( psint ) 0 )
	{
		if ( freeList[ i ] != ( psint ) -1 )
		{
			found = FLBitSearch( i,( psint ) 32 ) ;
		}
	}

	if ( found == ( psint ) 0 ) found = FLBitSearch( completeWords,finalBits ) ;
	if ( found == ( psint ) 0 ) found = FLsize ;
	return ( found ) ;
}
			
static void clear_freelist()
{
	psint cnt ;
	char *tptr ;

	cnt = FLmax >> ( psint ) 3 ;			/* div 8 to get bytes */
	cnt += 4 ;					/* add four to get a least a multiple of four */
	tptr = ( char * ) freeList ;
	while ( cnt-- > ( psint ) 0 ) *tptr++ = ( char ) 0 ;
}

static void setup_freelist()
{
	extern char *memalign() ;
	psint  maxFreeListSize ;

	FLsize = store_length >> PAGEPWROF2 ;			/* Count number of disk pages */
	FLmax = VASPACEPAGES ;					/* Count number of va pages */
	maxFreeListSize  = FLmax ;
	maxFreeListSize >>= 3 ;					/* num. of bytes at 1 bit per page (hence div 8 ) */
	maxFreeListSize += 4 ;					/* add four to get a least a multiple of four */

	freeList = ( psint * ) memalign( WORDSIZE,maxFreeListSize ) ;		/* create a bitmap */
	if ( freeList == ( psint * ) 0 )
		error( "failed to initialise the store file freelist" ) ;

	clear_freelist() ;
	FLSet( 0 ) ;						/* DIsk page for Ist root page is used */
	FLSet( 1 ) ;						/* DIsk page for 2st root page is used */
}

/* Given a page return the address of its associated pte */
static psint *getPteAddr( p )
psint	p ;
{
	psint	*result ;
	
	p >>= PAGEPWROF2 ;			/* Get relative pageNo. */
	p -= ( psint ) 2 ;			/* ignore 1st two pages */
	
	if ( p < PT1PAGES )			/* This page is itself a pte */
	{
		return( root_page.pt2 + p ) ;
	} else
	{
		return( ( psint * )( ( char * ) disk_vaddr + ( ( psint ) 2 << PAGEPWROF2 ) ) - PT1PAGES + p ) ;
	}
}

/* Given a page return its associated pte */
#define getPte( p )	( *getPteAddr( p ) )

/* Given a page set the flags entry for its associated pte */
static void setPteFlags( p,flags )
psint	p,flags ;
{
	psint	*pteAddr,blockNo ;
	
	pteAddr = getPteAddr( p ) ;
	blockNo = *pteAddr & LOWER24 ;
	*pteAddr = blockNo | flags ;
}

/* Given a page set the blockNo entry for its associated pte */
static void setPteBlockNo( p,blockNo )
psint	p,blockNo ;
{
	psint	*pteAddr,flags ;
	
	pteAddr = getPteAddr( p ) ;
	flags = *pteAddr & UPPER8 ;
	*pteAddr = blockNo | flags ;
}


/* Given a page set the flag and blockNo entry for its associated pte */
#define setPteEntry( p,blockNo,flags )	( *getPteAddr( p ) = blockNo | flags )

static void remmap_page( target,source,protection )		/* re mmap the data page */
psint target,source,protection ;
{
	caddr_t mm_res,start ;

	start = ( caddr_t )( ( char * ) disk_vaddr + target ) ;
	source <<= PAGEPWROF2 ;
 
	mm_res = mmap( start,BPAGESIZE,(int) protection,MAP_SHARED|MAP_FIXED,pstorefd,( off_t ) source ) ;
	if ( mm_res != start )
		error( "failed to re-mmap the shadow copied page" ) ;
}

static psint map_page( p,protection )
psint	p,protection ;
{
	psint	blockNo,pageProtection ;

	pageProtection = ( psint )( PROT_WRITE | PROT_READ | PROT_EXEC ) ;

	blockNo = FLSearch() ;
	if ( blockNo == FLsize )		/* No free disk blocks - extend file */
	{
		psint	to ;

		if ( FLsize == FLmax )
			return( PSFALSE ) ;		/* Store file is full and same size as VA space */
		to = blockNo << PAGEPWROF2 ;
       		if ( lseek( pstorefd,to,0 ) != to )
			return( PSFALSE ) ;
		if ( fcntl( pstorefd,F_SETFL,file_flags | O_SYNC ) == -1 )	/* make sure the write is physically completed */
			return( PSFALSE ) ;
		if ( write( pstorefd,( char * ) zeroPage,BPAGESIZE ) != BPAGESIZE )
			return( PSFALSE ) ;
		if ( fcntl( pstorefd,F_SETFL,file_flags ) == -1 )
			return( PSFALSE ) ;
		FLsize++ ;					/* Ok - so freelist size is extended by one bit - already marked unused */
	}
	if ( protection & PROTECTED ) pageProtection = ( psint )( PROT_READ | PROT_EXEC ) ;
	remmap_page( p & PAGENUMBER,blockNo,pageProtection ) ;
	FLSet( blockNo ) ;
	setPteEntry( p,blockNo,protection ) ;
	return( PSTRUE ) ;
}

/* Make sure that the given page p and all its ancestor ptes are mapped */
static psint checkPageMapping( p,protection )
psint	p,protection ;
{
	psint	p1 ;

	p1 = p >> PAGEPWROF2 ;

	if ( p1 >= ( PT1PAGES + ( psint ) 2 ) )			/* This page is not a pte */
	{
		p1 = getPte( p ) ;				/* Get pte of this page */

		if ( p1 == ( psint ) 0 )			/* If zero then page p is not mapped!! */
		{
			if ( protection == DN )			/* its its dont need - no point allocating a block */
			{
				return( PSFALSE ) ;
			} else
			{
				return( map_page( p,protection ) ) ;	/* Map the page */
			}
		}
	}
	return( PSTRUE ) ;
}


static void remap_read()
{
	psint 	i,j,flags,*pte,p,q,result ;

	for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ )
	{
		p = ( i + ( psint ) 2 ) << PAGEPWROF2 ;

		if ( root_page.pt2[ i ]  != ( psint ) 0 )
		{
			/* This is silly - its being mapped in to map out elsewhere */

			setPteFlags( p,RO ) ;		/* Mark it for read-only */

			result = SS_shadowVM( p,p ) ;	/* Make a copy of the pte */
			if ( !result )
				error( "failed to shadow copy the page tables" ) ;

			q = ( i * WPAGESIZE + PT1PAGES + ( psint ) 2 ) << PAGEPWROF2 ;

			for ( j = ( psint ) 0 ; j < WPAGESIZE ; j++ )
			{
    				pte = ( psint * ) ( ( char * ) disk_vaddr + p ) ;

				if ( pte[ j ]  != ( psint ) 0 )
				{
					flags = pte[ j ] & UPPER8 ;
					
					if ( flags & RESTORE )			/* Shadowed or SaveOnly or ReadOnly */
					{
						if ( !( flags & RO ) ) setPteFlags( q,RO ) ; 	/* Now mark it as read-only */
					}
				}
				q += BPAGESIZE ;
			}

		} else
		{
			if ( !map_page( p,SO ) )
				error( "failed to create the page tables" ) ;
		}
	}

}

static void remap_write( p1,p2 )			/* set the memory protection to read/write on the pages p1 to p2 */
psint p1,p2 ;
{
	int mp_res,length ;
	caddr_t start ;

	start = ( caddr_t )( ( char * ) disk_vaddr + p1 ) ;
	length = p2 - p1 + BPAGESIZE ;

	mp_res = mprotect( start,length,PROT_READ|PROT_EXEC|PROT_WRITE ) ;
	if ( mp_res != 0 )
	{
		extern int errno ;

		printf( "mprotect failed for %x,%x,%x,%d\n",start,length,PROT_READ|PROT_EXEC|PROT_WRITE,errno ) ;
		error( "failed to remap some data pages as read/write" ) ;
	}
}

static void remap_noaccess( p1,p2 )			/* set the memory protection to no access on the pages p1 to p2 */
psint p1,p2 ;
{
	int mp_res,length ;
	caddr_t start ;

	start = ( caddr_t )( ( char * ) disk_vaddr + p1 ) ;
	length = p2 - p1 + BPAGESIZE ;

	mp_res = mprotect( start,length,PROT_NONE ) ;
	if ( mp_res != 0 )
		error( "failed to remap some data pages as inaccessible" ) ;
}

static void sync_page( p )				/* sync the vm copy of the given page with the disk */
psint	p ;
{
	int ms_res ;
	caddr_t start ;

	start = ( caddr_t )( ( char * ) disk_vaddr + p ) ;
	ms_res = msync( start,BPAGESIZE,0 ) ;
	if ( ms_res != 0 )
		error( "failed to synchronise a page with the store file" ) ;
}

static void sync_root()				/* sync the vm copy of the root pages with the disk */
{
	int ms_res ;

	ms_res = msync( disk_vaddr,BPAGESIZE * ( psint ) 2,0 ) ;
	if ( ms_res != 0 )
		error( "failed to synchronise the root pages with the store file" ) ;
}

static void read_root()					/* copy root page from the store file - to the local copy */
{
	register psint cnt,*p1,*p2 ;

	cnt = WPAGESIZE ;
	p1 = ( psint * )( &root_page ) ;
	p2 = ( psint * ) root ;
	while( cnt-- > ( psint ) 0 )
	{
		*p1++ = *p2++ ;
	}
}

static void write_root()					/* write copy of root page to the store file */
{
	register psint cnt ;

	root_page.date1 += ( psint ) 1 ;
	root_page.date2 = root_page.date1 ;

        cnt = root == root1 ? BPAGESIZE : 0 ;
        if ( lseek( pstorefd,cnt,0 ) != cnt )
                error( "lseek failed while writing root page" ) ;
        if ( write( pstorefd,( char * )( &root_page ),BPAGESIZE ) != BPAGESIZE )
                error( "write failed while writing root page" ) ;

	sync_root() ;					/* msync the root pages */
        root = root == root1 ? root2 : root1 ;		/* Swap root pages */
}

static psint move_page( from,to )
psint from,to ;
{
	psint topos ;

	topos = to << PAGEPWROF2 ;
       	if ( lseek( pstorefd,topos,0 ) != topos )
		return( PSFALSE ) ;

	if ( to == FLsize )		/* No free disk blocks - extend file */
	{
		if ( FLsize == FLmax ) return ( PSFALSE ) ;	/* Store file is full and same size as VA space */

		if ( fcntl( pstorefd,F_SETFL,file_flags | O_SYNC ) == -1 )	/* make sure the write will be physically completed */
			return( PSFALSE ) ;
		if ( write( pstorefd,( char * ) disk_vaddr + from,BPAGESIZE ) != BPAGESIZE )
			return( PSFALSE ) ;
		if ( fcntl( pstorefd,F_SETFL,file_flags ) == -1 )
			return( PSFALSE ) ;

		FLsize++ ;		/* Ok - so freelist size is extended by one bit - already marked unused */
	} else
	{
		if ( write( pstorefd,( char * ) disk_vaddr + from,BPAGESIZE ) != BPAGESIZE )
			return( PSFALSE ) ;
	}
	return ( PSTRUE ) ;
}

static void restore_pages()
{
	psint 	i,j,blockNo,flags,*pte,p,q,result ;

	for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ )
	{
		p = ( i + ( psint ) 2 ) << PAGEPWROF2 ;

		if ( root_page.pt2[ i ]  != ( psint ) 0 )
		{
			/* This is silly - its being mapped in to map out elsewhere */
			blockNo = root_page.pt2[ i ] & LOWER24 ;
			flags = root_page.pt2[ i ] & UPPER8 ;
			remmap_page( p,blockNo,( psint ) ( PROT_READ | PROT_EXEC ) ) ;
			FLSet( blockNo ) ;		/* Mark block in free list */

			q = ( i * WPAGESIZE + PT1PAGES + ( psint ) 2 ) << PAGEPWROF2 ;

			for ( j = ( psint ) 0 ; j < WPAGESIZE ; j++ )
			{
    				pte = ( psint * ) ( ( char * ) disk_vaddr + p ) ;
				if ( pte[ j ]  != ( psint ) 0 )
				{
					flags = pte[ j ] & UPPER8 ;
					
					if ( flags & RESTORE )			/* Shadowed or SaveOnly or ReadOnly */
					{
						blockNo = pte[ j ] & LOWER24 ;
						remmap_page( q,blockNo,( psint ) ( PROT_READ | PROT_EXEC ) ) ;
						FLSet( blockNo ) ;		/* Mark block in free list */
					} else pte[ j ]  = ( psint ) 0 ;	/* Is this necessary */
				}
				q += BPAGESIZE ;
			}

		} 
	}

}

static void sync_pages()
{
	psint 	i,j,flags,*pte,p,q ;

	for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ )
	{
		if ( root_page.pt2[ i ]  != ( psint ) 0 )
		{
			flags = root_page.pt2[ i ] & UPPER8 ;
			
			if ( flags & WRITTEN )			/* Shadowed or SaveOnly */
			{
				p = ( i + ( psint ) 2 ) << PAGEPWROF2 ;
				sync_page( p ) ;

				q = ( i * WPAGESIZE + PT1PAGES + ( psint ) 2 ) << PAGEPWROF2 ;

				for ( j = ( psint ) 0 ; j < WPAGESIZE ; j++ )
				{
    					pte = ( psint * ) ( ( char * ) disk_vaddr + p ) ;
					if ( pte[ j ]  != ( psint ) 0 )
					{
						flags = pte[ j ] & UPPER8 ;
						
						if ( flags & WRITTEN )			/* Shadowed or SaveOnly */
						{
							sync_page( q ) ;
						}
					}
					q += BPAGESIZE ;
				}

			}
		}
	}
}


static page_fault( sig,code,scp,addr )
int sig,code ; struct sigcontext *scp ; char *addr ;
{
	register psint *pageaddr,*svmaddr ;
	extern void BSet() ;

	signal( sig,SIG_IGN ) ;				/* ignore the offending signal for now */
	switch( SEGV_CODE( code ) )
	{
	case SEGV_OBJERR:
		if ( SEGV_ERRNO( code ) != EINTR )	/* OK */
		{
			printf( "object error for address %x %d\n",addr,SEGV_ERRNO( code ) ) ;
			error( "System signal - segmentation violation" ) ;
		}
	case SEGV_PROT:
		pageaddr = ( psint * )( ( psint ) addr & PAGENUMBER ) ;
		if ( pageaddr < d_start || pageaddr >= d_end )
		{
			printf( "segv_prot or segv_obj_error & errno != EINTR for address %x %d\n",addr,SEGV_ERRNO( code ) ) ;
			error( "System signal - segmentation violation" ) ;
		}
		svmaddr = ( psint * )( addr - ( char * ) disk_vaddr ) ;
		user_write_fault( svmaddr,( psint * ) scp ) ;	/* user must fix things up - we get a loop if not.... */
		break ;
	case SEGV_NOMAP:
		printf( "no mapping for address %x\n",addr ) ;
	default:
		error( "System signal - segmentation violation" ) ;
	}
	signal( sig,page_fault ) ;			/* reenable the offending signal */
}

static void setup_root_pages()
{
	root1 = ( root_pg * )( disk_vaddr ) ;	/* address of the start of the disk in vm - first root page */
	root2 = root1 + 1 ;			/* address of the second root page */

	if ( root1->date1 == root1->date2 )	/* find the valid root page */
	{
		if ( root2->date1 != root2->date2 ) root = root1 ; else
		{
			root = root1->date1 > root2->date1 ? root1 : root2 ;
		}
	} else
	{
		if ( root2->date1 == root2->date2 ) root = root2 ; else
			error( "root pages both corrupt - tough !" ) ;
	}
}

static void setup_length_info()
{
	if ( root->sstoreMagic != SSTOREMAGIC )
		error( "the store file was created by a different implementation of the stable store" ) ;
	if ( root->page_size != BPAGESIZE )
		error( "page size used by the store file is incompatible with the stable store implementation" ) ;
	if ( root->page_size < getpagesize() )
		error( "page size used by the store file is incompatible with virtual memory" ) ;

	d_start = ( psint * )( root1 + 2 + PT1PAGES ) ;	/* calculate the first usable address of shadowed store */
						/* calculate the end of the shadowed store */
	d_end = ( psint * )( ( char * ) disk_vaddr +  VASPACESIZE ) ;
	d_length = root->store_length ;
	if ( d_length <= ( psint ) 0 )
		error( "root pages are corrupt - length information incorrect" ) ;
}

static void inc_restart_clock()
{
	root_page.restart_clock++ ;
}

static void init_store( dirname )			/* open the file containing the store */
char *dirname ;
{
	open_disk( dirname ) ;			/* lock and open the stable store */
	get_suitable_va() ;			/* Find suitable VA and mmap root pages */
	setup_root_pages() ;			/* find out which root page is uptodate */
	setup_length_info() ;			/* find out how big the store actually is? */
	read_root() ;				/* make a local copy of the root page */
	setup_freelist() ;			/* setup freespace list from scratch */
	restore_pages() ;			/* re-map pages from last checkpoint */
	remap_read() ;				/* Change Pte's to read-only */


	inc_restart_clock() ;			/* increment the restart clock */

	signal( SIGSEGV,page_fault ) ;		/* enable the page_fault handler */
	signal( SIGBUS,page_fault ) ;		/* enable the page_fault handler */

	user_restore() ;			/* startup the user's restorey code */
}

void SS_close()					/* finish using the shadow store */
{
	unmap_disk() ;				/* forget the vm copy of the stable store */
	close_disk() ;				/* finish using the disk */
}

void SS_stabilise()				/* secure all changed pages to the store file as an atomic action */
{
	user_save() ;				/* execute the user's save code */

	sync_pages() ;				/* make sure the active data pages are uptodate on the disk */
	write_root() ;				/* write the root page */
	clear_freelist() ;			/* clear out the freelist */
	FLSet( 0 ) ;				/* Disk page for Ist root page is used */
	FLSet( 1 ) ;				/* Disk page for 2st root page is used */
	restore_pages() ;
	remap_read() ;				/* Change Pte's to read-only */

	user_restore() ;			/* execute the user's restorey code */
}

psint *SS_real_address( pos )			/* should give physical address of the word */
psint pos ;
{
	return( ( psint * )( ( char * ) disk_vaddr + pos ) ) ;
}

psint SS_read_word( pos )			/* return word as address pos */
psint pos ;
{
	psint *addr ;

	addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
	return( *addr ) ;
}

void SS_read_words( pos,buffer,nwords )	/* copy n words from 'pos' in the store file to the buffer, pos is in bytes */
psint pos ; psint *buffer ; psint nwords ;
{
	psint *addr ;

	addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
	while( nwords-- > ( psint ) 0 )
	{
		*buffer++ = *addr++ ;
	}
}

void SS_write_word( pos,theword )			/* write theword to address pos */
psint pos,theword ;
{
	psint *addr ;

	addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
	*addr = theword ;
}

void SS_write_words( pos,buffer,nwords )	/* copy n words from the buffer to 'pos' in the store file, pos is in bytes */
psint pos ; psint *buffer ; psint nwords ;
{
	psint *addr ;

	addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
	while( nwords-- > ( psint ) 0 )
	{
		*addr++ = *buffer++ ;
	}
}

psint SS_first_address()					/* what is the first usable address */
{
	return ( root_page.page_size * ( PT1PAGES + ( psint ) 2 ) ) ;
}

psint SS_last_address()					/* what is the address of the last usable word */
{
	return( SS_first_address() + d_length - WORDSIZE ) ;
}

void SS_set_sheap_version( vnum )			/* save version number for stable heap */
psint vnum ;
{
	root_page.sheapMagic = vnum ;
}

psint SS_get_sheap_version()				/* return version number for stable heap */
{
	return( root_page.sheapMagic ) ;
}

void SS_set_restart_clock( tim )				/* save version number for stable heap */
psint tim ;
{
	root_page.restart_clock = tim ;
}

psint SS_get_restart_clock()				/* return version number for stable heap */
{
	return( root_page.restart_clock ) ;
}

static psint shadow_copy( page )
psint page ;
{
	psint	nextdst,result ;
	
	nextdst = FLSearch() ;				/* Find first free blck */
	result = move_page( page,nextdst ) ;
	if ( result )
	{
		remmap_page( page,nextdst,( psint ) ( PROT_WRITE | PROT_READ | PROT_EXEC ) ) ;	/* mmap shadow copy to new disk block */
		FLSet( nextdst ) ;			/* Mark it as being used */
		setPteBlockNo( page,nextdst ) ;		/* Record new mapping in page tables */
	}
	return( result ) ;
}

psint SS_shadowVM( a1,a2 )
psint a1,a2 ;
{
	psint pte,flags,t,p1,p2,savep1 ;

	if ( a1 > a2 )						/* make sure a1 <= a2 */
	{
		t = a1 ;
		a1 = a2 ;
		a2 = t ;
	}

	p1 = a1 & PAGENUMBER ; p2 = a2 & PAGENUMBER ;		/* 1st and last pages */
	savep1 = p1 ;
	while ( p1 <= p2 )
	{
		if ( !checkPageMapping( p1,RO ) ) return( PSFALSE ) ;	/* Shouldn't need this here ?? */
		pte = getPte( p1 ) ;
		flags = pte & UPPER8 ;
		if ( !( flags & WRITTEN ) )
		{
			if ( flags & PROTECTED )
			{
				if ( !shadow_copy( p1 ) ) return( PSFALSE ) ;
				setPteFlags( p1,SP ) ; 			/* Change pte to reflect the copy - already done ???  */
			} else
			{
				remap_write( p1,p1 ) ;			/* if OK write enable the pages */
				setPteFlags( p1,SO ) ; 			/* Change pte to reflect the change */
			}
		}
		p1 += BPAGESIZE ;
	}
	return( PSTRUE ) ;
}

psint SS_saveonlyVM( a1,a2 )
psint a1,a2 ;
{
	psint pte,flags,t,p1,p2,pp1,pp2,savep1,savep2 ;

	if ( a1 > a2 )						/* make sure a1 <= a2 */
	{
		t = a1 ;
		a1 = a2 ;
		a2 = t ;
	}

	p1 = a1 & PAGENUMBER ; savep1 = p1 ;			/* make sure we've remembered the two page numbers */
	p2 = a2 & PAGENUMBER ; savep2 = p2 ;

								/* are the first and last pages complete? */
	pp1 = ( ( a1 & PAGEINDEX ) != ( psint ) 0 ) ? PSTRUE : PSFALSE ;
	pp2 = ( ( a2 & PAGEINDEX ) != LASTWORD ) ? PSTRUE : PSFALSE ;
	if ( p1 == p2 )
	{
		pp1 = pp1 || pp2 ;
		pp2 = PSFALSE ;
	}

	if ( pp1 )						/* if 1st page overlaps another use - check it */
	{
		if ( !checkPageMapping( p1,SO ) ) return( PSFALSE ) ;
		pte = getPte( p1 ) ;
		flags = pte & UPPER8 ;
		if ( !( flags & WRITTEN ) )
		{
			if ( flags & PROTECTED )
			{
				if ( !shadow_copy( p1 ) ) return( PSFALSE ) ;
				setPteFlags( p1,SP ) ; /* CHange pte to reflect the copy - already done ???  */
			} else
			{
				remap_write( p1,p1 ) ;			/* if OK write enable the pages */
				setPteFlags( p1,SO ) ; /* Change pte to reflect the change */
			}
		}
		p1 += BPAGESIZE ;
	}

	if ( pp2 )						/* if 2nd page overlaps another use - check it */
	{
		if ( !checkPageMapping( p2,SO ) ) return( PSFALSE ) ;
		pte = getPte( p2 ) ;
		flags = pte & UPPER8 ;
		if ( !( flags & WRITTEN ) )
		{
			if ( flags & PROTECTED )
			{
				if ( !shadow_copy( p2 ) ) return( PSFALSE ) ;
				setPteFlags( p2,SP ) ; /* CHange pte to reflect the copy - already done ???  */
			} else
			{
				remap_write( p2,p2 ) ;			/* if OK write enable the pages */
				setPteFlags( p2,SO ) ; /* Change pte to reflect the change */
			}
		}
		p2 -= BPAGESIZE ;
	}

	while ( p1 <= p2 )				/* fix up the complete pages in the range a1->a2 */
	{
		if ( !checkPageMapping( p1,SO ) ) return( PSFALSE ) ;
		if ( !( flags & WRITTEN ) )
		{
			remap_write( p1,p1 ) ;			/* if OK write enable the pages */
		}
		setPteFlags( p1,SO ) ; 				/* Change pte to reflect the change */
		p1 += BPAGESIZE ;
	}
	return( PSTRUE ) ;
}

psint SS_scratchVM( a1,a2 )
psint a1,a2 ;
{
	psint pte,flags,t,p1,p2,pp1,pp2,savep1,savep2 ;

	if ( a1 > a2 )						/* make sure a1 <= a2 */
	{
		t = a1 ;
		a1 = a2 ;
		a2 = t ;
	}

	p1 = a1 & PAGENUMBER ; savep1 = p1 ;			/* make sure we've remembered the two page numbers */
	p2 = a2 & PAGENUMBER ; savep2 = p2 ;

								/* are the first and last pages complete? */
	pp1 = ( ( a1 & PAGEINDEX ) != ( psint ) 0 ) ? PSTRUE : PSFALSE ;
	pp2 = ( ( a2 & PAGEINDEX ) != LASTWORD ) ? PSTRUE : PSFALSE ;
	if ( p1 == p2 )
	{
		pp1 = pp1 || pp2 ;
		pp2 = PSFALSE ;
	}

	if ( pp1 )
	{
		if ( !checkPageMapping( p1,SC ) ) return( PSFALSE ) ;
		pte = getPte( p1 ) ;
		flags = pte & UPPER8 ;
		if ( !( flags & WRITTEN ) )
		{
			if ( flags & PROTECTED )
			{
				if ( !shadow_copy( p1 ) ) return( PSFALSE ) ;
				setPteFlags( p1,SP ) ; /* CHange pte to reflect the copy - already done ???  */
			}
		}
		p1 += BPAGESIZE ;
	}

	if ( pp2 )
	{
		if ( !checkPageMapping( p2,SC ) ) return( PSFALSE ) ;
		pte = getPte( p2 ) ;
		flags = pte & UPPER8 ;
		if ( !( flags & WRITTEN ) )
		{
			if ( flags & PROTECTED )
			{
				if ( !shadow_copy( p2 ) ) return( PSFALSE ) ;
				setPteFlags( p2,SP ) ; /* CHange pte to reflect the copy - already done ???  */
			}
		}
		p2 -= BPAGESIZE ;
	}

	while ( p1 <= p2 )				/* fix up the complete pages in the range a1->a2 */
	{
		if ( !checkPageMapping( p1,SC ) ) return( PSFALSE ) ;
		setPteFlags( p1,SC ) ; 				/* Change pte to reflect the change */
		p1 += BPAGESIZE ;
	}
	remap_write( savep1,savep2 ) ;				/* if OK write enable the pages */
	return( PSTRUE ) ;
}

psint SS_reserveVM( a1,a2 )						/* address in the range a1 -> a2 will be used later */
psint a1,a2 ;
{
	psint t,p1,p2,pp1,pp2,savep1 ;

	if ( a1 > a2 )						/* make sure a1 <= a2 */
	{
		t = a1 ;
		a1 = a2 ;
		a2 = t ;
	}

	p1 = a1 & PAGENUMBER ;
	p2 = a2 & PAGENUMBER ;

								/* are the first and last pages complete? */
	pp1 = ( ( a1 & PAGEINDEX ) != ( psint ) 0 ) ? PSTRUE : PSFALSE ;
	pp2 = ( ( a2 & PAGEINDEX ) != LASTWORD ) ? PSTRUE : PSFALSE ;
	if ( p1 == p2 )
	{
		pp1 = pp1 || pp2 ;
		pp2 = PSFALSE ;
	}

	if ( pp1 ) p1 += BPAGESIZE ;

	if ( pp2 ) p2 -= BPAGESIZE ;

	savep1 = p1 ;						/* preserve the 1st page number */
	while ( p1 <= p2 )					/* fix up the complete pages in the range a1->a2 */
	{
		if ( !checkPageMapping( p1,RE ) ) return( PSFALSE ) ;
		setPteFlags( p1,RE ) ; 				/* Change pte to reflect the change */
		p1 += BPAGESIZE ;
	}
	if ( savep1 <= p2 ) remap_noaccess( savep1,p2 ) ;	/* if OK disable the pages */
	return( PSTRUE ) ;
}

void SS_dontneedVM( a1,a2 )
psint a1,a2 ;
{
#ifdef	NOTUSED
	psint t,p1,p2,pp1,pp2,savep1 ;

	if ( a1 > a2 )						/* make sure a1 <= a2 */
	{
		t = a1 ;
		a1 = a2 ;
		a2 = t ;
	}

	p1 = a1 & PAGENUMBER ;
	p2 = a2 & PAGENUMBER ;

								/* are the first and last pages complete? */
	pp1 = ( ( a1 & PAGEINDEX ) != ( psint ) 0 ) ? PSTRUE : PSFALSE ;
	pp2 = ( ( a2 & PAGEINDEX ) != LASTWORD ) ? PSTRUE : PSFALSE ;
	if ( p1 == p2 )
	{
		pp1 = pp1 || pp2 ;
		pp2 = PSFALSE ;
	}

	if ( pp1 ) p1 += BPAGESIZE ;

	if ( pp2 ) p2 -= BPAGESIZE ;

	savep1 = p1 ;						/* preserve the 1st page number */
	while ( p1 <= p2 )				/* fix up the complete pages in the range a1->a2 */
	{
		if ( checkPageMapping( p1,DN ) )
		{
			setPteFlags( p1,DN ) ; 				/* Change pte to reflect the change */
		}
		p1 += BPAGESIZE ;
	}
	if ( savep1 <= p2 ) remap_noaccess( savep1,p2 ) ;	/* if OK write enable the pages */
#endif	NOTUSED
}

psint SS_open( dirname,error,save,restore,write_fault )
char *dirname ; void ( *error )(),( *save )(),( *restore )(),( *write_fault )() ;
{
	user_error = error ;				/* save the user specified interfaces */
	user_save = save ;
	user_restore = restore ;
	user_write_fault = write_fault ;

	init_store( dirname ) ;				/* open the stable store */

							/* if error occurs never reach here.... */
	return( PSTRUE ) ;				/* return success indicator */
}

psint SS_set_lock( pos )
psint pos ;
{
	psint *addr ;

	addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
	return( SS_Xmemlock( addr - WORDSIZE ) ) ;
}

psint SS_Xmemlock( real_addr )
psint *real_addr ;
{
	error( "Exclusive memory lock can only be set by inline code obtained from '/usr/lib/libpostore.il'" ) ;
	return( ( psint ) 0 ) ;
}
