/********************************************************/
/*                                                    	*/
/*            PINT POT LAYERS 2 and 3                 	*/
/*                                                    	*/
/* LAYER 2 - PHYSICAL STORAGE                         	*/
/* LAYER 3 - SHADOW PAGED STABLE STORAGE              	*/
/*                                                    	*/
/* This module exports the functions SS_close(),	*/
/* SS_configuration(),SS_dontneedVM(),			*/
/* SS_first_address(),SS_get_restart_clock(),		*/
/* SS_get_sheap_version(),SS_last_address(),SS_open(),	*/
/* SS_read_word(),SS_read_words(),SS_real_address(),	*/
/* SS_reserveVM(),SS_saveVM(),SS_scratchVM(),		*/
/* SS_set_lock(),SS_set_restart_clock(),		*/
/* SS_set_sheap_version(),SS_stabilise(),		*/
/* SS_statistics(),SS_write_word(),SS_write_words(),	*/
/* SS_Xmemlock()					*/
/*                                                    	*/
/* created  1/ 7/91    DSM                            	*/
/* mod     25/10/91    AB - flock used inplace of lockf */
/* mod      13/3/92    AB - bug fixes, file extending   */
/*                          writes can find disk full   */
/* mod     24/06/92    AB/DM - optimised + v2 interface */
/* mod     22/10/93    DM - added compactor             */
/* mod     20/06/94    GK - re-added host validation    */
/* mod     16/09/96    HK - Added Arch_sun5 (Solaris)   */
/* mod     14/06/97    HK - Added Arch_i586 (Linux)     */
/* mod     18/02/98    HK - Added Arch_win32 (Win/NT)   */
/********************************************************/

#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>

#include	<sys/file.h>		/* file locking macros */

#include	<signal.h>
#include	<unistd.h>

/* 1998/02/16 HK: <sys/errno.h> should be included by <errno.h> */
/* #include	<sys/errno.h> */

#if defined(Arch_hpux)		/* 1997/05/20 HK */
#include	<sys/stat.h>
#endif

#include	<sys/types.h>
#include	<sys/mman.h>
#include	<errno.h>

#include	"sstore.h"		/* stable store definitions */

#if defined(Arch_i586)		/* 1997/06/14 HK */

#include	<asm/sigcontext.h>

#elif defined(Arch_sun5)||defined(Arch_hpux)	/* 1996/09/11 HK */

#include	<ucontext.h>
#include	<fcntl.h>
#include	<siginfo.h>

static char	SigStack [ SIGSTKSZ ];

#elif defined(Arch_win32)	/* 1998/02/17 HK */

#include	<direct.h>
#include	<win32sig.h>

#endif

#if defined(Arch_hpux)
/* HP-UX does not like PROT_EXEC on memory-mapped files: */
#ifdef	PROT_EXEC
#undef	PROT_EXEC
#endif
#define	PROT_EXEC 0
#endif

#ifndef MAP_FILE
#define MAP_FILE 0
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

#if ! defined(MAX)
#define MAX(x,y) (((x)>(y))?(x):(y))
#endif

/******************************************************/
/* PINT POT LAYER 2 - access to the physical storage  */
/******************************************************/

/* external functions */

psint	__bInitializeSStore__	= 1;

static psint move_page( psint from, psint to );
static void remmap_page( psint target,psint source,psint protection );

/* variables required by layer2 of the pintpot - the physical storage */

/* char * to hold the name of stable store file */
static char	storefname[ 256 ] ;

/* fd for the psstore */
static int	pstorefd ;

/* Size of the store file */
static psint	store_length ;

static int	lockfd ;

/* char * to hold the name of the lock file */
static char	lockfname[ 256 ] ;

/* char * to hold the name of the host on which we must be */
static char	lockhost[ 64 ] ;

/* char * to hold the name of the host we're on */
static char	thishost[ 64 ] ;

/* save status flags for the open store file */
static int	file_flags ;

/* user provided interface variables */

static PFNUSERERROR user_error;
static PFNVOID user_save, user_restore;
static PFNUSERWRITEFAULT user_write_fault;
static PFNUSERPAGEFAULT user_page_fault;

#include "../secure/ValidateHost.c"

static const char __file__ []	= __FILE__;

#define error( message ) fnError ( __LINE__, errno, message, user_error )
/* error handling code */
static void fnError( int nLine, int nErrNo, const char * pszMessage,
		     PFNUSERERROR pfnError )

{
  char szBuffer[ 1024 ] ;

  sprintf ( szBuffer, "%s(%d): %s.\n"
	    "       errno is %d.",
	    __file__, nLine, pszMessage, nErrNo );
  ( * pfnError ) ( ( char * ) szBuffer ) ;
  SS_close() ;
  exit( -1 ) ;
}

static void reorderPage( char * buffer )
{
  int i ;
  char sbuffer[ BPAGESIZE ] ;

  for ( i = 0 ; i < BPAGESIZE ; i++ )
    {
      sbuffer[ i ^ 3 ] = buffer[ i ] ;
    }
  for ( i = 0 ; i < BPAGESIZE ; i++ )
    {
      buffer[ i ] = sbuffer[ i ] ;
    }
}

static reorderStore( const char *dirname )
{
  static const char szError [] =
    "could not change the object store's byte ordering";

  int tempfd ;
  psint len = store_length ;
  char buffer[ BPAGESIZE ] ;
  char tempfname[ 81920 ] ;

  ( void ) strcpy( tempfname,dirname ) ;
  ( void ) strcat( tempfname,TEMPSUFFIX ) ;
  tempfd = open( tempfname,O_RDWR | O_CREAT,0666 ) ;
  ( void ) chmod( tempfname,0666 );

  if ( tempfd < 0 )
    error( szError ) ;
		
  while( len > 0 )
    {
      if ( read( pstorefd,buffer,BPAGESIZE ) != BPAGESIZE )
	error( szError ) ;
      reorderPage( buffer ) ;
      if ( write( tempfd,buffer,BPAGESIZE ) != BPAGESIZE )
	error( szError ) ;
      len -= BPAGESIZE ;
    }
	
  (void) fsync( tempfd ) ;
  (void) close( tempfd ) ;
  (void) close( pstorefd ) ;
  (void) rename( tempfname,storefname ) ;
	
  pstorefd = open( storefname,O_RDWR ) ;
  if ( pstorefd < 0 ) {
    /* 1996/10/23 HK: extended the error message: */
    char	szMsg [ 256 ];
    sprintf ( szMsg, "cannot open the store `%s'", storefname );
    error(  szMsg );
  }

  file_flags = fcntl( pstorefd,F_GETFL,0 ) ;	/* remember the status flags */
}

/* lock then open the stable store */
static void open_disk( const char *dirname)
{
  psint mymagic ;
  int status, i ;
  /* 1998/02/16 HK:
     extern int errno ;
     */
	
  if ( dirname == ( char * ) 0 )
    {	
      error( "a null pathname was specified for the stable store" ) ;
    }

  /* discover the name of the store and its lock file */
  ( void ) strcpy( storefname,dirname ) ;
  ( void ) strcat( storefname,STORESUFFIX ) ;

  status = XXValidHost() ;
  if ( status < 0 )
    {
      error( "This host is not authorised to use the stable store" ) ;
    }

  /* open the pstore */
  pstorefd = open( storefname,O_RDWR ) ;
  if ( pstorefd < 0 )
    error( "cannot open the store" ) ;
  file_flags = fcntl( pstorefd,F_GETFL,0 ) ;	/* remember the status flags */

  if ( read( pstorefd,(char *) &mymagic,WORDSIZE ) != WORDSIZE )
    error( "cannot read the store's magic number" ) ;

  /* how long is the actual store file */
  store_length = ( psint ) lseek( pstorefd,( off_t ) 0,SEEK_END ) ;
  /* reset file pos to the start */
  ( void ) lseek( pstorefd,( off_t ) 0,SEEK_SET ) ;
  if ( store_length < ( psint ) 0 )
    error( "unable to determine the length of the stable store" ) ;
  if ( store_length & PAGEINDEX )
    error( "the length of the store file is not a multiple number of virtual pages" ) ;

  /* lock the pstore */
  ( void ) strcpy( lockfname,dirname ) ;
  ( void ) strcat( lockfname,LOCKSUFFIX ) ;
  lockfd = open( lockfname,2 ) ;
  if ( lockfd < 0 )
    error( "cannot open the store's lock file" ) ;

#if defined(Arch_sun4)
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
#else	/* 1996/09/11 HK */
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
  memset ( thishost, 0, sizeof ( thishost ) );
  ( void ) gethostname( thishost, sizeof ( thishost ) ) ;
  for ( i = 0; i < sizeof ( thishost ) - 1 && thishost [ i ] > ' '; i++ );
  thishost[ i ] = '\0';
  /* get the name of the host we should be on - not more than 63 chars */
  memset ( lockhost, 0, sizeof ( lockhost ) );
  ( void ) read( lockfd,lockhost,sizeof ( lockhost ) ) ;
  for ( i = 0; i < sizeof ( lockhost ) - 1 && lockhost [ i ] > ' '; i++ );
  lockhost[ i ] = '\0';
  if ( strcmp( lockhost,thishost ) )	/* give up if the two names dont match */
    {
      char buff[ 512 ] ;

      ( void ) sprintf( buff,"system must be run on host '%s', not '%s'",lockhost,thishost ) ;
      error( buff ) ;
    }
  if ( mymagic == SSTOREREVERSED )			/* does the byte ordering need fixed for this object store? */
    {
      reorderStore( dirname ) ;
    }
}

/* close then unlock the stable store */
static void close_disk()
{
  /* close the store file */
  ( void ) close( pstorefd ) ;
  pstorefd = -1;

  /* 1996/09/16: No locking on the lock file for now: */
  /*
    ( void ) flock( lockfd,LOCK_UN ) ;
  */

  ( void ) close( lockfd ) ;			/* close the lock file */
 lockfd = -1;
}
/*********************************************************/
/*               the shadow paging mechanism             */
/*********************************************************/

/* variables required by layer1 of the pint pot - the shadow paging */

/* copy of root page for pstore, vaddr of active root + two actual pages */
static	root_pg	root_page,*root,*root1,*root2 ;

/* address of the memory mapped stable store */
caddr_t	disk_vaddr = (caddr_t) -1 ;

static psint	FLsize,*pte,*blockList,blockCount,*reuseList,*freeList ;

/* Count the number of modified data pages written back on checkpoint */
static psint	modifiedCount = ( psint ) 0 ;

/* More stats */
static psint	freeCount,needCount,toFreeCount ;

/* Starting place to look for free block */
static psint	lookFrom ;

static char	zeroPage[ BPAGESIZE ] ;

static psint extend_store( psint npages )
{
  psint result = 0 ;

  /* End of file - ignore errors?!? */
  ( void ) lseek( pstorefd,( off_t ) 0,SEEK_END ) ;
  while( npages-- > 0 ) {
    /* Don't care what's on the disk block - use dummy address */
    if ( write( pstorefd,zeroPage,BPAGESIZE ) != BPAGESIZE ) {
      npages = 0 ;
    } else {
      result++ ;
    }
  }
  return( result ) ;
}

static void get_suitable_va( caddr_t min_addr )
{
  int	f,zero = 0 ;
  int 	map_res ;
  char	tmpname[ 256 ] ;

  if ( min_addr != (caddr_t) NULL ) {
    unsigned long m, s;
    /* Round up to nearest BPAGESIZE: */
    m		= (unsigned long) min_addr;
    s		= (unsigned long) BPAGESIZE;
    min_addr	= (caddr_t) ( ( ( m + s - 1 ) / s ) * s );
  }

#if defined(Arch_mips)
  /* this puts postore at 384M */
  disk_vaddr = (caddr_t) MAX ( 0x18000000, (unsigned long) min_addr );

  /* Now map in the two root pages */
  if ( mmap( disk_vaddr,
	     ROOTPAGES << PAGEPWROF2,
	     PROT_READ | PROT_WRITE | PROT_EXEC,
	     MAP_SHARED | MAP_FIXED | MAP_FILE,
	     pstorefd,( off_t ) 0 ) != disk_vaddr )
    {
      error( "failed to map root pages into virtual memory" ) ;
    }
#elif defined(Arch_alpha)
  /* this puts c. 64G between mapped store and malloc'd stuff */
  disk_vaddr = (caddr_t) MAX ( 0x1000000000, (unsigned long) min_addr );

  /* Now map in the two root pages */
  if ( mmap( disk_vaddr,
	     ROOTPAGES << PAGEPWROF2,
	     PROT_READ | PROT_WRITE | PROT_EXEC,
	     MAP_SHARED | MAP_FIXED | MAP_FILE,
	     pstorefd,( off_t ) 0 ) != disk_vaddr )
    {
      error( "failed to map root pages into virtual memory" ) ;
    }
#elif defined(Arch_sun4)||defined(Arch_sun5)
#if defined(Arch_hpux)
  disk_vaddr = (caddr_t) MAX ( 0xC8000000, (unsigned long) min_addr );
#else
  /* 128M in, leaves 384M of mappable address space */
  disk_vaddr = (caddr_t) MAX ( 0x8000000, (unsigned long) min_addr );
#endif
  /* Now map in the two root pages */
  if ( mmap( disk_vaddr,
	     ROOTPAGES << PAGEPWROF2,
	     PROT_READ | PROT_WRITE | PROT_EXEC,
	     MAP_SHARED | MAP_FIXED,
	     pstorefd,( off_t ) 0 ) != disk_vaddr )
    {
      error( "failed to map root pages into virtual memory" ) ;
    }
#elif defined(Arch_win32)||defined(Arch_hpux)
  /* Now map in the two root pages */
  disk_vaddr	= (caddr_t) mmap( min_addr, ROOTPAGES << PAGEPWROF2,
				  PROT_READ | PROT_WRITE | PROT_EXEC,
				  MAP_SHARED | MAP_FIXED,
				  pstorefd, (off_t) 0 );
  if ( disk_vaddr == (caddr_t) -1 ) {
    error( "failed to map root pages into virtual memory" ) ;
  } else {
    munmap ( disk_vaddr, ROOTPAGES << PAGEPWROF2 );
    if ( min_addr == (caddr_t) NULL ) {
    /* 1998/02/24 HK: mmap() and malloc() share the same address
       space and do therefore collide if too many malloc()s are
       done. My temporary solution is to increment the address
       returned from mmap() by 32 MB. This means too that
       allocating more than 32 MB by malloc() will lead to the
       same problem again, i.e. pointers given back by malloc()
       pointing into the mmap()ed address space.
       If the user supplied a min_addr, it is assumed that this
       min_addr contains already an increment. */
      disk_vaddr =
	(caddr_t) ( (char *) disk_vaddr + 256 * 1024 * 1024 );
    }
    disk_vaddr = (caddr_t) MAX ( (unsigned long) disk_vaddr,
				 (unsigned long) min_addr );
    /* Now map in the two root pages */
    if ( mmap( disk_vaddr, ROOTPAGES << PAGEPWROF2,
	       PROT_READ | PROT_WRITE | PROT_EXEC,
	       MAP_SHARED | MAP_FIXED, pstorefd, (off_t) 0 ) !=
	 disk_vaddr ) {
      error( "failed to map root pages into virtual memory" ) ;
    }
  }
#else
  ( void ) strcpy( tmpname,"/tmp/sstore.XXXXXX" ) ;
  f = mkstemp( tmpname );
  ( void ) lseek( f,( off_t ) VASPACESIZE, SEEK_SET ) ;
  ( void ) write( f,( char * ) &zero,4 ) ;
  disk_vaddr	= (caddr_t) mmap( /* ( caddr_t ) 0 */
				  min_addr,
				  VASPACESIZE,
				  PROT_READ | PROT_WRITE | PROT_EXEC,
				  MAP_SHARED, f, ( off_t ) 0 ) ;
  if ( disk_vaddr == ( caddr_t ) -1 ) {
    if ( f >= 0 ) {
      ( void ) close( f ) ;
      f = -1;
      ( void ) unlink( tmpname ) ;
    }
    error( "failed to find contiguous virtual memory\n"
	   "       to map the stable store" ) ;
  }
  map_res = munmap( disk_vaddr,VASPACESIZE ) ;
  if ( map_res != 0 ) {
    if ( f >= 0 ) {
      ( void ) close( f ) ;
      f = -1;
      ( void ) unlink( tmpname ) ;
    }
    error( "failed to unmap the stable store" ) ;
  }	
  if ( f >= 0 ) {
    ( void ) close( f ) ;
    ( void ) unlink( tmpname ) ;
  }
#if defined(Arch_i586)
  if ( min_addr == (caddr_t) NULL ) {
    /* 1997/08/08 HK: mmap() and malloc() share the same address
       space and do therefore collide if too many malloc()s are
       done. My temporary solution is to increment the address
       returned from mmap() by 32 MB. This means too that
       allocating more than 32 MB by malloc() will lead to the
       same problem again, i.e. pointers given back by malloc()
       pointing into the mmap()ed address space.
       If the user supplied a min_addr, it is assumed that this
       min_addr contains already an increment. */
    disk_vaddr	=
      (caddr_t) ( (char *) disk_vaddr + 32 * 1024 * 1024 );
  }
#endif /* Arch_i586 */
  disk_vaddr = (caddr_t) MAX ( (unsigned long) disk_vaddr,
			       (unsigned long) min_addr );
  /* Now map in the two root pages */
  if ( mmap( disk_vaddr, ROOTPAGES << PAGEPWROF2,
#if defined(Arch_i586)
	     /* 1997/06/14 HK: For Linux, write access must be
		granted to the root pages, otherwise writes to
		the root page will raise a SIGSEGV: */
	     PROT_READ | PROT_WRITE | PROT_EXEC,
#else
	     PROT_READ | PROT_EXEC,
#endif
	     MAP_SHARED | MAP_FIXED,
	     pstorefd, (off_t) 0 ) != disk_vaddr )
    {
      error( "failed to map root pages into virtual memory" ) ;
    }
#endif
}

static void unmap_disk()	/* dispose of the logical disk mapping */
{
  int umap_res ;

  if ( disk_vaddr != ( caddr_t ) -1 ) {
    umap_res = munmap( disk_vaddr,VASPACESIZE ) ;
    disk_vaddr = ( caddr_t ) -1 ;
    if ( umap_res != 0 ) {
      error( "failed to unmap the stable store" ) ;
    }
  }
}


static void FLSet( psint blockNo )
{
  psint w,b ;

  w = BLWRD( blockNo ) ;
  b = BBIT( blockNo ) ;

  freeCount-- ;
  freeList[ w ] |= b ;
}

static void RLClear( psint blockNo )
{
  psint w,b ;

  w = BLWRD( blockNo ) ;
  b = BBIT( blockNo ) ;

  toFreeCount++ ;				/* "n" shadowed pages */
  reuseList[ w ] &= ~b ;
}

static void FLClear( psint blockNo )
{
  psint w,b ;

  w = BLWRD( blockNo ) ;
  b = BBIT( blockNo ) ;

  freeCount++ ;
  freeList[ w ] &= ~b ;
}

static psint FLtest( psint blockNo )
{
  psint w,b ;

  w = BLWRD( blockNo ) ;
  b = BBIT( blockNo ) ;
  return( ( freeList[ w ] & b ) ? PSTRUE : PSFALSE ) ;
}

static psint FLtestSet( psint blockNo )
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


static psint FLBitSearch( psint w, psint bitMax )
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
  i = lookFrom ;

  while ( found == ( psint ) 0 && i < completeWords )
    {
      if ( freeList[ i ] != ( psint ) -1 )
	{
	  found = FLBitSearch( i,( psint ) 32 ) ;
	} else
	  {
	    if ( lookFrom < completeWords ) lookFrom++ ;
	  }
      i++ ;
    }

  if ( found == ( psint ) 0 ) found = FLBitSearch( completeWords,finalBits ) ;
  if ( found == ( psint ) 0 ) found = FLsize ;
  return ( found ) ;
}
			
static void clear_freelist()
{
  psint cnt,*tptr,*tptr2 ;

  cnt = FLMAX >> ( psint ) 5 ;			/* div 8 to get bytes */
  tptr = freeList ;
  tptr2 = reuseList ;
  while ( cnt-- > ( psint ) 0 )
    {
      *tptr++ = ( psint ) 0 ;			/* freeList is cleared to 0 */
      *tptr2++ = ( psint ) -1 ;		/* reuseList is cleared to all 1's */
    }
}

/* clear freeList entries for pages on the reuseList */
static void reset_freelist()
{
  psint cnt,*tptr,*tptr2 ;

  cnt = FLMAX >> ( psint ) 5 ;			/* div 32 to get bytes */
  tptr = freeList ;
  tptr2 = reuseList ;
  while ( cnt-- > ( psint ) 0 )
    {
      *tptr = *tptr & *tptr2 ;		/* and the reuse & free lists to free blocks on the reuselist */
      tptr++ ;				/* inc the tptr */
      *tptr2++ = ( psint ) -1 ;		/* reuseList is cleared to all 1's */
    }

  lookFrom = DATABLOCKSTART >> 5 ;			/* ( ( 2 * pte ) + 2 ) /32 */
}

static void setup_freelist()
{
  /* 1998/02/17 HK:
     extern char *malloc() ;
     */
  psint	i ;

  FLsize = store_length >> PAGEPWROF2 ;	/* Count number of disk pages */

  freeCount = FLsize ;			/* All blocks free */
  toFreeCount = ( psint ) 0 ;
  freeList = ( psint * ) malloc( FLMAX >> 3 ) ;	/* create a bitmap */
  if ( freeList == ( psint * ) 0 )
    {
      error( "failed to initialise the store file freelist" ) ;
    }

  reuseList = ( psint * ) malloc( FLMAX >> 3 ) ;	/* create a bitmap */
  if ( reuseList == ( psint * ) 0 )
    {
      error( "failed to initialise the store file reuse list" ) ;
    }

  clear_freelist() ;

  lookFrom = DATABLOCKSTART >> 5 ;	/* ( ( 2 * pte ) + 2 ) /32 */
  for ( i = ( psint ) 0 ; i < DATABLOCKSTART ; i++ )
    {
      FLSet( i ) ;			/* Slobs method */
    }
}


/* Allocate a number of free blocks reserved for coping with shadowing */
static void allocate_shadowBlocks()
{
  psint	shortFall,result ;

  /* Guesstimate of no. of required blocks for shadowing */
  needCount = root_page.no_mod_pages ;
  shortFall = needCount - freeCount ;

  if ( shortFall > 0 )
    {
      if ( ( FLsize + shortFall ) < FLMAX ) 
	{
#if ! (defined(Arch_sun4)||defined(Arch_sun5))	/* 1996/09/16 HK */
	  result = extend_store( shortFall ) ;
	  if ( result > FLsize )
	    {
	      FLsize = result ;			/* Extend free list */
	      freeCount += result - FLsize ; 	/* Increment the free count */
	    }
#else
	  ( void ) lseek( pstorefd,( off_t ) 0,SEEK_END ) ; /* End of file - ignore errors?!? */
	  result = 0 ;
	  while( shortFall-- > 0 )
	    {
	      /* Don't care what's on the disk block - use dummy address */
	      if ( write( pstorefd,zeroPage,BPAGESIZE ) != BPAGESIZE )
		{
		  shortFall = 0 ;
		} else
		  {
		    result += BPAGESIZE ;
		  }
	    }
	  if ( result >= BPAGESIZE )
	    {
	      result = result >> PAGEPWROF2 ;		/* File was extended by this many complete pages */
	      FLsize += result ;			/* Extend free list */
	      freeCount += result ; 		/* Increment the free count */
	    }
#endif	/*Arch_sun4*/
	}
    }
}

/* Given a page return the address of its associated pte */

/* divide page address by pagesize to get the pte index */
#define	pteIndex( p )		( p >> PAGEPWROF2 )

/* the pindex is in words - divide by pagesize in words */
#define	pte2Index( pindex )	( pindex >> WPAGEPWROF2 )
/* read a page's block number given the pte index */
#define getPteBlockNo( pindex )	( pte[ pindex ] & LOWER24 )

/* read a page's flags given the pte index */
#define getPteFlags( pindex )	( pte[ pindex ] & UPPER8 )

/* read a page's P and W flags given the pte index */
#define getPtePWFlags( pindex )	( pte[ pindex ] & SAVE )

/* check a pte entry can be changed given its pte index */
#define	checkPte( pindex )	if ( !root_page.pteModified[ pte2Index( pindex ) ] ) shadow_pte( pte2Index( pindex ) )

/* clear a page's entry given the pte index */
#define zeroPte( pindex )	checkPte( pindex ) ; pte[ pindex ] = ( psint ) 0

/* set a page's entry given the pte index and new values */
#define	setPteEntry( pindex,blockNo,flags )\
checkPte( pindex ) ; pte[ pindex ] = blockNo | flags

/* set a page's block number given the pte index */
#define	setPteBlockNo( pindex,blockNo )	setPteEntry( pindex,blockNo,getPteFlags( pindex ) )

/* set a page's flags given the pte index */
#define	setPteFlags( pindex,flags )\
setPteEntry( pindex,getPteBlockNo( pindex ),flags )

static void shadow_pte( psint offset )
{
  psint	p,newPteBlock,result ;
	
  p = ( offset + ROOTPAGES ) << PAGEPWROF2 ;

  if ( root_page.pteWhere[ offset ] < PT1PAGES + ROOTPAGES ) {
    newPteBlock = offset + PT1PAGES + ROOTPAGES ;
  } else {
    newPteBlock = offset + ROOTPAGES ;
  }
  result = move_page( p,newPteBlock ) ;
  if ( !result ) error( "failed to shadow copy a pte page" ) ;

  root_page.pteWhere[ offset ] = newPteBlock ;
  root_page.pteModified[ offset ] = PSTRUE ;
}

/* re mmap the data page */
static void remmap_page( psint target,psint source,psint protection )
{
  caddr_t mm_res,start ;

  start = ( caddr_t )( ( char * ) disk_vaddr + target ) ;
  source <<= PAGEPWROF2 ;
 
  /* we dont really care if this works or not.... */
  munmap( start, BPAGESIZE ) ;
  mm_res = (caddr_t) mmap( start,BPAGESIZE,( int ) protection,
			   MAP_SHARED|MAP_FIXED|MAP_FILE,
			   pstorefd,( off_t ) source ) ;
  if ( mm_res != start ) {
    char	szMessage [ 256 ];
    int	nErrNo	= errno;
    sprintf ( szMessage, "failed to re-mmap the shadow copied page.\n"
	      "       Expected addr 0x%X, received addr 0x%X, offset 0x%X",
	      (unsigned int) start, (unsigned int) mm_res,
	      (unsigned int) source );
    /* 1998/07/09 HK: Debug: */
    errno	= nErrNo;
    error( szMessage ) ;
  }
}

static void unmap_page( psint p )
{
  int	umap_res ;
  caddr_t start ;
  psint	pindex,blockNo ;

  start = ( caddr_t )( ( char * ) disk_vaddr + p ) ;

  umap_res = mprotect( start,BPAGESIZE,PROT_NONE ) ;
  if ( umap_res != 0 ) {
    /* 1997/08/14 HK: Linux returns sometimes with an error
       code; pass errno to the error message: */
    char	szMessage [ 128 ];
    sprintf ( szMessage,
	      "failed to remap some data pages as inaccessible;\n"
	      "       offset 0x%X",
	      (unsigned int) p );
    error ( szMessage );
  }

  pindex = pteIndex( p ) ;
  blockNo = getPteBlockNo( pindex ) ;
  setPteEntry( pindex,( psint ) 0,getPteFlags( pindex ) & BLOCKLISTED ) ;
  /* mark the old block as free from the next checkpoint */
  RLClear( blockNo ) ;
}
	
static psint map_page( psint p,psint flags )
{
  psint	pindex,blockNo ;

  if ( needCount >= freeCount )
    {
      /* Force a file extension */
      blockNo = FLsize ;
    } else blockNo = FLSearch() ;

  /* No free disk blocks - extend file */
  if ( blockNo == FLsize )
    {
      psint	to ;

      if ( FLsize == FLMAX )
	/* Store file is max size to support the max VA size */
	return( PSFALSE ) ;
#if ! (defined(Arch_sun4)||defined(Arch_sun5))	/* 1996/09/16 HK */
      if ( extend_store( 1 ) != 1 ) return( PSFALSE ) ;
#else
      to = blockNo << PAGEPWROF2 ;
      if ( lseek( pstorefd,( off_t ) to,SEEK_SET ) != ( off_t ) to )
	return( PSFALSE ) ;
      if ( write( pstorefd,zeroPage,BPAGESIZE ) != BPAGESIZE )
	return( PSFALSE ) ;
#endif	/*Arch_sun4*/
      /* Ok - so freelist size is extended by one bit -
	 already marked unused */
      FLsize++ ;
      /* One more free block - not for long  */
      freeCount++ ;
    }
  remmap_page( p & PAGENUMBER,blockNo,
	       ( psint )( PROT_WRITE | PROT_READ | PROT_EXEC ) ) ;
  FLSet( blockNo ) ;
  /* Record new mapping in page tables */
  pindex = pteIndex( p ) ;
  if ( !( getPteFlags( pindex ) & BLOCKLISTED ) ) {
    /* Record the new block as needing checked by sync_pages */
    blockList[ blockCount++ ] = pindex ;
  }
  setPteEntry( pindex,blockNo,flags | BLOCKLISTED ) ;
  return( PSTRUE ) ;
}

/* set the memory protection to read/write on the pages p1 to p2 */
static void remap_write( psint p1,psint p2 )
{
  int mp_res,length ;
  caddr_t start ;

  start = ( caddr_t )( ( char * ) disk_vaddr + p1 ) ;
  length = p2 - p1 + BPAGESIZE ;

  mp_res = mprotect( start,length,PROT_READ|PROT_EXEC|PROT_WRITE ) ;
  if ( mp_res != 0 ) {
    error( "failed to remap some data pages as read/write" ) ;
  }
}

static void memcopy( psint * src,psint * dst,psint nwords )
{
	while( nwords-- > 0 )
	{
		*dst++ = *src++ ;
	}
}

static void sync_alpha_page( psint firstp,psint npages )
{
  psint i,pindex,to,blockNo ;

  pindex = firstp ;

  for ( i = 0 ; i < npages ; i++ )
    {	
      blockNo = getPteBlockNo( pindex ) ;
      to = blockNo << PAGEPWROF2 ;
      if ( lseek( pstorefd,( off_t ) to,SEEK_SET ) != ( off_t ) to )
	{
	  error( "failed to synchronise a page with the store file" ) ;
	}
      if ( write( pstorefd,( char * ) disk_vaddr + ( pindex << PAGEPWROF2 ),
		  BPAGESIZE ) != BPAGESIZE )
	{
	  error( "failed to synchronise a page with the store file" ) ;
	}
      pindex++ ;
    }
}

/* sync the vm copy of the given pages with the disk */
/* and read protect the page */
static void sync_page( psint firstp,psint npages )
{
  int m_res ;
  caddr_t start ;

#ifdef	Arch_alpha
  sync_alpha_page( firstp,npages ) ;
#else
  start = ( caddr_t )( ( char * ) disk_vaddr + ( firstp << PAGEPWROF2 ) ) ;
  npages <<= PAGEPWROF2 ;
#ifdef	Arch_sun4
  m_res = msync( start,( int ) npages,0 ) ;
#else
  m_res = msync( start,( int ) npages,MS_SYNC ) ;
#endif	/*Arch_sun4*/
  if ( m_res != 0 ) {
      error( "failed to synchronise a page with the store file" ) ;
  }
  m_res = mprotect( start,npages,PROT_READ|PROT_EXEC ) ;
  if ( m_res != 0 ) {
    error( "failed to remap a page as read only" ) ;
  }
#endif	/*Arch_alpha*/
}

static void sync_alpha_root()
{
  /* reset file pos to the start */
  ( void ) lseek( pstorefd,( off_t ) 0,SEEK_SET ) ;
  if ( write( pstorefd,disk_vaddr,BPAGESIZE * ROOTPAGES ) != BPAGESIZE * ROOTPAGES )
    {
      error( "failed to synchronise the root pages with the store file" ) ;
    }
}

/* sync the vm copy of the root pages with the disk */
static void sync_root()
{
  int ms_res ;

#ifdef	Arch_alpha
  sync_alpha_root() ;
#else
#ifdef	Arch_sun4
  ms_res = msync( disk_vaddr,BPAGESIZE * ROOTPAGES,0 ) ;
#else
  ms_res = msync( disk_vaddr,BPAGESIZE * ROOTPAGES,MS_SYNC ) ;
#endif	/*Arch_sun4*/
  if ( ms_res != 0 )
    {
      error( "failed to synchronise the root pages with the store file" ) ;
    }
#endif	/* Arch_alpha */
}

/* copy root page from the store file - to the local copy */
static void read_root()
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

/* write copy of root page to the store file */
static void write_root()
{
  register psint cnt ;
  register psint average,i,j ;

  /* One more checkpoint */
  root_page.no_syncs += ( psint ) 1 ;
  i = root_page.no_syncs ;
  j = root_page.no_mod_pages ;
  average = ( ( i - ( psint ) 1 ) * j + modifiedCount ) / i ;
  root_page.no_mod_pages = average ;
  modifiedCount = ( psint ) 0 ;
  freeCount += toFreeCount ;
  toFreeCount = ( psint ) 0 ;
  needCount = average ;

  root_page.date1 += ( psint ) 1 ;
  root_page.date2 = root_page.date1 ;

  memcopy( (psint *)( &root_page ),(psint *) root,WPAGESIZE ) ;

  /* msync the root pages *//********* DO WE NEED THIS IF WRITE IS SYNC. **/
  sync_root() ;
  /* Swap root pages */
  root = root == root1 ? root2 : root1 ;
}

static psint move_page( psint from, psint to )
{
#if ! (defined(Arch_sun4)||defined(Arch_sun5))	/* 1996/09/16 HK */
  psint topos ;
  psint buffer[ WPAGESIZE ] ;

  memcopy((psint *)((char *) disk_vaddr + from),buffer,WPAGESIZE ) ;

  topos = to << PAGEPWROF2 ;
  /* No free disk blocks - extend file */
  if ( to == FLsize )
    {
      /* Store file is full and same size as VA space */
      if ( FLsize == FLMAX ) return ( PSFALSE ) ;

      if ( extend_store( 1 ) != 1 )
	return( PSFALSE ) ;

      /* Ok - so freelist size is extended by one bit -
	 already marked unused */
      FLsize++ ;
      /* One more free block */
      freeCount++ ;
    }
  /* mmap shadow copy to new disk block */
  remmap_page( from,to,(psint)(PROT_WRITE|PROT_READ|PROT_EXEC) ) ;
  memcopy(buffer,(psint *)((char *) disk_vaddr + from),WPAGESIZE ) ;
  return ( PSTRUE ) ;
#else
  psint topos ;

  topos = to << PAGEPWROF2 ;
  if ( lseek( pstorefd,( off_t ) topos,SEEK_SET ) != ( off_t ) topos )
    return( PSFALSE ) ;
  /* No free disk blocks - extend file */
  if ( to == FLsize )
    {
	/* Store file is full and same size as VA space */
      if ( FLsize == FLMAX ) return ( PSFALSE ) ;

      if ( write( pstorefd,( char * ) disk_vaddr + from,BPAGESIZE ) !=
	   BPAGESIZE )
	return( PSFALSE ) ;

      /* Ok - so freelist size is extended by one bit -
	 already marked unused */
      FLsize++ ;
      /* One more free block */
      freeCount++ ;
    } else
      {
	if ( write( pstorefd,( char * ) disk_vaddr + from,BPAGESIZE ) !=
	     BPAGESIZE )
	  return( PSFALSE ) ;
      }
  /* mmap shadow copy to new disk block */
  remmap_page( from,to,(psint)(PROT_WRITE|PROT_READ|PROT_EXEC) ) ;
  return ( PSTRUE ) ;
#endif	/*Arch_sun4*/
}

/* re mmap the data page */
static void remmap_page_restore( psint target,psint source,
				 psint protection,psint length )
{
  caddr_t mm_res,start ;

  start = ( caddr_t )( ( char * ) disk_vaddr + target ) ;
  source <<= PAGEPWROF2 ;
  length <<= PAGEPWROF2;

  /* we dont really care if this works or not.... */
  munmap( start, length ) ;
  mm_res = (caddr_t) mmap( start,length,( int ) protection,
			   MAP_SHARED|MAP_FIXED|MAP_FILE,
			   pstorefd,( off_t ) source );
  if ( mm_res != start ) {
    char	szMessage [ 256 ];
    int	nErrNo	= errno;
    sprintf ( szMessage, "failed to re-mmap the shadow copied page(s).\n"
	      "       Expected addr 0x%X, received addr 0x%X",
	      (unsigned int) start, (unsigned int) mm_res );
    errno	= nErrNo;
    error( szMessage ) ;
  }
}

static void restore_pages()
{
  psint   i,blockNo,p, j, firstAddress, firstPage, lastBlock ;
  psint   runCount ;
  /*  initialise the address of the pagetable */
  pte = ( psint * )( ( char * ) disk_vaddr + ( ROOTPAGES << PAGEPWROF2 ) ) ;
  blockList = ( psint * )( ( char * ) disk_vaddr + DATAPAGESTART ) ;
  blockCount = ( psint ) 0 ;

  p = ROOTPAGES << PAGEPWROF2 ;
  for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ ) {
      /* All pte pages will be mapped in as read-only */
      blockNo = root_page.pteWhere[ i ]  ;
      remmap_page( p,blockNo,( psint )( PROT_READ | PROT_EXEC ) ) ;
      p += BPAGESIZE ;
    }

  /* address of next page to map */
  p = DATAPAGESTART ;
  /* for loop looks at every entry in pte */
  for ( i = ROOTPAGES + PT1PAGES ; i < ( PT1PAGES << WPAGEPWROF2 ) ; i++ ) {
    switch( ( int ) getPteFlags( i ) ) {

      case ( int ) WRITTEN:
      case ( int ) PROTECT :
	FLSet( getPteBlockNo( i ) ) ;
	runCount = ( psint ) 1 ;
	lastBlock = getPteBlockNo( i )  ;
	firstAddress = p ;
	firstPage = i ;
	i++ ;
	while( i < ( PT1PAGES << WPAGEPWROF2 )
	       && getPteFlags( i ) & ( PROTECT | WRITTEN )
	       && getPteBlockNo( i ) == lastBlock + 1 )
	  {
	    FLSet( getPteBlockNo( i ) ) ;
	    lastBlock++ ;
	    runCount++ ;
	    p += BPAGESIZE ;
	    i++ ;
	  }
	i-- ;
	remmap_page_restore( firstAddress, getPteBlockNo( firstPage ),
			     ( psint ) ( PROT_READ | PROT_EXEC ),
			     runCount ) ;
	for ( j = 0 ; j < runCount ; j++ ) {
	    if ( getPteFlags( firstPage + j ) == WRITTEN ) {
		caddr_t start =  ( caddr_t )( ( char * ) disk_vaddr +
					      ( firstAddress +
						( j << PAGEPWROF2 ) ) ) ;
		int mp_res = mprotect( start, BPAGESIZE,
				       PROT_READ | PROT_EXEC | PROT_WRITE ) ;
		if ( mp_res != 0 ) {
		  char szError [ 256 ];
		  sprintf( szError,
			   "failed to change permissions on page %d",
			   j );
		  error( szError );
		}
	      }
	}
	break ;

      case ( int ) DONTNEED :
	break ;

      default :
	error( "Page table entry found that"
	       " was not DONTNEED or WRITTEN or PROTECTED" ) ;
	break ;
      }
    p += BPAGESIZE ;
  }
}

static void sync_pages()
{
  psint 	i,pindex,firstp,npages ;

  /* 0,0 never matches any real pages */
  firstp = ( psint ) 0 ; npages = ( psint ) 0 ;

  /* look at potentially modified pages in block order */
  for ( i = 0 ; i < blockCount ; i++ ) {

    /* index of next page to sync? */
    pindex = blockList[ i ] ;
    switch( ( int ) getPtePWFlags( pindex ) ) {

    case ( int ) PROTECT :
      setPteFlags( pindex,PROTECT ) ;
      break ;

    case ( int ) WRITTEN :
      setPteFlags( pindex,WRITTEN ) ;
      break ;

    case ( int ) SAVE :	
      /* Write back data page to disk and mark as read-only */
      if ( firstp + npages == pindex )
	{
	  /* we have an extra page in the run */
	  npages++ ;
	} else {
	  /* break in runlength encoding - sync previous run */
	  if ( npages != ( psint ) 0 )
	    {
	      sync_page( firstp,npages ) ;
	    }
	  firstp = pindex ; npages = ( psint ) 1 ;
	}
      /* page is now readonly */
      setPteFlags( pindex,PROTECT ) ;
      /* One more data page written */
      modifiedCount++ ;
      break ;

    default :
      setPteFlags( pindex,DONTNEED ) ;
      break ;
    }
  }
  if ( npages != ( psint ) 0 ) {
      sync_page( firstp,npages ) ;
  }
  blockCount = ( psint ) 0 ;

#ifdef	Arch_alpha
  /* write out the pagetable pages */
  for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ ) {
    psint to,blockNo ;

    if ( root_page.pteModified[ i ] ) {

      blockNo = root_page.pteWhere[ i ] ;
      to = blockNo << PAGEPWROF2 ;

      if ( lseek( pstorefd,( off_t ) to,SEEK_SET ) != ( off_t ) to )
	error( "failed to synchronise a PTE page with the store file" ) ;

      if ( write( pstorefd,
		  ( char * ) disk_vaddr + ( ( i + ROOTPAGES ) << PAGEPWROF2 ),
		  BPAGESIZE ) != BPAGESIZE ) {
	error( "failed to synchronise a page with the store file" ) ;
      }
    }
  }

#else
  /* write out the pagetable pages - assume OS does it efficiently */
  sync_page( ROOTPAGES,PT1PAGES ) ;
#endif	/* Arch_alpha*/

  for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ ) {
    root_page.pteModified[ i ] = PSFALSE ;
  }
}

#if defined(Arch_sun5)||defined(Arch_hpux)	/* 1996/09/11 HK */
static void page_fault( int sig, siginfo_t * sip,void *uap )
#elif defined(Arch_i586)||defined(Arch_win32)
     /* 1997/06/14 HK: Added Linux
        1998/02/17 HK: Added Windows/NT */
static void page_fault(int sig, struct sigcontext_struct sc)
#else
static void page_fault( int sig, int code,struct sigcontext *scp,char *addr )
#endif
{
  register psint svmaddr,cde;
#if defined(Arch_sun5)||defined(Arch_hpux)		/* 1996/09/11 HK */
  register psint		code;
  register char			*addr = (char *) sip->si_addr;
  register psint		errn;
  register struct sigaction	*pSigAction;
#elif defined(Arch_i586)||defined(Arch_win32)
  /* 1997/06/14 HK: Added Linux
     1998/02/17 HK: Added Windows/NT */
  register psint		code = 0;
  register char			*addr = (char *) sc.cr2;
#endif

#if defined(Arch_i586)||defined(Arch_win32)	/* 1998/02/17 HK */
  { /* ignore the offending signal for now */
    struct sigaction	SigAction;

    memset ( &SigAction, 0, sizeof ( SigAction ) );
    SigAction.sa_handler	= SIG_IGN;
    sigaction ( sig, &SigAction, NULL );
  }
#elif !defined(Arch_sun5)&&!defined(Arch_hpux)	/* 1996/09/11 HK */
  signal( sig,SIG_IGN );	/* ignore the offending signal for now */
#endif
#if defined(Arch_mips)
  addr = (char *)( scp->sc_badvaddr ) ;
  svmaddr = ( psint )( addr - ( char * ) disk_vaddr ) ;
  if ( svmaddr >= USERPAGESTART &&
       svmaddr < VASPACESIZE &&
       getPteFlags( pteIndex( svmaddr ) ) == PROTECT ) {
    /* user must fix things up - we get a loop if not.... */
    user_write_fault( svmaddr,( psint * ) sig ) ;
  } else {
    user_page_fault( sig,code,sig,addr ) ;
  }
#elif defined(Arch_alpha)
  addr = (char *)( scp->sc_traparg_a0 ) ;
  svmaddr = ( psint )( addr - ( char * ) disk_vaddr ) ;
  if ( ( (unsigned long) addr >= (unsigned long) disk_vaddr ) &&
       svmaddr >= USERPAGESTART &&
       svmaddr < VASPACESIZE &&
       getPteFlags( pteIndex( svmaddr ) ) == PROTECT ) {
    /* user must fix things up - we get a loop if not.... */
    user_write_fault( svmaddr,( psint * ) sig ) ;
  } else {
    user_page_fault( sig,code,sig,addr ) ;
  }
#else
  svmaddr = ( psint )( addr - ( char * ) disk_vaddr ) ;
#if defined(Arch_sun5)||defined(Arch_hpux)	/* 1996/09/11 HK */
  code = sip->si_code;
  errn = sip->si_errno;
  if ( ( code == SEGV_MAPERR || code == SEGV_ACCERR ) &&
       ( (unsigned long) addr >= (unsigned long) disk_vaddr ) &&
       ( svmaddr >= USERPAGESTART ) && ( svmaddr < VASPACESIZE ) &&
       ( getPteFlags( pteIndex( svmaddr ) ) == PROTECT ) ) {
    /* user must fix things up - we get a loop if not.... */
    user_write_fault( svmaddr,( psint * ) sig ) ;
  } else {
    user_page_fault( sig,code,sig,addr ) ;
  }
#elif defined(Arch_i586)||defined(Arch_win32)
  /* 1997/06/14 HK: Added Linux */
  /* 1998/02/17 HK: Added Windows NT */
  if ( ( (unsigned long) addr >= (unsigned long) disk_vaddr ) &&
       svmaddr >= USERPAGESTART && svmaddr < VASPACESIZE &&
       getPteFlags( pteIndex( svmaddr ) ) == PROTECT ) {
      /* user must fix things up - we get a loop if not.... */
      user_write_fault( svmaddr,( psint * ) sig ) ;
  } else {
    user_page_fault( sig,code,sig,addr ) ;
  }
#else
  cde = SEGV_CODE( code ) ;
  /* Obj error - only in pre4.1.2 systems - BUS err instead of SEGV */
  if ( ( ( cde == SEGV_OBJERR && SEGV_ERRNO( code ) == EINTR ) ||
	 cde == SEGV_PROT ) &&
       svmaddr >= USERPAGESTART && svmaddr < VASPACESIZE &&
       getPteFlags( pteIndex( svmaddr ) ) == PROTECT ) {
    /* user must fix things up - we get a loop if not.... */
    user_write_fault( svmaddr,( psint * ) sig ) ;
  } else {
      user_page_fault( sig,code,sig,addr ) ;
  }
#endif	/*Arch_sun5*/		/* 1996/09/16 HK */
#endif	/*Arch_mips*/
#if defined(Arch_i586)||defined(Arch_win32)	/* 1998/02/17 HK */
  { /* reenable the offending signal */
    struct sigaction	SigAction;

    memset ( &SigAction, 0, sizeof ( SigAction ) );
    SigAction.sa_handler	= page_fault;
    SigAction.sa_flags		= SA_ONESHOT;
    sigaction ( sig, &SigAction, NULL );
  }
#elif !defined(Arch_sun5)&&!defined(Arch_hpux)	/* 1996/09/11 HK */
  /* reenable the offending signal */
  signal( sig,page_fault ) ;	
#endif
}

static void setup_root_pages()
{
  /* address of the start of the disk in vm - first root page */
  root1 = ( root_pg * )( disk_vaddr ) ;
  /* address of the second root page */
  root2 = root1 + 1 ;

  /* find the valid root page */
  if ( root1->date1 == root1->date2 ) {
    if ( root2->date1 != root2->date2 ) {
      root = root1 ;
    } else {
      root = root1->date1 > root2->date1 ? root1 : root2 ;
    }
  } else {
    if ( root2->date1 == root2->date2 ) {
      root = root2 ;
    } else {
      error( "root pages both corrupt - tough !" ) ;
    }
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
  if ( root->data_start != DATAPAGESTART )
    error( "store length recorded by the store file is incompatible with the stable store implementation" ) ;
  if ( root->data_end != VASPACESIZE - WORDSIZE )
    error( "store length recorded by the store file is incompatible with the stable store implementation" ) ;
}

static void inc_restart_clock()
{
	root_page.restart_clock++ ;
}

/* open the file containing the store */
static void init_store( const char *dirname,
			caddr_t min_addr )
{
  /* lock and open the stable store */
  open_disk( dirname ) ;
  /* Find suitable VA and mmap root pages */
  get_suitable_va( min_addr ) ;
  /* find out which root page is uptodate */
  setup_root_pages() ;
  /* find out how big the store actually is? */
  setup_length_info() ;
  /* make a local copy of the root page */
  read_root() ;
  /* setup freespace list from scratch */
  setup_freelist() ;
  /* re-map pages from last checkpoint */
  restore_pages() ;
  /* Get some disk blocks for shadows if needed */
  allocate_shadowBlocks() ;
  /* increment the restart clock */
  inc_restart_clock() ;
  /* Set signal handlers */
  SS_set_signal_handler ();
  /* startup the user's restore code */
  user_restore() ;
} /* init_store */

void SS_close()					/* finish using the shadow store */
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  unmap_disk() ;	/* forget the vm copy of the stable store */
  close_disk() ;	/* finish using the disk */
  CATCH_EXCEPTION;
}

void SS_stabilise()	/* secure all changed pages to the store file as an atomic action */
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  user_save() ;		/* execute the user's save code */

  sync_pages() ;	/* make sure the active data pages are uptodate on the disk */
  write_root() ;	/* write the root page */
  reset_freelist() ;	/* free the blocks replaced by shadow copying */

  user_restore() ;	/* execute the user's restorey code */
  CATCH_EXCEPTION;
}

/* should give physical address of the word */
psint *SS_real_address( psint pos )
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  return( ( psint * )( ( char * ) disk_vaddr + pos ) ) ;
}

/* return word as address pos */
psint SS_read_word( psint pos )
{
  psint *addr ;
  psint	word;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  addr	= ( psint * )( ( char * ) disk_vaddr + pos ) ;
  word	= *addr;
  CATCH_EXCEPTION;
  return( word ) ;
}

/* copy n words from 'pos' in the store file to the buffer, pos is in bytes */
void SS_read_words( psint pos,psint * buffer,psint nwords )
{
  psint *addr ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
  while( nwords-- > ( psint ) 0 )
    {
      *buffer++ = *addr++ ;
    }
  CATCH_EXCEPTION;
}

/* write theword to address pos */
void SS_write_word( psint pos,psint theword )
{
  psint *addr ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
  *addr = theword ;
  CATCH_EXCEPTION;
}

/* copy n words from the buffer to 'pos' in the store file, pos is in bytes */
void SS_write_words( psint pos,psint * buffer,psint nwords )
{
  psint *addr ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
  while( nwords-- > ( psint ) 0 )
    {
	  *addr++	= *buffer++ ;
    }
  CATCH_EXCEPTION;
}

psint SS_first_address()	/* what is the first usable address */
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  return ( USERPAGESTART ) ;
}

psint SS_last_address()					/* what is the address of the last usable word */
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  return( VASPACESIZE - WORDSIZE ) ;
}

/* save version number for stable heap */
void SS_set_sheap_version( psint vnum )
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  root_page.sheapMagic = vnum ;
  CATCH_EXCEPTION;
}

/* return version number for stable heap */
psint SS_get_sheap_version()
{
  psint	magic;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  magic	= root_page.sheapMagic;
  CATCH_EXCEPTION;
  return magic;
}

/* save version number for stable heap */
void SS_set_restart_clock( psint tim )
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  root_page.restart_clock = tim ;
  CATCH_EXCEPTION;
}

psint SS_get_restart_clock()				/* return version number for stable heap */
{
  psint	clock;
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  clock	= root_page.restart_clock;
  CATCH_EXCEPTION;

  return clock;
}

void SS_configuration( struct stablestore_configuration * config )
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  config->configuration_flags = REAL_ADDRESS | SHADOW_PAGING ;
  config->page_size = BPAGESIZE ;
}

void SS_statistics( struct stablestore_statistics * stats )
{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
}

static psint shadow_copy( psint page,psint flags )
{
  psint	nextdst,result ;
	
  nextdst = FLSearch() ;	/* Find first free blck */
  result = move_page( page,nextdst ) ;
  if ( result )
    {
      psint pindex ;

      /* Mark it as being used */
      FLSet( nextdst ) ;
      pindex = pteIndex( page ) ;
      /* mark the old block as free from the next checkpoint */
      RLClear( getPteBlockNo( pindex ) ) ;
      needCount-- ;
      if ( needCount < ( psint ) 0 ) needCount = ( psint ) 0 ;
      if ( !( getPteFlags( pindex ) & BLOCKLISTED ) )
	{
	  /* Record the new block as needing checked by sync_pages */
	  blockList[ blockCount++ ] = pindex ;
	}
      /* Record new mapping in page tables */
      setPteEntry( pindex,nextdst,flags | BLOCKLISTED ) ;
    }
  return( result ) ;
}

psint SS_saveVM( psint a1,psint a2 )
{
  psint pindex,p1,p2 ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  /* make sure a1 <= a2 */
  if ( a1 > a2 ) {
      psint t ;

      t = a1 ;
      a1 = a2 ;
      a2 = t ;
  }

  if ( a1 < USERPAGESTART || a2 >= VASPACESIZE ) {
    error( "SS_saveVM passed addresses outwith the user's address space" ) ;
  }

  /* get the page addresses */
  p1 = a1 & PAGENUMBER ; pindex = pteIndex( p1 ) ;
  p2 = a2 & PAGENUMBER ;

  while ( p1 <= p2 ) {
    switch( ( int ) getPtePWFlags( pindex ) ) {

    case ( int ) PROTECT :
      if ( !shadow_copy( p1,SAVE ) )
	return( PSFALSE ) ;
      break ;

    case ( int ) WRITTEN :
      if ( !( getPteFlags( pindex ) & BLOCKLISTED ) ) {
	blockList[ blockCount++ ] = pindex ;
      }
      setPteFlags( pindex,SAVE | BLOCKLISTED ) ;
      break ;

    case ( int ) SAVE :
      break ;

    default :
      if ( !map_page( p1,SAVE ) )
	return( PSFALSE ) ;
      break ;
    }
    p1 += BPAGESIZE ; pindex++ ;
  }

  CATCH_EXCEPTION;
  return( PSTRUE ) ;
}

psint SS_scratchVM( psint a1,psint a2 )
{
  psint pindex,p1,p2,partial_p2 ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  /* make sure a1 <= a2 */
  if ( a1 > a2 ) {
    psint t ;

    t = a1 ;
    a1 = a2 ;
    a2 = t ;
  }

  if ( a1 < USERPAGESTART || a2 >= VASPACESIZE ) {
      error( "SS_scratchVM passed addresses outwith"
	     " the user's address space" ) ;
  }

  p1 = a1 & PAGENUMBER ; pindex = pteIndex( p1 ) ;
  p2 = a2 & PAGENUMBER ; 

  /* are the first and last pages complete? */
  if ( ( a1 & PAGEINDEX ) != ( psint ) 0 ) {
    switch( ( int ) getPtePWFlags( pindex ) ) {

    case ( int ) PROTECT :
      if ( !shadow_copy( p1,SAVE ) ) return( PSFALSE ) ;
      break ;

    case ( int ) WRITTEN :
      break ;

    case ( int ) SAVE :
      break ;

    default :
      if ( !map_page( p1,WRITTEN ) ) return( PSFALSE ) ;
      break ;
    }
    p1 += BPAGESIZE ; pindex++ ;
  }
	
  if ( ( p1 <= p2 ) && ( ( a2 & PAGEINDEX ) != LASTWORD ) ) {
    partial_p2 = PSTRUE ; p2 -= BPAGESIZE ;
  } else {
    partial_p2 = PSFALSE ;
  }

  /* fix up the complete pages in the range a1->a2 */
  while ( p1 <= p2 ) {
    switch( ( int ) getPtePWFlags( pindex ) ) {

    case ( int ) PROTECT :
      if ( !shadow_copy( p1,WRITTEN ) ) {
	return( PSFALSE ) ;
      }
      break ;

    case ( int ) WRITTEN :
      break ;

    case ( int ) SAVE :
      setPteFlags( pindex,WRITTEN | BLOCKLISTED ) ;
      break ;

    default :
      if ( !map_page( p1,WRITTEN ) ) {
	return( PSFALSE ) ;
      }
      break ;
    }
    p1 += BPAGESIZE ; pindex++ ;
  }

  if ( partial_p2 ) {
    switch( ( int ) getPtePWFlags( pindex ) ) {

    case ( int ) PROTECT :
      if ( !shadow_copy( p1,SAVE ) ) {
	return( PSFALSE ) ;
      }
      break ;

    case ( int ) WRITTEN :
      break ;

    case ( int ) SAVE :
      break ;

    default :
      if ( !map_page( p1,WRITTEN ) ) {
	return( PSFALSE ) ;
      }
      break ;
    }
  }
  CATCH_EXCEPTION;
  return( PSTRUE ) ;
}


psint SS_reserveVM( psint a1,psint a2 )
{
  psint pindex,p1,p2,partial_p2 ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  /* make sure a1 <= a2 */
  if ( a1 > a2 ) {
      psint t ;

      t = a1 ;
      a1 = a2 ;
      a2 = t ;
  }

  if ( a1 < USERPAGESTART || a2 >= VASPACESIZE ) {
      error( "SS_reserveVM passed addresses outwith"
	     " the user's address space" ) ;
  }

  p1 = a1 & PAGENUMBER ; pindex = pteIndex( p1 ) ;
  p2 = a2 & PAGENUMBER ; 

  /* are the first and last pages complete? */

  if ( ( a1 & PAGEINDEX ) != ( psint ) 0 ) {
    if ( getPtePWFlags( pindex ) == DONTNEED ) {
      if ( !map_page( p1,WRITTEN ) ) return( PSFALSE ) ;
    }
    p1 += BPAGESIZE ; pindex++ ;
  }
	
  if ( ( p1 <= p2 ) && ( ( a2 & PAGEINDEX ) != LASTWORD ) )
    {
      partial_p2 = PSTRUE ; p2 -= BPAGESIZE ;
    } else
      {
	partial_p2 = PSFALSE ;
      }

  /* fix up the complete pages in the range a1->a2 */
  while ( p1 <= p2 ) {
      switch( ( int ) getPtePWFlags( pindex ) )	{

      case ( int ) PROTECT :
	break ;

      case ( int ) WRITTEN :
	break ;

      case ( int ) SAVE :
	setPteFlags( pindex,WRITTEN | BLOCKLISTED ) ;
	break ;

      default :
	if ( !map_page( p1,WRITTEN ) ) return( PSFALSE ) ;
	break ;
      }
      p1 += BPAGESIZE ; pindex++ ;
  }

  if ( partial_p2 ) {
    if ( getPtePWFlags( pindex ) == DONTNEED ) {
      if ( !map_page( p1,WRITTEN ) ) return( PSFALSE ) ;
    }
  }
  CATCH_EXCEPTION;
  return( PSTRUE ) ;
}


void SS_dontneedVM( psint a1,psint a2 )
{
  psint pindex,p1,p2 ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  /* make sure a1 <= a2 */
  if ( a1 > a2 ) {
      psint t ;

      t = a1 ;
      a1 = a2 ;
      a2 = t ;
  }

  if ( a1 < USERPAGESTART || a2 >= VASPACESIZE ) {
    error( "SS_dontneedVM passed addresses outwith"
	   " the user's address space" ) ;
  }

  p1 = a1 & PAGENUMBER ;	/* the pages concerned */
  p2 = a2 & PAGENUMBER ; 
  /* are the first and last pages complete? */
  if ( ( a1 & PAGEINDEX ) != ( psint ) 0 ) {
    p1 += BPAGESIZE ;
  }
  if ( ( a2 & PAGEINDEX ) != LASTWORD ) {
    p2 -= BPAGESIZE ;
  }

  pindex = pteIndex( p1 ) ;

  /* fix up the complete pages in the range a1->a2 */
  while ( p1 <= p2 ) {
    if ( getPtePWFlags( pindex ) != DONTNEED ) {
      unmap_page( p1 ) ;
    }
    p1 += BPAGESIZE ; pindex++ ;
  }
  CATCH_EXCEPTION;
}

psint  SS_open ( const char *dirname,
		 PFNUSERERROR error,
		 PFNVOID save,
		 PFNVOID restore,
		 PFNUSERWRITEFAULT write_fault,
		 PFNUSERPAGEFAULT page_fault )
{
  return SS_open_at_addr ( dirname, error, save, restore, write_fault,
			   page_fault, (caddr_t) NULL );
}

psint  SS_open_at_addr ( const char *dirname,
			 PFNUSERERROR error,
			 PFNVOID save,
			 PFNVOID restore,
			 PFNUSERWRITEFAULT write_fault,
			 PFNUSERPAGEFAULT page_fault,
			 caddr_t min_addr )

{
  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  /* save the user specified interfaces */
  user_error = error ;
  user_save = save ;
  user_restore = restore ;
  user_write_fault = write_fault ;
  user_page_fault = page_fault ;

  /* open the stable store */
  init_store( dirname, min_addr ) ;

  /* if error occurs never reach here.... */
  CATCH_EXCEPTION;
  /* return success indicator */
  return( PSTRUE ) ;
}

#if defined(Arch_alpha)||defined(Arch_sun5)||defined(Arch_sun4)

/* MICK */
psint SS_Xmemlock( psint * addr )
{
  psint	lock;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  lock	= mick_SS_Xmemlock( addr );
  CATCH_EXCEPTION;
  return lock;
  /*
    error( "MICK: SS_Xmemlock not yet implemented.\n" ) ;
    return 0;
    */
}
/* MICK END */

#else

psint SS_Xmemlock( psint *addr )
{
  psint	lock;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
#if 0
  error( "SS_Xmemlock not yet implemented.\n" ) ;
  lock	= 0;
#else
  lock	= ( *addr |= 0x80000000 );
#endif
  CATCH_EXCEPTION;
  return lock;
}

#endif

psint SS_set_lock( psint pos )
{
  psint *addr;
  psint	lock;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;
  addr = ( psint * )( ( char * ) disk_vaddr + pos ) ;
  lock	= SS_Xmemlock( addr );
  CATCH_EXCEPTION;
  return lock;
}

static void restore_pages_compact()
{
  psint 	i,j,blockNo,p,q,*page ;
  /*  initialise the address of the pagetable */
  pte = ( psint * )( ( char * ) disk_vaddr + ( ROOTPAGES << PAGEPWROF2 ) ) ;
  blockList = ( psint * )( ( char * ) disk_vaddr + DATAPAGESTART ) ;

  p = ROOTPAGES << PAGEPWROF2 ;
  for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ )
    {
      /* All pte pages will be mapped in as read-only */
      blockNo = root_page.pteWhere[ i ]  ;
      remmap_page( p,blockNo,( psint )( PROT_READ | PROT_EXEC ) ) ;
      p += BPAGESIZE ;
    }

  p = DATAPAGESTART ;			/* address of blocklist */

  for ( i = ROOTPAGES + PT1PAGES; i < ROOTPAGES + PT1PAGES + RESERVEDPAGES;
	i++ ) {
    /* All pte pages will be mapped in as read-only */
    blockNo = getPteBlockNo( i ) ;
    /* Mark block in free list */
    FLSet( blockNo ) ;
    remmap_page( p,blockNo,( psint )( PROT_WRITE | PROT_READ | PROT_EXEC ) ) ;
    page = ( psint * )( ( char * ) disk_vaddr + p ) ;
    for ( j = ( psint ) 0 ; j < WPAGESIZE ; j++ ) {
      page[ j ] = ( psint ) 0 ;
    }
    p += BPAGESIZE ;
  }

  q = USERPAGESTART ;
  /* for loop looks at every entry in pte */
  for ( i = ROOTPAGES + PT1PAGES + RESERVEDPAGES;
	i < ( PT1PAGES << WPAGEPWROF2 ) ; i++ ) {
    switch( ( int ) getPteFlags( i ) ) {

    case ( int ) PROTECT : 
    case ( int ) WRITTEN : 
      blockNo = getPteBlockNo( i ) ;
      remmap_page( p,blockNo,( psint ) ( PROT_READ | PROT_EXEC ) ) ;
      /* Mark block in free list */
      FLSet( blockNo ) ;
      blockList[ blockNo - DATABLOCKSTART ] = p ;
      break ;

    case ( int ) DONTNEED :
      break ;

    default :
      error( "Page table entry found that was not"
	     " DONTNEED or WRITTEN or PROTECTED" ) ;
      break ;
    }
    p += BPAGESIZE ;
  }
}

/* open the file containing the store */
static void init_store_compact( const char *dirname)
{
  /* lock and open the stable store */
  open_disk( dirname ) ;
  /* Find suitable VA and mmap root pages */
  get_suitable_va( (caddr_t) NULL ) ;
  /* find out which root page is uptodate */
  setup_root_pages() ;
  /* find out how big the store actually is? */
  setup_length_info() ;
  /* make a local copy of the root page */
  read_root() ;
  /* setup freespace list from scratch */
  setup_freelist() ;
  /* re-map pages as read_only from last checkpoint */
  restore_pages_compact() ;
  /* increment the restart clock */
  inc_restart_clock() ;
  /* Set signal handlers */
  SS_set_signal_handler ();
  /* startup the user's restore code */
  user_restore() ;
} /* init_store_compact */


static psint getHighestUsed( psint startFrom )
{
  psint	i,found ;

  /* Now find the highest used block */
  found = PSFALSE ;

  i = startFrom - DATABLOCKSTART ;
  while ( !found )
    {
      if ( blockList[ i ] ) found = PSTRUE ; else i-- ;
    }

  return ( i + DATABLOCKSTART ) ;
}
	
static void compact_store()
{
  psint	lo_free,high_used ;
  psint	found,result ;
  psint	flags ;
  psint	i,p,pindex ;

  lo_free = FLSearch() ;	/* Get lowest free block */
  high_used = getHighestUsed( ( store_length >> PAGEPWROF2 ) - ( psint ) 1 ) ;	/* Get last block */

  while( lo_free < high_used ) {

    i = high_used - DATABLOCKSTART ;
    p = blockList[ i ] ;

    result = move_page( p,lo_free ) ;

    /* get the page addresses */
    pindex = pteIndex( p ) ;
    flags = getPtePWFlags( pindex ) ;
    /* Mark it as being used */
    if ( result ) FLSet( lo_free ) ;
    /* Record new mapping in page tables */
    setPteEntry( pindex,lo_free,flags ) ;
    lo_free = FLSearch() ;
    high_used = getHighestUsed( high_used - 1 ) ;
		
  }
}

psint  SS_compact	( const char *dirname,
			  PFNUSERERROR error,
			  PFNVOID save,
			  PFNVOID restore,
			  PFNUSERWRITEFAULT write_fault,
			  PFNUSERPAGEFAULT page_fault )
{
  psint	i,size ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */
  TRY_EXCEPTION;

  /* save the user specified interfaces */
  user_error = (void ( * )( const char * )) error ;
  user_save = save ;
  user_restore = restore ;
  user_write_fault = (PFNUSERWRITEFAULT) write_fault ;
  user_page_fault = (PFNUSERPAGEFAULT) page_fault ;

  /* open the stable store ; dont make new shadow blocks */
  init_store_compact( dirname) ;
  /* Do the compaction */
  compact_store() ;

  /* write out the pagetable pages - assume OS does it efficien
  sync_page( ROOTPAGES,PT1PAGES ) ;
							     tly */
  for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ ) {
    root_page.pteModified[ i ] = PSFALSE ;
  }

  /* Stops a flood after a truncate */
  root_page.no_mod_pages = ( psint ) 0 ;
  write_root() ;
  /* forget the vm copy of the stable store */
  unmap_disk() ;

  /* ( ( 2 * pte ) + 2 ) /32 */
  lookFrom = DATABLOCKSTART >> 5 ;
  /* last used block + BPAGESIZE = first free block !!! */
  size = FLSearch() << PAGEPWROF2 ;
  ftruncate( pstorefd,size ) ;
  /* finish using the disk */
  close_disk() ;
  CATCH_EXCEPTION;
  return( PSTRUE ) ;			/* return success indicator */
}

/* 1998/02/26 HK: Local functions for SS_create_database: */
/* fill in the root page for the given store size */
static void fill_root_page( root_pg	* root_page )
{
  psint i,plen ;
  psint blockNo,flags ;
  char *s ;

  /* initialise the root page to 0s */
  s = ( char * )( root_page ) ;
  for( i = ( psint ) 0 ; i < BPAGESIZE ; i++ )
    s[ i ] = ( char ) 0 ;

  /* magic number for stable store */
  root_page->sstoreMagic = SSTOREMAGIC ;
  /* magic number for stable heap - unset value */
  root_page->sheapMagic = ( psint ) -1 ;
  /* page size in the store file */
  root_page->page_size = BPAGESIZE ;
  /* length of VA space in bytes */
  root_page->data_start = DATAPAGESTART ;
  /* length of VA space in bytes */
  root_page->data_end = VASPACESIZE - WORDSIZE ;
  /* count 1 for the initial set of reserved pages */
  root_page->no_syncs = ( psint ) 1 ;
  /* on init the page table and reserved pages are written */
  root_page->no_mod_pages = PT1PAGES + RESERVEDPAGES ;

  blockNo = ( psint ) 2 ;

  for ( i = ( psint ) 0 ; i < PT1PAGES ; i++ ) {
    root_page->pteWhere[ i ]  = blockNo++ ;
  }
}

static void writepagetablestart( int fd,
				 void ( * pfnError ) ( const char * ) )
{
  psint i,pindex,blockNo ;
  psint page[ WPAGESIZE ] ;

  /* zero the page copy */
  for ( i = ( psint ) 0 ; i < WPAGESIZE ; i++ ) {
    page[ i ] = ( psint ) 0 ;
  }

  /* setup the pte for the reserved pages */
  pindex = DATAPAGESTART >> PAGEPWROF2 ;
  blockNo = DATABLOCKSTART ;
  for ( i = 0 ; i < RESERVEDPAGES ; i++ ) {
    page[ pindex ] = WRITTEN | blockNo ;
    blockNo++ ; pindex++ ;
  }
  if ( write( fd,( char * ) page,( int )( BPAGESIZE ) ) !=
       ( int )( BPAGESIZE ) ) {
    fnError ( __LINE__, errno,
	      "failed to format the store", pfnError );
  }
}

static void writezeropages( int fd, psint len,
			    void ( * pfnError ) ( const char * ) )
{
#define	BUFFER	( PT1PAGES << PAGEPWROF2 )
  psint i ;
  char page[ BUFFER ] ;

  for ( i = ( psint ) 0 ; i < BUFFER ; i++ ) {
    page[ i ] = ( char ) 0 ;
  }
  len <<= PAGEPWROF2 ;
  while ( len > ( psint ) 0 ) {
    i = write( fd,page,( int )( len >= BUFFER ? BUFFER : len ) ) ;
    if ( i > 0 ) {
      len -= i ;
    } else if ( i < 0 && errno != EINTR ) {
      fnError ( __LINE__, errno,
		"failed to format the store", pfnError ) ;
    }
  }
}

/* 1998/02/26 HK: Added SS_create_database: */
psint	SS_create_database	( const char	* pszDatabase,
				  PFNUSERERROR	pfnError )
{
  psint	bDone	= PSFALSE;
  int	lfd,fd,status,i;
  psint	len,slen,maxslen ;
  char	sfname[ 256 ] ;
  char	lfname[ 256 ] ;
  char	hostname[ 64 ] ;
  /* root page structure used in the store file */
  root_pg	root_page ;

  INITIALIZE_SSTORE ();	/* Stable Store initialization. */

  mkdir ( pszDatabase, 0755 );

  strcpy( lfname,pszDatabase ) ;
  strcat( lfname,LOCKSUFFIX ) ;
  /* set all mode bits during a create */
  lfd		= open( lfname,O_RDWR | O_CREAT | O_EXCL,0644 ) ;
  if ( lfd < 0 ) {
      if ( errno == EEXIST ) {
	char	szMessage [ 256 ];
	sprintf ( szMessage, 
		  "the directory %s may already contain an object store",
		  pszDatabase );
	fnError ( __LINE__, errno, szMessage, pfnError );
	return bDone;
      } else {
	char	szMessage [ 256 ];
	sprintf ( szMessage, "failed to create the lockfile %s", lfname );
	fnError ( __LINE__, errno, szMessage, pfnError );
	return bDone;
      }
  }
#if defined(Arch_sun4)	/* 1996/09/11 HK */
  if ( flock( lfd,LOCK_EX | LOCK_NB ) ) {
    fnError ( __LINE__, errno, "cannot lock the store", pfnError ) ;
    return bDone;
  }
#else
  if ( lockf ( lfd, F_TLOCK, 0L ) ) {
    fnError ( __LINE__, errno, "cannot lock the store", pfnError ) ;
    return bDone;
  }
#endif
  memset ( hostname, 0, sizeof ( hostname ) );
  /* get our hostname */
  gethostname( hostname, sizeof ( hostname )) ;
  /* make sure its null terminated */
  for ( i = 0; i < sizeof ( hostname ) - 2 && hostname [ i ] > ' '; i++ );
  hostname[ i++ ] = '\n';
  hostname[ i ] = '\0';
  if ( write( lfd,hostname, i ) != i ) {
    fnError ( __LINE__, errno,
	      "write error occurred while creating the lockfile",
	      pfnError ) ;
    return bDone;
  }

  strcpy( sfname,pszDatabase ) ;
  strcat( sfname,STORESUFFIX ) ;

  /* set all mode bits during a create */
  fd		= open( sfname,O_RDWR | O_CREAT | O_EXCL,0644 ) ;
  if ( fd < 0 ) {
    if ( errno == EEXIST ) {
      char	szMessage [ 256 ];
      sprintf ( szMessage, 
		"the directory %s may already contain an object store",
		pszDatabase );
      fnError ( __LINE__, errno, szMessage, pfnError );
      return bDone;
    } else {
      char	szMessage [ 256 ];
      sprintf ( szMessage, "failed to create the lockfile %s", sfname );
      fnError ( __LINE__, errno, szMessage, pfnError );
      return bDone;
    }
  }
  fill_root_page( & root_page ) ;
  if ( write( fd,( char * )( &root_page ),( int )( BPAGESIZE ) ) !=
       ( int )( BPAGESIZE ) ||
       write( fd,( char * )( &root_page ),( int )( BPAGESIZE ) ) !=
       ( int )( BPAGESIZE ) ) {
    char	szMessage [ 256 ];
    sprintf ( szMessage, "failed to write root pages to %s", sfname );
    fnError ( __LINE__, errno, szMessage, pfnError );
    return bDone;
  }
  /* write the first page of the page table */
  writepagetablestart( fd, pfnError ) ;
  /* write 0 pages for blocks of page table, page table shadows and
     res. pages */
  writezeropages( fd,
		  RESERVEDPAGES + DATABLOCKSTART - ROOTPAGES - ( psint ) 1,
		  pfnError ) ;
#if defined(Arch_sun4)	/* 1996/09/11 HK */
  flock( lfd,LOCK_UN ) ;
#else
  lockf( lfd,F_ULOCK,0) ;
#endif
  close( lfd ) ;
  close( fd ) ;

  bDone	= PSTRUE;
  return bDone;
}

void	SS_initialize()
{
  if ( __bInitializeSStore__ ) {
    __bInitializeSStore__	= 0;
#if defined(Arch_win32)
    {
      WORD	wVersionRequested;
      WSADATA	wsaData;
      wVersionRequested	= MAKEWORD ( 2, 0 );
      WSAStartup ( wVersionRequested, &wsaData );
    }
#else
#endif
  }
}

void	SS_set_signal_handler()
{
  if ( disk_vaddr != (caddr_t) -1 ) {

#if defined(Arch_sun5)||defined(Arch_hpux)	/* 1996/09/16 HK */
    struct sigaction	SigAction;
    stack_t		Stack;

    memset ( &SigAction, 0, sizeof ( SigAction ) );
    memset ( &Stack, 0, sizeof ( Stack ) );
    Stack.ss_sp			= SigStack;
    Stack.ss_size		= sizeof ( SigStack );
    Stack.ss_flags		= 0;
    sigaltstack ( &Stack, NULL );
    SigAction.sa_sigaction	= page_fault;
    SigAction.sa_flags		= SA_SIGINFO;
    sigaction ( SIGSEGV, &SigAction, NULL );
    sigaction ( SIGBUS,  &SigAction, NULL );

#elif defined(Arch_i586)||defined(Arch_win32)	/* 1998/02/17 HK */

    struct sigaction	SigAction;

    memset ( &SigAction, 0, sizeof ( SigAction ) );
    SigAction.sa_handler	= page_fault;
    SigAction.sa_flags		= SA_ONESHOT;
    sigaction ( SIGSEGV, &SigAction, NULL );

#else

    /* enable the page_fault handler */
    (void) signal( SIGSEGV,page_fault );
    /* enable the page_fault handler - to cope with a pre4.1.2 bug */
    (void) signal( SIGBUS,page_fault );

#endif
  }
} /* SS_set_signal_handler */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
