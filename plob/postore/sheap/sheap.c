/*******************************************************************/
/*                                                                 */
/*              PINT POT LAYER 4                                   */
/*                                                                 */
/* LAYER 4 - STABLE HEAP                                           */
/*                                                                 */
/* this module exports the functions init_sheap(),close_sheap(),   */
/* chkpt_sheap(),create_object(),destroy_object(),gc_sheap(),      */
/* first_object(),read_object(),write_object(),can_create(),       */
/* uncreate() and can_modify()                                     */
/*                                                                 */
/* these functions are constructed from those                      */
/* exported by SHADOW PAGED STABLE STORE.                          */
/*                                                                 */
/* created   29/1/87      ALB                                      */
/* modified  28/6/89      ALB checks version numbers               */
/* modified  20/7/89      ALB can_create/uncreate/can_modify stats */
/*                            procs added, heap_limit removed      */
/* modified 15/11/89      ALB use mem advise fns from v3 stable VM */
/* modified  23/7/90      ALB ignores pntrs if <= 0 during GC      */
/* modified   9/8/90      ALB chucked can_create/uncreate          */
/* modified  10/8/90      ALB more efficient free lists, compacts  */
/* modified 12/11/90      ALB removed compaction - not permissible */
/*                            object compaction requires address   */
/*                            ordered list of keys                 */
/* modified  23/1/92      ALB excess objects alloc'd if poss.      */
/*                            via temp. ifdef RAPIER               */
/* modified  25/5/92      ALB sstore interface changed             */
/* modified   5/6/92      ALB use direct addressing where poss     */
/* modified   7/7/92      ALB new compaction - no change bits - no */
/*                            free lists.                          */
/*                                                                 */
/*******************************************************************/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	<sys/types.h>
#include	<sys/mman.h>

#include	"sstore.h"	/* the interface to the stable store library */
#include	"sheap.h"	/* stable store definitions */

#include	<signal.h>
#if defined(Arch_win32)	/* 1998/02/17 HK */
#include	<win32sig.h>
#endif

#define	LSIGN24		( ( psint )( ( ( psint ) 1 << ( psint ) 24 ) - ( psint ) 1 ) )

static void initial_map() ;
static void remap_stabilise() ;
static void gc_remap() ;
static void remap_sheap() ;
					/* stable heap control variables */
static psint SH_base ;			/* address of base of stable heap */
static psint SH_limit ;			/* address + 1 of top of stable heap */


/* the following control variables will be saved in the heap's root object */

/* stable heap's mode, 0 - normal, 1..n - phase of GC */
static psint SH_mode ;

/* start of free space in the stable heap */
static psint SH_data_free ;

/* end of free space + 1 in the stable heap */
static psint SH_data_end ;

/* first free entry in the stable heap's Index space */
static psint SH_IX_free ;

/* last free entry in the stable heap's Index space */
static psint SH_IX_end ;

/* temporary pntr into stable heap for use by garbage collection */
static psint SH_tpntr ;

/* object index marker for use by the garbage collector */
static psint SH_tix ;

/* object copying pointer for use by the garbage collector */
static psint SH_to ;

/* object copying pointer for use by the garbage collector */
static psint SH_from ;

/* object hdr words still to copy during compacting gc */
static psint SH_hdr ;

/* number of pntrs still to copy during compacting gc */
static psint SH_npntrs ;

/* number of non pntrs still to copy during compacting gc */
static psint SH_notpntrs ;

static PFNUSERERROR user_error;
static PFNVOID user_save, user_restore;
static PFNUSERSTABILISE user_stabilise;

static void sstore_error( const char *s)
{
	user_error( s ) ;
	SH_close() ;
	exit( -1 ) ;
}

static void sheap_error( const char *s)
{
	char buffer[ 512 ] ;

	( void ) strcpy( ( char * ) buffer,"sheap: " ) ;
	( void ) strcat( ( char * ) buffer,s ) ;
	user_error( ( char * ) buffer ) ;
	SH_close() ;
	exit( -1 ) ;
}

static void sheap_save(void)
{
}

static void sheap_restore(void)
{
}

static void save_controls(void);

static void sheap_write_fault( psint svmaddr, psint * context )
{
  if ( !SS_saveVM( svmaddr,svmaddr ) ) {
    /* if in GC signal user_stabilise with no context */
    switch ( ( int ) SH_mode ) {

    case 0:
      user_stabilise( context ) ;	/* normal operation */
      break ;

    case 1:	/* mode 1 of GC */
    case 2:	/* mode 2 of GC */
    case 3:	/* mode 3 of GC */
    case 4:	/* mode 4 of GC */
      save_controls() ;
      /* in case the user prog doesnt want to stabilise */
      user_stabilise( ( psint * ) 0 ) ;
      remap_stabilise() ;
      SS_stabilise() ;
      gc_remap() ;
      break ;

    default:
      sheap_error( "stable heap is in an inconsistent state"
		   " - an unknown mode" ) ;
    }
  }
}

static void sheap_page_fault( int sig,int code,
			      struct sigcontext * scp,char * addr )
{
  char	szBuffer [ 256 ];
  sprintf ( szBuffer, "Unexpected page fault encountered (signal %d)",
	    sig ) ;
  sheap_error ( szBuffer );
  abort ();
}

					/* efficient vm addressing support */
static char *Base_Addr ;		/* address to convert sstore addresses into real addresses */
					/* macro to do the conversion efficiently */

#define SS_Real_Address(addr)	( ( psint * )( Base_Addr + (addr) ) )

#define SS_read_word(addr)	( *SS_Real_Address( addr ) )
#define SS_write_word(addr,w)	( *SS_Real_Address( addr ) = w )

/* proc to initialise the base address correctly */
static void init_SS_Real_Address()
{
  psint first,*First ;

  /* the first usable sstore address */
  first = SS_first_address() ;
  /* convert it into a real address */
  First = SS_real_address( first ) ;
  if ( First == ( psint * ) 0 )
    {
      sheap_error( "the stable store interface does not implement SS_real_address" ) ;
    }
  /* calculate the real base address */
  Base_Addr = ( ( char * ) First ) - first ;
}


static void extract_controls()			/* extract the control variables from the root object */
{
	psint root_object ;			/* the stable store address of the root object */

	root_object = SH_first_object() ;	/* the root object's key */
	root_object = SS_read_word( root_object ) ;

	/* check there is a root object */
	if ( root_object == ( psint ) 0 )
		sheap_error( "Could not find the stable store root object" ) ;

	/* read the control variables 1 at a time */
	SH_mode = SS_read_word( root_object + SHMODE ) ;
	SH_data_free = SS_read_word( root_object + SHDATAFREE ) ;
	SH_data_end = SS_read_word( root_object + SHDATAEND ) ;
	SH_IX_free = SS_read_word( root_object + SHIXFREE ) ;
	SH_IX_end = SS_read_word( root_object + SHIXEND ) ;
	SH_tpntr = SS_read_word( root_object + SHTPNTR ) ;
	SH_tix = SS_read_word( root_object + SHTIX ) ;
	SH_to = SS_read_word( root_object + SHTO ) ;
	SH_from = SS_read_word( root_object + SHFROM ) ;
	SH_hdr = SS_read_word( root_object + SHHDR ) ;
	SH_npntrs = SS_read_word( root_object + SHNPNTRS ) ;
	SH_notpntrs = SS_read_word( root_object + SHNOTPNTRS ) ;
}

/* initialise the control variables and the root object */
static void initialise_controls()
{
  /* the stable store address of the root object */
  psint root_object ;

  /* the root object's key */
  root_object = SH_first_object() ;
  /* the root object's stable store address */
  if ( !SS_saveVM( root_object,root_object ) )
    sheap_error( "Cannot initialise the stable heap" ) ;

  /* there is no root object so create it */
  /* initialise the control variables */
  SH_mode = ( psint ) 0 ;
  SH_data_free = SH_base ;
  SH_data_end = SH_base ;
  SH_IX_free = SH_limit ;
  SH_IX_end = SH_limit ;
  SH_tpntr = ( psint ) 0 ;
  SH_tix = ( psint ) 0 ;
  SH_to = ( psint ) 0 ;
  SH_from = ( psint ) 0 ;
  SH_hdr = ( psint ) 0 ;
  SH_npntrs = ( psint ) 0 ;
  SH_notpntrs = ( psint ) 0 ;

  /* setup use of VM in order to create the root object */
  initial_map() ;
  /* NOW CREATE THE ROOT OBJECT */
  root_object = SH_create_object( SHROOTSIZE ) ;
  /* the root object's address */
  root_object = SS_read_word( root_object ) ;

  /* set the number of pointers to 1 */
  SS_write_word( root_object,( psint ) 1 ) ;
  /* set the data pointer field to point at self */
  SS_write_word( root_object + SHDATA,SH_first_object() ) ;

  /* write the control variables 1 at a time */
  SS_write_word( root_object + SHMODE,SH_mode ) ;
  SS_write_word( root_object + SHDATAFREE,SH_data_free ) ;
  SS_write_word( root_object + SHDATAEND,SH_data_end ) ;
  SS_write_word( root_object + SHIXFREE,SH_IX_free ) ;
  SS_write_word( root_object + SHIXEND,SH_IX_end ) ;
  SS_write_word( root_object + SHTPNTR,SH_tpntr ) ;
  SS_write_word( root_object + SHTIX,SH_tix ) ;
  SS_write_word( root_object + SHTO,SH_to ) ;
  SS_write_word( root_object + SHFROM,SH_from ) ;
  SS_write_word( root_object + SHHDR,SH_hdr ) ;
  SS_write_word( root_object + SHNPNTRS,SH_npntrs ) ;
  SS_write_word( root_object + SHNOTPNTRS,SH_notpntrs ) ;

  /* ensure the new root object is made part of the store */
  SS_stabilise() ;
}

/* save the control variables in the root object */
static void save_controls(void)
{
  /* the address of the root object */
  psint root_object ;

  /* the root object's key */
  root_object = SH_first_object() ;
  /* the root object's address */
  root_object = SS_read_word( root_object ) ;

  /* write the control variables 1 at a time */
  SS_write_word( root_object + SHMODE,SH_mode ) ;
  SS_write_word( root_object + SHDATAFREE,SH_data_free ) ;
  SS_write_word( root_object + SHDATAEND,SH_data_end ) ;
  SS_write_word( root_object + SHIXFREE,SH_IX_free ) ;
  SS_write_word( root_object + SHIXEND,SH_IX_end ) ;
  SS_write_word( root_object + SHTPNTR,SH_tpntr ) ;
  SS_write_word( root_object + SHTIX,SH_tix ) ;
  SS_write_word( root_object + SHTO,SH_to ) ;
  SS_write_word( root_object + SHFROM,SH_from ) ;
  SS_write_word( root_object + SHHDR,SH_hdr ) ;
  SS_write_word( root_object + SHNPNTRS,SH_npntrs ) ;
  SS_write_word( root_object + SHNOTPNTRS,SH_notpntrs ) ;
}

static void gc_1_sheap() ;
static void gc_2_sheap() ;
static void gc_3_sheap() ;
static void gc_4_sheap() ;

/* initialise the stable heap */
static void init_sheap( const char *dirname,
			caddr_t min_addr )
{
  psint vno,ok ;

  /* init the shadow paged stable store */
  ok = SS_open_at_addr ( dirname,sstore_error,
			 sheap_save,sheap_restore,sheap_write_fault,
			 (PFNUSERPAGEFAULT) sheap_page_fault,
			 min_addr );
  if ( ok == ( psint ) 0 )
    sheap_error( "failed to access stable store interface" ) ;

  init_SS_Real_Address() ;	/* initialise the direct VM addressing */

  /* initialise the stable heap addresses */
  SH_base = SS_first_address()  ;
  /* to convert last usable address to +1 word */
  SH_limit = SS_last_address() + WORDSIZE  ;
  /* check the stable heap addresses are sensible */
  if ( SH_base <= ( psint ) 0 || SH_limit <= ( psint ) 0 )
    {
      sheap_error( "first and last addresses of stable storage must be greater than 0" ) ;
    }
  if ( SH_limit % KEYALIGNMENT )
    {
      sheap_error( "stable storage address space is not correctly aligned" ) ;
    }

  vno = SS_get_sheap_version() ;
  if ( vno == ( psint ) -1 )	/* has the stable heap vnumber been set? */
    {				/* NO... so set it */
      SS_set_sheap_version( SHEAPMAGIC ) ;
      initialise_controls() ;	/* then create and initialise the root object */
    } else
      {				/* YES... so check it */
	if ( vno != SHEAPMAGIC )
	  sheap_error( "stable heap was not created by this implementation of the stable store" ) ;
	extract_controls() ;	/* extract the control variables from the root object */
      }

  switch ( ( int ) SH_mode )
    {
    case 0:		break ;			/* mode 0 is OK */
    case 1:		gc_1_sheap() ;		/* mode 1 of GC */
    case 2:		gc_2_sheap() ;		/* mode 2 of GC */
    case 3:		gc_3_sheap() ;		/* mode 3 of GC */
    case 4:		gc_4_sheap() ;		/* mode 4 of GC */
      SH_mode = ( psint ) 0 ;	/* we've finished the GC */
      save_controls() ;
      SS_stabilise() ;	/* save the completed GC */
      break ;
    default:	sheap_error( "stable heap is in an inconsistent state - an unknown mode" ) ;
    }

  /* setup required use of stable heap's VM */
  remap_sheap() ;
  /* finally setup the user's cached data */
  user_restore() ;
}

void SH_close()		/* finished with stable heap */
{
  SS_close() ;		/* finished with shadow paged stable store */
}

void SH_stabilise()	/* checkpoint the stable heap */
{
  TRY_EXCEPTION;
  /* save the user's cached data in the stable heap */
  user_save() ;
  /* save the control variables in the root object */
  save_controls() ;
  /* junk the unused areas of the stable heap - no point saving them */
  remap_stabilise() ;
  /* checkpoint the shadow paged stable store */
  SS_stabilise() ;
  /* setup required use of stable heap's VM */
  remap_sheap() ;
  /* restore the user's cached data */
  user_restore() ;
  CATCH_EXCEPTION;
}

psint *SH_key_to_address( psint key )
{
  psint addr, *p ;

  TRY_EXCEPTION;
  addr	= SS_read_word ( key ) ;
  p	= SS_Real_Address ( addr );
  CATCH_EXCEPTION;
  return p;
}

psint SH_create_object( psint size )	/* create object of size words */
{
  psint new_object ;		/* the address of the new object */
  psint newkey ;

  /* dont bother for silly sizes */
  if ( size < MINOBJECTSIZE ) return( ( psint ) 0 ) ;

  TRY_EXCEPTION;
  /* place at the start of the free space + lock word */
  new_object = SH_data_free ;
  /* allocate the space */
  SH_data_free += ( size << WORDPWROF2 ) + PREFIXSIZE ;
  /* allocate IX space */
  SH_IX_free -= IXSIZE ;
  /* we now have the new key */
  newkey = SH_IX_free ;

  if ( ( SH_data_free > SH_data_end ) || ( SH_IX_free < SH_IX_end ) )
    {
      psint ok,new_data_end,new_IX_end ;

      new_data_end = SH_data_free + GENDATASIZE / 2 ;
      new_IX_end = SH_IX_free - GENIXSIZE / 2 ;
      if ( new_data_end <= new_IX_end )
	{
	  ok = PSTRUE ;
	  if ( new_data_end > SH_data_end )
	    {
	      ok = SS_saveVM( SH_data_end,new_data_end - WORDSIZE ) ;
	    }
	  if ( ok && ( new_IX_end < SH_IX_end ) )
	    {
	      ok = SS_saveVM( new_IX_end,SH_IX_end - WORDSIZE ) ;
	    }
	} else
	  {
	    ok = PSFALSE ;
	  }
      if ( ok )
	{
	  SH_data_end = new_data_end ;
	  SH_IX_end = new_IX_end ;
	} else
	  {
	    SH_data_free = new_object ;		/* free the space */
	    SH_IX_free += IXSIZE ;		/* free IX space */
	    return( ( psint ) 0 ) ;		/* return 0 key */
	  }
    }

  /* write the object address to the IX space */
  SS_write_word( newkey,new_object + PREFIXSIZE ) ;
  /* write the header of the new object */
  /* lock word is 0 */
  SS_write_word( new_object,( psint ) 0 ) ;
  new_object += WORDSIZE ;
  /* no pointers */
  SS_write_word( new_object,( psint ) 0 ) ;
  new_object += WORDSIZE ;
  /* size in words */
  SS_write_word( new_object,size ) ;
  CATCH_EXCEPTION;
  return( newkey ) ;	/* return the object's key */
}

void SH_destroy_object( psint key )	/* destroy the object with key 'key' */
{
  /* A NO-OP in this case */
}

static void gc_1_sheap()	/* phase 1 of the garbage collection */
{
  if ( SH_mode != ( psint ) 1 )
    {
      /* set the pointer to the current copied object */
      SH_tpntr = SH_limit ;
      /* mode 1 - initialise the mark stack */
      SH_mode = ( psint ) 1 ;	
    }

  while( SH_tpntr > SH_IX_free )
    {
      SS_write_word( SH_tpntr - IXSIZE + WORDSIZE,( psint ) 0 ) ;
      SH_tpntr -= IXSIZE ;
    }
}

/* stats from last GC */
/* total pntrs scanned */
static psint totalKeys ;

/* total words excluding pfields and headers */
static psint totalWords ;

/* total objects marked */
static psint totalObjects ;

/* total objects marked with keys */
static psint totalObjwKeys ;

/* total words if no keys */
static psint totalWordsOnly ;

/* phase 2 of the garbage collection */
static void gc_2_sheap()
{
  if ( SH_mode != ( psint ) 2 )
    {
      /* start with root object as 1st mark stack entry */
      psint Root ;

      Root = SH_limit - IXSIZE ;	/* Root object's key */
      SS_write_word( Root + WORDSIZE,SS_read_word( Root ) | MARKBIT ) ;
      SH_tix = Root ;			/* Root is at end of mstack */
      /* set the pointer to the current marked object */
      SH_tpntr = SH_limit ;
      SH_mode = ( psint ) 2 ;		/* mode 2 - marking phase */
    }

  /* scan markstack until all reachables have been marked */
  totalKeys = totalWords = totalObjects = ( psint ) 0 ;
  while( SH_tpntr > SH_tix )
    {
      psint nobj,nkeys,nkey ;

      nobj = SS_read_word( SH_tpntr - IXSIZE + WORDSIZE ) & ADDRESSBITS ;

      /* the number of pointers in the object */
      nkeys = SS_read_word( nobj ) ;
      nkey = nobj + HEADERSIZE ;
      totalObjects++ ;
      totalKeys += nkeys ;
      totalWords += SS_read_word( nobj + WORDSIZE ) - nkeys - HEADERWORDS ;
      if ( nkeys > ( psint ) 0 )
	totalObjwKeys++ ;
      else
	totalWordsOnly += SS_read_word( nobj + WORDSIZE ) - nkeys - HEADERWORDS ;
      while( nkeys-- > ( psint ) 0 )
	{
	  psint key,mstack_key ;

	  key = SS_read_word( nkey ) ; nkey += WORDSIZE ;
	  /* a key is a legal pointer if its in the correct range
	     and is correctly aligned */
	  if ( ( key >= SH_IX_free ) && ( key < SH_limit ) &&
	       !( key % KEYALIGNMENT ) )
	    {
	      mstack_key = SS_read_word( key + WORDSIZE ) ;
	      if ( !( mstack_key & MARKBIT ) )	/* mark the object */
		{ /* must 1st add it to the mstack */
		  if ( key == ( SH_tix - IXSIZE ) )
		    {	/* mstack & mark bits overlap */
		      mstack_key = SS_read_word( key ) | MARKBIT ;
		      SS_write_word( key + WORDSIZE,mstack_key ) ;
		    } else
		      {	/* mstack and mark bits do not overlap */
			psint mstack_tix ;
			/* write out next mstack word */
			mstack_tix = SS_read_word( SH_tix - IXSIZE + WORDSIZE ) ;
			mstack_tix &= MARKBIT ;
			mstack_tix |= SS_read_word( key ) ;
			SS_write_word( SH_tix - IXSIZE + WORDSIZE,mstack_tix ) ;
			/* set the mark bit for the object */
			mstack_key |= MARKBIT ;
			SS_write_word( key + WORDSIZE,mstack_key ) ;
		      }

		  /* move the mstack end marker down one */
		  SH_tix -= IXSIZE ;
		}
	    }
	}
      SH_tpntr -= IXSIZE ;
    }
  /* 1998/02/25 HK: Debug: */
  /*
  printf( "total objects %d, with keys %d,nkeys %d,"
	  " nonkeys %d,nonkeysonly %d\n",
	  totalObjects,totalObjwKeys,totalKeys,totalWords,totalWordsOnly ) ;
  */
}

static void gc_3_sheap()	/* phase 3 of the garbage collection */
{
  if ( SH_mode != ( psint ) 3 )
    {
      /* set the pointer to the IX words to be checked */
      SH_tpntr = SH_limit - IXSIZE + WORDSIZE ;
      /* the last key that was allocated */
      SH_tix = SH_limit ;
      /* mode 3 - reallocate keys and calculate new object addresses */
      SH_mode = ( psint ) 3 ;
    }
  while ( SH_tpntr > SH_IX_free )
    {
      psint mstack ;

      /* read the next word of the mstack */
      mstack = SS_read_word( SH_tpntr ) ;
      /* if its for a marked object allocate the next free key */
      if ( mstack & MARKBIT )
	{
	  /* next key to be allocated */
	  psint nextkey ;

	  /* tix is the last key allocated - so dec by 1 */
	  nextkey = SH_tix - IXSIZE ;
	  SS_write_word( SH_tpntr,nextkey ) ;
	  SH_tix = nextkey ;	/* remember the key */
	} else
	  { /* its an unmarked object so the new key is 0 */
	    SS_write_word( SH_tpntr,( psint ) 0 ) ;
	  }
      SH_tpntr -= IXSIZE ;
    }
}

static void copy_object()
{
  /* if the object is staying put - just check its pointer fields */
  if ( SH_from == SH_to )
    {
      SH_from += SH_hdr ; SH_to += SH_hdr ; SH_hdr = ( psint ) 0 ;

      while ( SH_npntrs > ( psint ) 0 )
	{
	  psint oldkey,newkey ;

	  oldkey = SS_read_word( SH_from ) ;
	  /* only update valid keys -
	     in the right range and correctly aligned */
	  if ( ( oldkey >= SH_IX_free ) && ( oldkey < SH_limit ) &&
	       !( oldkey % KEYALIGNMENT ) )
	    {
	      newkey = SS_read_word( oldkey + WORDSIZE ) ;
	      /* if the key has changed write it out */
	      if ( newkey != oldkey )
		{
		  SS_write_word( SH_to,newkey ) ;
		}
	    }
	  SH_to += WORDSIZE ; SH_from += WORDSIZE ;
	  SH_npntrs-- ;
	}
      SH_to += SH_notpntrs ; SH_notpntrs = ( psint ) 0 ;
    } else
      {	/* the object must move, copy all of it */
	while ( SH_hdr > ( psint ) 0 )
	  {
	    SS_write_word( SH_to,SS_read_word( SH_from ) ) ;
	    SH_to += WORDSIZE ; SH_from += WORDSIZE ;
	    SH_hdr -= WORDSIZE ;
	  }

	while ( SH_npntrs > ( psint ) 0 )
	  {
	    psint oldkey,newkey ;

	    oldkey = SS_read_word( SH_from ) ;
	    /* only update valid keys -
	       in the right range and correctly aligned */
	    if ( ( oldkey >= SH_IX_free ) && ( oldkey < SH_limit ) &&
		 !( oldkey % KEYALIGNMENT ) )
	      {
		newkey = SS_read_word( oldkey + WORDSIZE ) ;
		SS_write_word( SH_to,newkey ) ;
	      } else
		{
		  SS_write_word( SH_to,oldkey ) ;
		}
	    SH_to += WORDSIZE ; SH_from += WORDSIZE ;
	    SH_npntrs-- ;
	  }

	while ( SH_notpntrs > ( psint ) 0 )
	  {
	    SS_write_word( SH_to,SS_read_word( SH_from ) ) ;
	    SH_to += WORDSIZE ; SH_from += WORDSIZE ;
	    SH_notpntrs -= WORDSIZE ;
	  }
      }
}

static void gc_4_sheap()	/* phase 4 of the garbage collection */
{
  psint tmp ;

  if ( SH_mode != ( psint ) 4 )
    {
      /* set the pointer to the IX words to be checked */
      SH_tpntr = SH_limit ;
      /* number of header words still to be copied */
      SH_hdr = ( psint ) 0 ;
      /* number of pntrs still to be copied */
      SH_npntrs = ( psint ) 0 ;
      /* the number of non pointers still to be copied */
      SH_notpntrs = ( psint ) 0 ;
      /* source for compaction copying */
      SH_from = SH_base ;
      /* destination for compaction copying */
      SH_to = SH_base ;
      /* mode 4 - compaction phase */
      SH_mode = ( psint ) 4 ;
    } else
      {
	/* in case we were in the middle of one */
	copy_object() ;
      }
  /* scan the IX space as it was */
  while ( SH_tpntr > SH_IX_free )
    {
      psint newkey ;

      newkey = SS_read_word( SH_tpntr - IXSIZE + WORDSIZE ) ;
      if ( newkey == ( psint ) 0 )
	{
	  SH_tpntr -= IXSIZE ;
	} else
	  {
	    /* old address of next object to copy */
	    SH_from = SS_read_word( SH_tpntr - IXSIZE ) ;
	    /* check we are copying in the right direction */
	    if ( SH_from <= SH_to )
	      {
		char	szMessage [ 256 ];
		sprintf ( szMessage,
			  "compaction failure -"
			  " object is to be copied in wrong direction.\n"
			  "       From 0x%X (%d) to 0x%X (%d).",
			  SH_from, SH_from, SH_to, SH_to );
		sheap_error( szMessage );
	      }
	    /* record new address of the object */
	    SS_write_word( newkey,SH_to + PREFIXSIZE ) ;
	    /* move tpntr on so we dont repeat this step */
	    SH_tpntr -= IXSIZE ;
	    /* size of header + prefix in bytes */
	    SH_hdr = PREFIXSIZE + HEADERSIZE ;
	    /* no. of pntrs to be copied */
	    SH_npntrs = SS_read_word( SH_from ) ;
	    /* size of object to be copied */
	    SH_notpntrs = SS_read_word( SH_from + WORDSIZE ) ;
	    /* no. of non pointers in the object */
	    SH_notpntrs -= HEADERWORDS + SH_npntrs ;

	    /* check we have a sensible object */
	    if ( SH_notpntrs < ( psint ) 0 )
	      {
		sheap_error( "compaction failure - object size is less than header + pointer fields" ) ;
	      }

	    /* convert to bytes */
	    SH_notpntrs <<= WORDPWROF2 ;
	    /* set SH_from to start of the prefix */
	    SH_from -= PREFIXSIZE ;

	    /* finally copy the object */
	    copy_object() ;
	  }
    }
  /* tix contains the last key allocated in step 3 - the new IX_free */
  SH_IX_free = SH_tix ;
  /* SH_to is the start of the free space */
  SH_data_free = SH_to ;
  SH_mode = ( psint ) 0 ;	/* we've finished */
}

/* detect and destroy inaccessible objects within the stable heap */
void SH_garbage_collect()
{ /* perform all the garbage collection phases */
  TRY_EXCEPTION;
  user_save() ;
  remap_stabilise() ;

  gc_1_sheap() ;	/* execute phases in sequence */
  gc_2_sheap() ;
  gc_3_sheap() ;
  gc_4_sheap() ;
  SH_mode = ( psint ) 0 ;	/* we've finished */

  remap_sheap() ;		/* setup required use of stable heap's VM */
  user_restore() ;		/* restore the user's cached data */
  CATCH_EXCEPTION;
}

/* as per normal garbage collection but compact in phase 3 */
void SH_full_garbage_collect()
{ /* compaction also reallocates keys... */
  TRY_EXCEPTION;
  user_save() ;
  remap_stabilise() ;

  gc_1_sheap() ;	/* execute phases in sequence */
  gc_2_sheap() ;
  gc_3_sheap() ;
  gc_4_sheap() ;
  SH_mode = ( psint ) 0 ;	/* we've finished */

  remap_sheap() ;		/* setup required use of stable heap's VM */
  user_restore() ;		/* restore the user's cached data */
  CATCH_EXCEPTION;
}

void SH_incremental_garbage_collect()
{
}

/* return the key of the root object */
psint SH_first_object()
{ /* the first entry in the heap's Index */
  return( SH_limit - IXSIZE ) ;
}

/* copy n words from the object 'key' to the buffer */
void SH_read_words( psint key,psint index,psint * buffer,psint nwords )
     /* copy starts at offset 'index' within the object */
{
  psint source,*src ;		/* actual address of the source for the read */

  /* read the actual address of the object */
  TRY_EXCEPTION;
  source = SS_read_word( key ) ;
  src = SS_Real_Address( source ) ;	/* the real address of the object to be read */
  src += index ;			/* add index to address */
  while( nwords-- > ( psint ) 0 )	/* read in the nwords */
    {
      *buffer++ = *src++ ;
    }
  CATCH_EXCEPTION;
}

/* copy n words from the buffer to object 'key' */
void SH_write_words( psint key,psint index,psint * buffer,psint nwords )
     /* copy starts at offset 'index' within the object */
{
  psint dest,*dst ;		/* actual address of the destination for the write */

  /* read the actual address of the object */
  TRY_EXCEPTION;
  dest = SS_read_word( key ) ;
  dst = SS_Real_Address( dest ) ;	/* the real address of the object to be written */
  dst += index ;			/* add index to address */
  while( nwords-- > ( psint ) 0 )	/* write out the nwords */
    {
      *dst++ = *buffer++ ;
    }
  CATCH_EXCEPTION;
}

/* read the byte at offset 'index' in the object 'key' */
psint SH_read_byte( psint key,psint index )
{
  psint source ;
  unsigned char *src ;
  psint		byte;

  TRY_EXCEPTION;
  /* read the actual address of the object */
  source = SS_read_word( key ) ;
  /* the real address of the object as a byte pntr */
  src = ( unsigned char * )( SS_Real_Address( source ) ) ;
  byte	= ( psint )( src[ index ] ) ;
  CATCH_EXCEPTION;
  return byte;
}

/* write the byte at offset 'index' in the object 'key' */
void SH_write_byte( psint key,psint index,unsigned char thebyte )
{
  psint dest ;
  unsigned char *dst ;

  TRY_EXCEPTION;
  /* read the actual address of the object */
  dest = SS_read_word( key ) ;
  /* the real address of the object as a byte pntr */
  dst = ( unsigned char * )( SS_Real_Address( dest ) ) ;
  dst[ index ] = thebyte ;
  CATCH_EXCEPTION;
}

/* read the word at offset 'index' in the object 'key' */
psint SH_read_word( psint key,psint index )
{
  psint source,*src,word ;

  TRY_EXCEPTION;
  /* read the actual address of the object */
  source = SS_read_word( key ) ;
  /* the real address of the object as a word pntr */
  src = SS_Real_Address( source ) ;
  word	=  src[ index ];
  CATCH_EXCEPTION;

  return word;
}

/* write the word at offset 'index' in the object 'key' */
void SH_write_word( psint key,psint index,psint theword )
{
  psint dest,*dst ;

  TRY_EXCEPTION;
  /* read the actual address of the object */
  dest = SS_read_word( key ) ;
  /* the real address of the object as a word pntr */
  dst = SS_Real_Address( dest ) ;
  dst[ index ] = theword ;
  CATCH_EXCEPTION;
}

/* read the word at offset 'index' in the object 'key' */
psint SH_read_key( psint key,psint index )
{
  psint source,*src, key_read ;

  TRY_EXCEPTION;
  /* read the actual address of the object */
  source	= SS_read_word ( key );
  /* the real address of the object as a word pntr */
  src		= SS_Real_Address ( source );
  key_read	= src [ index ];
  CATCH_EXCEPTION;
  return key_read;
}

/* write the word at offset 'index' in the object 'key' */
void SH_write_key( psint key,psint index,psint theptr )
{
  psint dest,*dst ;

  TRY_EXCEPTION;
  /* read the actual address of the object */
  dest = SS_read_word( key ) ;
  /* the real address of the object as a word pntr */
  dst = SS_Real_Address( dest ) ;
  dst[ index ] = theptr ;
  CATCH_EXCEPTION;
}

static void initial_map()		/* setup enoug store to create the stable heap's root object */
{
  SH_data_end = SH_data_free + ( SHROOTSIZE * WORDSIZE ) + PREFIXSIZE ;
  SH_IX_end = SH_IX_free - IXSIZE ;

  if ( SH_IX_end < SH_data_end )
    {
      sheap_error( "stable store is too small to be usable" ) ;
    }

  if ( !SS_saveVM( SH_data_free,SH_data_end - WORDSIZE ) )
    {
      sheap_error( "stable store cannot support desired store use" ) ;
    }

  if ( !SS_saveVM( SH_IX_end,SH_IX_free - WORDSIZE ) )
    {
      sheap_error( "stable store cannot support desired store use" ) ;
    }
}

static void gc_remap()
{
  if ( !SH_can_modify( SH_first_object() ) )
    {
      sheap_error( "stable store cannot modify its own root object" ) ;
    }
}

static void remap_stabilise()
{
  if ( SH_data_end > SH_data_free )
    {
      SS_dontneedVM( SH_data_free,SH_data_end - WORDSIZE ) ;
      SH_data_end = SH_data_free ;
    }
  if ( SH_IX_end < SH_IX_free )
    {
      SS_dontneedVM( SH_IX_end,SH_IX_free - WORDSIZE ) ;
      SH_IX_end = SH_IX_free ;
    }
}

/* we just want a generation of extra space for new objects */
static void remap_sheap()
{
  psint new_data_end,new_IX_end ;

  /* if too much space in use - free the excess */
  new_data_end = SH_data_free + GENDATASIZE ;
  if ( new_data_end < SH_data_end )
    {
      SS_dontneedVM( new_data_end,SH_data_end - WORDSIZE ) ;
    }
  SH_data_end = new_data_end ;

  /* if too much space in use - free the excess */
  new_IX_end = SH_IX_free - GENIXSIZE ;
  if ( new_IX_end > SH_IX_end )
    {
      SS_dontneedVM( SH_IX_end,new_IX_end - WORDSIZE ) ;
    }
  SH_IX_end = new_IX_end ;

  if ( SH_IX_end < SH_data_end )
    {
      psint ix_diff ;

      ix_diff = ( SH_IX_free - SH_data_free ) / ( IXSIZE * ( psint ) 8 ) ;
      SH_IX_end = SH_IX_free - ix_diff * IXSIZE ;
      SH_data_end = SH_IX_end ;
    }

  if ( !SS_saveVM( SH_data_free,SH_data_end - WORDSIZE ) )
    {
      sheap_error( "stable store cannot support desired store use" ) ;
    }

  if ( !SS_saveVM( SH_IX_end,SH_IX_free - WORDSIZE ) )
    {
      sheap_error( "stable store cannot support desired store use" ) ;
    }

  if ( !SH_can_modify( SH_first_object() ) )
    {
      sheap_error( "stable store cannot modify its own root object" ) ;
    }
}

/* can the object with address key be modified
   without exhuasting the shadow store */
psint SH_can_modify( psint key )
{
  /* actual address of the source to be updated */
  psint source = 0,len = 0;
  psint	modifyp = 0;

  TRY_EXCEPTION;
  /* read the actual address of the object */
  source = SS_read_word( key ) ;
  /* read the object size */
  len = SS_read_word( source + WORDSIZE ) ;
  len <<= WORDPWROF2 ;
  /* find out if the pages may be changed */
  modifyp	= SS_saveVM ( source - PREFIXSIZE,source + len - WORDSIZE );
  CATCH_EXCEPTION;
  return modifyp;
}

void SH_configuration( struct stableheap_configuration * config )
{
  config->configuration_flags =
    KEY_TO_ADDR | KEYS_ARE_INDIRECT | KEY_RANGE | REQUEST_STABILISE ;
  config->minimum_key = SH_base ;
  config->maximum_key = SH_limit - IXSIZE ;
  config->key_alignment = KEYALIGNMENT ;
}

void SH_statistics( struct stableheap_statistics * stats )
{
  stats->maximum_space = SH_limit - SH_base ;
  stats->allocated_space = SH_data_free - SH_base ;
  stats->unallocated_space =
    ( SH_data_end - SH_data_free ) + ( SH_IX_free - SH_IX_end ) ;
  stats->unused_allocated_space = ( psint ) 0 ;
  stats->allocated_management_space = SH_limit - SH_IX_free ;
  stats->number_of_objects = ( SH_limit - SH_IX_free ) / IXSIZE ;
}

psint SH_get_restart_clock()
{
  return( SS_get_restart_clock() ) ;
}

void SH_set_restart_clock( psint tim )
{
  SS_set_restart_clock( tim ) ;
}

psint SH_open	( const char *dirname,
		  PFNUSERERROR error,
		  PFNVOID save,
		  PFNVOID restore,
		  PFNUSERSTABILISE stabilise )
{
  return SH_open_at_addr ( dirname, error, save, restore, stabilise,
			   (caddr_t) NULL );
}

psint SH_open_at_addr	( const char *dirname,
			  PFNUSERERROR error,
			  PFNVOID save,
			  PFNVOID restore,
			  PFNUSERSTABILISE stabilise,
			  caddr_t min_addr )
{
  TRY_EXCEPTION;
  /* save the user specified interfaces */
  user_error = error ;
  user_save = save ;
  user_restore = restore ;
  user_stabilise = stabilise ;

  /* open the stable store */
  init_sheap( dirname, min_addr ) ;

  /* if error occurs never reach here.... */
  CATCH_EXCEPTION;

  /* return success flag */
  return( PSTRUE ) ;
}

psint SH_read_lock( psint key )
{
  psint addr, lock ;
  
  TRY_EXCEPTION;
  addr	= SS_read_word( key ) ;
  lock	= SS_read_word( addr - WORDSIZE );
  CATCH_EXCEPTION;
  return lock;
}

psint SH_set_lock( psint key )
{
  psint addr, lock ;

  TRY_EXCEPTION;
  addr	= SS_read_word( key ) ;
  lock	= SS_set_lock( addr - WORDSIZE );
  CATCH_EXCEPTION;
  return lock;
}

void SH_write_lock( psint key,psint loc )
{
  psint addr ;

  TRY_EXCEPTION;
  addr = SS_read_word( key ) ;
  SS_write_word( addr - WORDSIZE,loc ) ;
  CATCH_EXCEPTION;
}

/* 1998/02/26 HK: Added SH_create_database: */
extern psint	SH_create_database	( const char	* pszDatabase,
					  void		( * pfnError )
					  ( const char * ) )
{
  return SS_create_database ( pszDatabase, pfnError );
}

/* 1998/07/07 HK: Added SH_set_signal_handler() for re-establishing
   the current signal handler. */
extern void	SH_set_signal_handler (void)
{
  SS_set_signal_handler ();
} /* SH_set_signal_handler */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
