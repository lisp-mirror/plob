/**************************************************************/
/*                                                            */
/*          Defines for the memory mapped stable store        */
/*                       28 June 1991                         */
/*                                                            */
/**************************************************************/
/*                                                            */
/* This include file contains the defines for the sstore lib  */
/*                                                            */
/* created   28/ 6/91   DM                                    */
/* modified  17/ 5/92   DM                                    */
/*                                                            */
/**************************************************************/

#if defined(Arch_win32)
/* 1998/02/17 HK: Added Windows NT */
#include	<windows.h>
#endif

/* the type of a 32bit integer: long or int, as required. */
#ifdef	MAC
typedef long psint ;
typedef	short double psreal ;
#else
typedef int psint ;
typedef double psreal ;
#endif	/* MAC */

#define	WORDSIZE	( ( psint ) 4 )			/* the size of a WORD -> psint, in bytes */

/* Boolean literals */

#define PSTRUE	( ( psint ) 1 )
#define PSFALSE	( ( psint ) 0 )

/* Bit Masks */

#define BIT_31		( ( psint ) 020000000000 )	/* bit 31 */
#define	LOWER24		( ( psint ) 0x00FFFFFF )	/* low 24 bits */
#define	UPPER8		( ( psint ) 0xFF000000 )	/* top 8 bits */

/* The UNIX pathnames used to identify the store's files */

#define	STORESUFFIX		"/stablestore"
#define	LOCKSUFFIX		"/lockfile"
#define	TEMPSUFFIX		"/tempstablestore"

/* BPAGESIZE parameters for this particular stable store */

#if defined(Arch_mips)

/* page size is 1 << 14 -> c. 16384 */
#define	PAGEPWROF2		( ( psint ) 14 )

#elif defined(Arch_win32)

/* page size is 1 << 16 -> c. 65536 */
#define	PAGEPWROF2		( ( psint ) 16 )

#else

/* page size is 1 << 13 -> c. 8192 */
#define	PAGEPWROF2		( ( psint ) 13 )

#endif

#define BPAGESIZE		( ( psint ) 1 << PAGEPWROF2 )	/* page size in bytes */
#define	WPAGEPWROF2		( PAGEPWROF2 - ( psint ) 2 )	/* page size in words is 1 << 11 -> c. 2048 */
#define WPAGESIZE		( ( psint ) 1 << WPAGEPWROF2 )	/* page size in words */
#define PAGEINDEX		( BPAGESIZE - ( psint ) 1 )	/* page index mask for bytes */
#define WPAGEINDEX		( WPAGESIZE - ( psint ) 1 )	/* page index mask for words */
#define PAGENUMBER		( ~PAGEINDEX )			/* page number mask for bytes */
#define LASTWORD		( BPAGESIZE - WORDSIZE )		/* byte address of last word in a page */

/* Address space parameters for this particular stable store */

#if defined(Arch_win32)
/* Data virtual address space size 1GByte */
#define	VASPACESIZE		( (psint) 8 << 27 )
#else
/* Data virtual address space size 384Mbytes */
#define	VASPACESIZE		( (psint) 3 << 27 )
#endif

/* Data virtual address space size in pages */
#define	VASPACEPAGES		( VASPACESIZE >> PAGEPWROF2 )

/* Enough blocks for two copies of the store */
#define	FLMAX			( VASPACEPAGES + VASPACEPAGES )

/* Primary page table size in pages */
#define	PT1PAGES		( ( VASPACEPAGES << 2 ) >>  PAGEPWROF2 )

/* the number of root pages */
#define	ROOTPAGES		( ( psint ) 2 )

#define	LEFTOVERS		( WPAGESIZE - PT1PAGES - PT1PAGES - ( psint ) 10 )

/* extra space for block ordered pagetable */
#define	RESERVEDPAGES		PT1PAGES

/* Starting data page address */
#define	DATAPAGESTART		( ( PT1PAGES + ROOTPAGES ) << PAGEPWROF2 )

/* Starting user data page address */
#define	USERPAGESTART		( ( RESERVEDPAGES << PAGEPWROF2 ) + DATAPAGESTART )

/* Starting data block no. */
#define	DATABLOCKSTART		( PT1PAGES + PT1PAGES + ROOTPAGES )

/* what a root page looks like. NB there are two copies of this in a    */
/* store file. The current one can be identified by comparing the date  */
/* fields at the beginning and end of each one. The current one has the */
/* highest date duplicated in both its time fields.                     */
typedef struct
{
	psint	sstoreMagic ;					/* version number for a napier stable store */
	psint	sheapMagic ;					/* version number for a napier stable heap */
	psint	date1 ;						/* 'date' root page written */
	psint	restart_clock ;					/* the number of system restarts - inc'd by 1 on startup */
	psint	page_size ;					/* size of a page in the shadow store */
	psint	data_start ;					/* first usable data address */
	psint	data_end ;					/* last usable data address */
	psint	no_mod_pages ;					/* average number of modified pages per checkpoint */
	psint	no_syncs ;					/* Number of checkpoints on this store */
	psint	pteModified[ PT1PAGES ] ;			/* Array of true/false was the pte page modified? */
	psint	pteWhere[ PT1PAGES ] ;				/* Array of pte page locations */
	psint	dummy[ LEFTOVERS ] ;				/* Fill up a page with something */
	psint	date2 ;						/* 'date' root page written */
} root_pg ;							/* length should be BPAGESIZE words */

/* version number for the stable stable shadow implementation -
   should change if store layout changes */
#define	SSTOREMAGIC	( ( psint ) 0xf5c00005 )
#define	SSTOREREVERSED	( ( psint ) 0x0500c0f5 )

/* Page table flag definitions */

/* Page is not used - it has no block */
#define	DONTNEED	( ( psint ) 0 )

/* Page has block ; proctect from overwriting */
#define	PROTECT		( ( psint ) 1 << ( psint ) 24 )

/* Page has block ; modify freely */
#define	WRITTEN		( ( psint ) 2 << ( psint ) 24 )

#define	BLOCKLISTED	( ( psint ) 4 << ( psint ) 24 )

/* Save this page at next checkpoint */
#define SAVE		( PROTECT | WRITTEN )

/* Free list manipulation macros */

#define	BBIT( x )		( ( psint ) 1 << ( ( x )  & ( psint ) 31 ) )
#define	BLWRD( x )		( ( x ) >> ( psint ) 5 )

/* 1998/02/18 HK: System adaptions: */
#ifndef SEEK_SET
#define	SEEK_SET	L_SET
#endif

#ifndef SEEK_CUR
#define	SEEK_CUR	L_INCR
#endif

#ifndef SEEK_END
#define SEEK_END	L_XTND
#endif

/* the stable store interfaces */

#include "sstoreif.h"

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
