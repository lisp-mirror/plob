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
/*                                                            */
/**************************************************************/

/* the type of a 32bit integer: long or int, as required. */
typedef int psint ;
typedef double psreal ;

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

/* BPAGESIZE parameters for this particular stable store */

#define	PAGEPWROF2		( ( psint ) 13 )		/* page size is 1 << 13 -> c. 8192 */
#define BPAGESIZE		( ( psint ) 1 << PAGEPWROF2 )	/* page size in bytes */
#define WPAGESIZE		( BPAGESIZE >> ( psint ) 2 )	/* page size in words */
#define PAGEINDEX		( BPAGESIZE - ( psint ) 1 )	/* page index mask for bytes */
#define WPAGEINDEX		( WPAGESIZE - ( psint ) 1 )	/* page index mask for words */
#define PAGENUMBER		( ~PAGEINDEX )			/* page number mask for bytes */
#define LASTWORD		( BPAGESIZE - WORDSIZE )		/* byte address of last word in a page */

/* Address space parameters for this particular stable store */

#define	DATASPACESIZE		( ( psint ) ( 48 * 1024 ) * BPAGESIZE )		/* Data virtual address space size in bytes */
#define	DATASPACEPAGES		( DATASPACESIZE >> PAGEPWROF2 )			/* Data virtual address space size in pages */
#define	PT1PAGES		( ( DATASPACEPAGES << 2 ) >>  PAGEPWROF2 )	/* Primary page table size in pages */
#define	FREEPAGES		( ( ( ( DATASPACEPAGES + PT1PAGES + 7 ) / 8 ) + PAGEINDEX ) >>  PAGEPWROF2 )	/* Size of free list in pages */
#define	LEFTOVERS		( WPAGESIZE - PT1PAGES - ( psint ) 7 )
#define	VASPACEPAGES		( DATASPACEPAGES + PT1PAGES + ( psint ) 2 )	/* Total virtual address space size in pages */
#define	VASPACESIZE		( VASPACEPAGES << PAGEPWROF2 )			/* Total virtual address space size in bytes */

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
	psint	store_length ;					/* length of the store in bytes */
	psint	pt2[ PT1PAGES ] ;				/* Secondary page table */
	psint	dummy[ LEFTOVERS ] ;					/* Fill up a page with something */
	psint	date2 ;						/* 'date' root page written */
} root_pg ;							/* length should be BPAGESIZE words */

/* version number for the stable stable shadow implementation - should change if store layout changes */
#define	SSTOREMAGIC	( ( psint ) 0xf5c00004 )

/* Page table flag definitions */

#define	RO	( ( psint ) 1 << 24 ) 			/* Page is read only */
#define	SP	( ( psint ) 2 << 24 ) 			/* Page is shadow page */
#define	SO	( ( psint ) 4 << 24 ) 			/* Page is save only */
#define	SC	( ( psint ) 8 << 24 ) 			/* Page is scratch */
#define	RE	( ( psint ) 16 << 24 ) 			/* Page is reserved */
#define	DN	( ( psint ) 32 << 24 ) 			/* Page is dont need */

#define WRITTEN		( SP | SO )			/* Only these types of pages constitute "written" */
#define PROTECTED	( RO | RE )			/* Only these types of pages constitute "protected" */
#define RESTORE		( SP | SO | RO )		/* These types of pages get rstored after stabilise or init */

/* Free list manipulation macros */

#define	BBIT( x )		( ( psint ) 1 << ( ( x )  & ( psint ) 31 ) )
#define	BLWRD( x )		( ( x ) >> ( psint ) 5 )

/* the stable store interfaces */

#include "sstoreif.h"
