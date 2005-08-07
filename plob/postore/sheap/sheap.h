/**************************************************************/
/*                                                            */
/*                Defines for the stable heap                 */
/*                      28 June 1989                          */
/*                                                            */
/**************************************************************/
/*                                                            */
/* created   28/6/89   ALB                                    */
/*                                                            */
/**************************************************************/

#if defined(Arch_win32)
/* 1998/02/17 HK: Added Windows NT */
#include	<windows.h>
#endif

/* 2005-04-19 hkirschk: Already defined in sstore.h: */
/* #define	WORDSIZE	( ( psint ) 4 ) */		/* the size of a WORD -> psint, in bytes */
#define	WORDPWROF2	( ( psint ) 2 )				/* the size of a WORD as a power of 2 */

/* 2005-04-19 hkirschk: Already defined in sstore.h: */
/* #define	PSTRUE		( ( psint ) 1 ) */
/* 2005-04-19 hkirschk: Already defined in sstore.h: */
/* #define	PSFALSE		( ( psint ) 0 ) */

/* Bit Masks */

/* 2005-04-19 hkirschk: Already defined in sstore.h: */
/* #define BIT_31		( ( psint ) 020000000000 ) */	/* bit 31 */
/* 2005-04-19 hkirschk: Already defined in sstore.h: */
/* #define	LOWER24		( ( psint ) 0x00FFFFFF ) */	/* low 24 bits */

/* version number for the stable heap implementation - should change if heap layout changes */
#define	SHEAPMAGIC	( ( psint ) 0x5 )

/* support for generation scavenging - requires gen size for free space & ix space + mark bits & masks for marking */
#define	MARKBIT		( ( psint ) 0x1 )			/* mark bit */
#define	ADDRESSBITS	( ~MARKBIT )				/* mask for address bits - the mark bit is unused */
#define	KEYALIGNMENT	( ( psint ) 8 )				/* all keys are aligned on a multiple of 8 bytes - 2 words */

#define	LOCKWORD	( ( psint ) -1 )			/* word relative to obj address holding object's lock word */

#define	IXWORDS		( ( psint ) 2 )				/* number of words forming an IX entry */
#define	IXSIZE		( IXWORDS * WORDSIZE )			/* size of words forming an IX entry */

#define	GENDATASIZE	( ( psint ) 256 * 1024 )		/* a generation contains at most 0.25M new data */
#define	GENIXSIZE	( ( psint ) 256 * 16 * IXSIZE )		/* a generation contains at most 4K objects - 64 words/object */

#define	PREFIXWORDS	( ( psint ) 1 )				/* number of words prefixing objects - used for locks etc */
#define	PREFIXSIZE	( PREFIXWORDS * WORDSIZE )		/* size of words prefixing objects */
#define	HEADERWORDS	( ( psint ) 2 )				/* a header is a pntr count + a size */
#define HEADERSIZE	( HEADERWORDS * WORDSIZE )		/* the header in bytes */
#define	MINOBJECTSIZE	( ( psint ) 2 )				/* minimum size of object possible */

#define	SHDATA		( ( psint ) 2 * WORDSIZE )		/* the location of the data pointer field in the root object */
#define	SHMODE		( SHDATA + WORDSIZE ) 			/* locn of sheap mode indicates gc progress or normal operatin */
#define	SHDATAFREE	( SHMODE + WORDSIZE )			/* start of free data space */
#define	SHDATAEND	( SHDATAFREE + WORDSIZE )		/* end of free data space */
#define	SHIXFREE	( SHDATAEND + WORDSIZE )		/* start of free IX space */
#define	SHIXEND		( SHIXFREE + WORDSIZE )			/* end of free IX space */
#define	SHTPNTR		( SHIXEND + WORDSIZE )			/* temp used in restarting gc */
#define	SHTIX		( SHTPNTR + WORDSIZE )			/* temp used in restarting gc */
#define	SHTO		( SHTIX + WORDSIZE )			/* temp used in restarting gc */
#define	SHFROM		( SHTO + WORDSIZE )			/* temp used in restarting gc */
#define	SHHDR		( SHFROM + WORDSIZE )			/* temp used in restarting gc */
#define	SHNPNTRS	( SHHDR + WORDSIZE )			/* temp used in restarting gc */
#define	SHNOTPNTRS	( SHNPNTRS + WORDSIZE )			/* temp used in restarting gc */

								/* the size of the root object in the stable heap */
#define	SHROOTSIZE	( ( SHNOTPNTRS + WORDSIZE ) / WORDSIZE )

#include "sheapif.h"

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
