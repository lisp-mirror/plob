/* -------------------------------------------------------------------------
| Module	plobheap.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB source code common for server and client.
|
| Copyright	PLOB! Copyright 1994--2006 Heiko Kirschke.
|		All rights reserved.
|
| Unlimited use, reproduction, modification and distribution of this
| software is permitted.  Any copy or modified version of this
| software must include both the above copyright notice of
| Heiko Kirschke and this paragraph; for each modified version, an
| additional statement must be added telling the year of modification
| and quoting the author of the modification.  Any distribution of
| this software must comply with all applicable German export control
| laws.  This software is made available AS IS, and HEIKO KIRSCHKE
| DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT
| LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
| A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION
| CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM THE
| SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
| CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
| HEIKO KIRSCHKE IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
|
| Please note that these license terms adhere only to the code of
| PLOB!  itself. PLOB! uses POSTORE (Persistent Object Store) as a
| low-level persistent memory; it is provided in binary form within
| PLOB! with the permission of the University of St. Andrews
| (http://www-ppg.dcs.st-andrews.ac.uk/Default.html).  Contact the
| University of St. Andrews for getting their license terms on
| POSTORE.
|
| $Id$
|
 ------------------------------------------------------------------------- */

#include	<errno.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobintern.h"
#include	"plobmisc.h"
#include	"plobtype.h"
#include	"plobroot.h"
#include	"ploblock.h"
#include	"plobheap.h"
#include	"plobstruct.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitCommonHeapModule	( void )
{
  static CLASSINFO	ClassInfoTable []	= {
    { (SHTYPETAG) eshHeapTag,
        SZPLOB,
	"heap",
	(sizeof(PLOBHEAP)-sizeof(PLOBHEADER)-
	 sizeof(time_t)-2*sizeof(FILEPOS)-sizeof(HASHTABLE))*nBitsPerByte,
	(sizeof(time_t)+2*sizeof(FILEPOS)+sizeof(HASHTABLE))*nBitsPerByte,
	(LPCSTR) NULL,
        /* 1997/09/04 HK: Setted heaps to being recycleable, to handle
           alloc requests from client without running out of memory: */
        /* typeNoFlags */
        (TYPEFLAGS) ( (unsigned int) typeRecycleP |
		      (unsigned int) typeNotCachableP ) }
  };

  int		i;

  PROCEDURE	( fnInitCommonHeapModule );

  /* Register classes: */
  for ( i = 0; i < length ( ClassInfoTable ); i++ ) {
    RegisterPlobClass ( & ClassInfoTable [ i ] );
  }

  RETURN ( VOID );
} /* fnInitCommonHeapModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitCommonHeapModule	( void )
{
  PROCEDURE	( fnDeinitCommonHeapModule );

  RETURN ( VOID );
} /* fnDeinitCommonHeapModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
