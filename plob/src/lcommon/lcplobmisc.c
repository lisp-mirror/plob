/* -------------------------------------------------------------------------
| Module	lcplobmisc.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
| Description	PLOB local client functions
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"postore.h"
#include	"lcplob.h"
#include	"lcplobff.h"
#include	"lcplobintern.h"
#include	"lcplobnumber.h"
#include	"lcplobmisc.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( POINTER,
		 fnLISPmalloc, "c-malloc",
		 ( argument ( FIXNUM, value_in, nSizeInBytes ) ) )
{
  POINTER	pMemory	= NULL;

  INITIALIZEPLOB;

  pMemory	= Malloc ( nSizeInBytes );

  RETURN ( pMemory );
} EndFunction ( fnLISPmalloc );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnLISPfree, "c-free",
		( argument ( POINTER, value_in, pMemory ) ) )
{
  INITIALIZEPLOB;

  if ( pMemory != NULL ) {
    Free ( pMemory );
  }
} EndFunction ( fnLISPfree );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
