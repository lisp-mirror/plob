/* -------------------------------------------------------------------------
| Module	lploblock.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
| Description	PLOB local source code.
|
| Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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
| $Header$
|
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobintern.h"
#include	"lplobmisc.h"
#include	"lplobtype.h"
#include	"lplobnumber.h"
#include	"lplobroot.h"
#include	"lploblock.h"
#include	"lplobbtree.h"
#include	"lplobheap.h"
#include	"lplobsequ.h"
#include	"lplobclos.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientTransactionLockInsert, "c-sh-insert-lock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		  and
		  argument ( SHLOCK, value_in, nLock )
		  and
		  argument ( SHORTOBJID, value_in, oShortToLock )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagToLock )
		  and
		  argument ( FIXNUM, value_in, nIndex ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionLockInsert ( oShortObjIdHeap, oShortObjIdLockBy,
					   nLock, oShortToLock,
					   nTypeTagToLock, nIndex,
					   (FIXNUM *) NULL,
					   (FIXNUM *) NULL ) );
} EndFunction ( fnClientTransactionLockInsert );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientTransactionLockSet, "c-sh-set-lock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		  and
		  argument ( SHLOCK, value_in, nLock )
		  and
		  argument ( SHORTOBJID, value_in, oShortToLock )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagToLock )
		  and
		  argument ( FIXNUM, value_in, nIndex ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionLockSet ( oShortObjIdHeap, oShortObjIdLockBy,
					nLock, oShortToLock, nTypeTagToLock,
					nIndex,
					(FIXNUM *) NULL, (FIXNUM *) NULL ) );
} EndFunction ( fnClientTransactionLockSet );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnShortUnlock, "c-sh-unlock",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		  and
		  argument ( SHLOCK, value_in, nLock )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock )
		  and
		  argument ( FIXNUM, value_in, nIndex ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionUnlock ( oShortObjIdHeap, oShortObjIdLockedBy,
			nLock, oShortToUnlock, nIndex, (SHLOCK *) NULL ) );
} EndFunction ( fnShortUnlock );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnShortUnlockAll, "c-sh-unlock-all",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionUnlockAll ( oShortObjIdHeap, oShortObjIdLockedBy,
			   oShortToUnlock ) );
} EndFunction ( fnShortUnlockAll );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnShortUnlockAllAll, "c-sh-unlock-all-all",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortToUnlock ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerTransactionUnlockAllAll ( oShortObjIdHeap, oShortToUnlock ) );
} EndFunction ( fnShortUnlockAllAll );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
