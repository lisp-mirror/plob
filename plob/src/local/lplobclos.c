/* -------------------------------------------------------------------------
| Module	plob.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
| Description	PLOB local source code.
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

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobintern.h"
#include	"lplobmisc.h"
#include	"lplobtype.h"
#include	"lplobstruct.h"
#include	"lploblock.h"
#include	"lplobclos.h"
#include	"lplobheap.h"
#include	"lplobbtree.h"
#include	"lplobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbCreateInstance, "c-sh-create-instance",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in,
			     oShortObjIdClassDescr ) ) )
{
  SHORTOBJID	oInstance = NULLOBJID;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  fnServerDbCreateInstances ( oShortObjIdHeap, oShortObjIdClassDescr,
			1, (u_int *) &oInstance, (u_int *) NULL,
			(u_int *) NULL );
  RETURN ( oInstance );
} EndFunction ( fnClientDbCreateInstance );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientInstanceWriteWrapper,
		"c-sh-write-instance-wrapper",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdClassDescr ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerInstanceWriteWrapper ( oShortObjIdHeap, oShortObjIdInstance,
				      oShortObjIdClassDescr ) );
} EndFunction ( fnClientInstanceWriteWrapper );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientInstanceWriteData,
		"c-sh-write-instance-data",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdData ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerInstanceWriteData ( oShortObjIdHeap, oShortObjIdInstance,
				   oShortObjIdData ) );
} EndFunction ( fnClientInstanceWriteData );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
