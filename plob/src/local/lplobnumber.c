/* -------------------------------------------------------------------------
| Module	lplobnumber.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
| Description	PLOB local client source code.
|
| Copyright	PLOB! Copyright 1994--2002 Heiko Kirschke.
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

#include	<limits.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lplob.h"
#include	"lplobintern.h"
#include	"lplobmisc.h"
#include	"lplobtype.h"
#include	"lplobnumber.h"
#include	"lplobsequ.h"
#include	"lplobstruct.h"
#include	"lplobclos.h"
#include	"lploblock.h"
#include	"lplobheap.h"
#include	"lplobbtree.h"
#include	"lplobroot.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
#define	LOGGING	0x00	/* 0 (no), 1 (make-bignum) */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbMakeSingleFloat, "c-sh-make-single-float",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fFrom ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnShortMakeSingleFloat ( oShortObjIdHeap, fFrom ) );
} EndFunction ( fnClientDbMakeSingleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbMakeDoubleFloat, "c-sh-make-double-float",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fFrom ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnShortMakeDoubleFloat ( oShortObjIdHeap, fFrom ) );
} EndFunction ( fnClientDbMakeDoubleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbMakeBignum, "c-sh-make-bignum",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdFormat )
		  and
		  argument ( FIXNUM, value_in, nSizeInBits )
		  and
		  argument ( VECTOR
			     ( as_is,
			       AlignBitsToWords ( ABS ( nSizeInBits ) ) ),
			     vector_in, pnBignum )
		  and
		  /* A flag if the pointer passed in at pnBignum must
		     be dereferenced to get the real bignum pointer. */
		  argument ( BOOL, value_in, bDereferencePointer )
		  and
		  argument ( FIXNUM, value_in, nUnmask )
		  and
		  argument ( FIXNUM, value_in, nBignumDataOffset ) ) )
{
  SHORTOBJID	oBignum = NULLOBJID;

  INITIALIZEPLOB;
  UNSTORESESSION ();

#if (LOGGING+0) & 0x01
  INFO (( "nSizeInBits %d, bDeref %d, nUnmask %d, nOffset %d\n"
	  "       raw ptr 0x%X, aligned ptr 0x%X",
	  nSizeInBits, bDereferencePointer, nUnmask, nBignumDataOffset,
	  pnBignum,
	  fnMakeLispPointer ( pnBignum, bDereferencePointer,
			      nUnmask,
			      nBignumDataOffset ) ));
#endif /* #if (LOGGING+0) & 0x01 */

  oBignum	=
    fnServerDbMakeBignum ( oShortObjIdHeap, oShortObjIdFormat,
			   nSizeInBits,
			   fnMakeLispPointer ( pnBignum,
					       bDereferencePointer,
					       nUnmask,
					       nBignumDataOffset ) );

  RETURN ( oBignum );
} EndFunction ( fnClientDbMakeBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectReadBignum, "c-sh-read-bignum",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdFormat )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( FIXNUM, value_in, nSizeInBits )
		  and
		  argument ( VECTOR
			     ( as_is,
			       AlignBitsToWords ( ABS ( nSizeInBits ) ) ),
			     vector_out, pnBignum )
		  and
		  /* A flag if the pointer passed in at pnBignum must
		     be dereferenced to get the real bignum pointer. */
		  argument ( BOOL, value_in, bDereferencePointer )
		  and
		  argument ( FIXNUM, value_in, nUnmask )
		  and
		  argument ( FIXNUM, value_in, nBignumDataOffset ) ) )
{
  FIXNUM	nRead;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  ASSERT ( ABS ( nSizeInBits ) > 0 );

  nRead	=
    fnServerObjectReadBignum ( oShortObjIdHeap, oShortObjIdFormat,
			       oShortObjId, nSizeInBits,
			       fnMakeLispPointer ( pnBignum,
						   bDereferencePointer,
						   nUnmask,
						   nBignumDataOffset ) );

  RETURN ( nRead );
} EndFunction ( fnClientObjectReadBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectReadDoubleFloat, "c-sh-read-double-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( DOUBLE_FLOAT, value_out, pfDoubleFloat ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  ASSERT ( pfDoubleFloat != NULL );

  RETURN ( (SHLOCK)
	   fnServerObjectReadDoubleFloat ( oShortObjIdHeap, oShortObjId,
					   pfDoubleFloat ) );
} EndFunction ( fnClientObjectReadDoubleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectReadFixnum, "c-sh-read-fixnum",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( INTEGER, value_out, pnFixnum ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  ASSERT ( pnFixnum != NULL );

  RETURN ( fnServerObjectReadFixnum ( oShortObjIdHeap, oShortObjId,
				      oExpectingClass, nExpectingTypeTag,
				      nIndex, pnFixnum ) );
} EndFunction ( fnClientObjectReadFixnum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectReadSingleFloat, "c-sh-read-single-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SINGLE_FLOAT, value_out, pfSingleFloat ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();
  ASSERT ( pfSingleFloat != NULL );

  RETURN ( fnServerObjectReadSingleFloat ( oShortObjIdHeap, oShortObjId,
					   pfSingleFloat ) );
} EndFunction ( fnClientObjectReadSingleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectWriteBignum, "c-sh-write-bignum",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdFormat )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( FIXNUM, value_in, nSizeInBits )
		  and
		  argument ( VECTOR
			     ( as_is,
			       AlignBitsToWords ( ABS ( nSizeInBits ) ) ),
			     vector_out, pnBignum )
		  and
		  /* A flag if the pointer passed in at pnBignum must
		     be dereferenced to get the real bignum pointer. */
		  argument ( BOOL, value_in, bDereferencePointer )
		  and
		  argument ( FIXNUM, value_in, nUnmask )
		  and
		  argument ( FIXNUM, value_in, nBignumDataOffset ) ) )
{
  FIXNUM	nWritten;

  INITIALIZEPLOB;
  UNSTORESESSION ();

  ASSERT ( ABS ( nSizeInBits ) > 0 );

  nWritten	=
    fnServerObjectWriteBignum ( oShortObjIdHeap, oShortObjIdFormat,
				oShortObjId, nSizeInBits,
				fnMakeLispPointer ( pnBignum,
						    bDereferencePointer,
						    nUnmask,
						    nBignumDataOffset ) );
  RETURN ( (SHLOCK) nWritten );
} EndFunction ( fnClientObjectWriteBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectWriteDoubleFloat, "c-sh-write-double-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fDoubleFloat ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerObjectWriteDoubleFloat ( oShortObjIdHeap, oShortObjId,
					    fDoubleFloat ) );
} EndFunction ( fnClientObjectWriteDoubleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectWriteFixnum, "c-sh-write-fixnum",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( FIXNUM, value_in, nFixnumWrite ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerObjectWriteFixnum ( oShortObjIdHeap, oShortObjId,
				       oExpectingClass, nExpectingTypeTag,
				       nIndex, nFixnumWrite ) );
} EndFunction ( fnClientObjectWriteFixnum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectWriteSingleFloat, "c-sh-write-single-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fSingleFloat ) ) )
{
  INITIALIZEPLOB;
  UNSTORESESSION ();

  RETURN ( fnServerObjectWriteSingleFloat ( oShortObjIdHeap, oShortObjId,
					    fSingleFloat ) );
} EndFunction ( fnClientObjectWriteSingleFloat );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
