/* -------------------------------------------------------------------------
| Module	cplobnumber.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/11/07
| Description	PLOB client source code.
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
#include	<rpc/rpc.h>

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobnumber.h"
#include	"cplobsequ.h"
#include	"cplobstruct.h"
#include	"cplobclos.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobroot.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
#define	LOGGING	0x00	/* 0 (no), 4 (peek) (messages) */

/* ----------------------------------------------------------------------- */
static const char	szFixnum []	= "fixnum";

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeNumberModule	( void )
{
  PROCEDURE	( fnInitializeNumberModule );

  RETURN ( VOID );
} /* fnInitializeNumberModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeNumberModule	( void )
{
  PROCEDURE	( fnDeinitializeNumberModule );

  RETURN ( VOID );
} /* fnDeinitializeNumberModule */

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
  ASSERT ( ABS ( nSizeInBits ) > 0 );

  oBignum	=
    fnServerDbMakeBignum ( oShortObjIdHeap, oShortObjIdFormat,
			   nSizeInBits,
			   fnMakeLispPointer ( pnBignum, bDereferencePointer,
					       nUnmask,
					       nBignumDataOffset ) );

  RETURN ( oBignum );
} EndFunction ( fnClientDbMakeBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbMakeSingleFloat, "c-sh-make-single-float",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fFrom ) ) )
{
  SHORTOBJID	oSingle;
  SHLOCK	nLockOld;

  INITIALIZEPLOB;

  oSingle	= fnClientDbCreateObject ( oShortObjIdHeap, eshSingleFloatTag,
					   0, NULLTYPETAG, 0 );
  ASSERT ( oSingle != NULLOBJID );
  nLockOld	=
    fnClientTransactionLockInsert ( oShortObjIdHeap, oShortObjIdHeap,
				    eshLockVectorWrite, oSingle,
				    eshSingleFloatTag, -1 );
  if  ( (int) nLockOld < 0 ) {
    /* Locking the fresh allocated single float failed, e.g. because the 
       heap might be write-locked by someone else: */
    fnClientObjectDestroy ( oShortObjIdHeap, oSingle );
    oSingle	= NULLOBJID;
    RETURN ( nLockOld );
  }
  nLockOld	=
    fnClientObjectWriteSingleFloat ( oShortObjIdHeap, oSingle, fFrom );
  ASSERT ( (int) nLockOld >= 0 );
  RETURN ( oSingle );
} EndFunction ( fnClientDbMakeSingleFloat );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbMakeDoubleFloat, "c-sh-make-double-float",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( DOUBLE_FLOAT, value_in, fFrom ) ) )
{
  SHORTOBJID	oDouble;
  SHLOCK	nLockOld;

  INITIALIZEPLOB;

  oDouble	= fnClientDbCreateObject ( oShortObjIdHeap, eshDoubleFloatTag,
					   0, NULLTYPETAG, 0 );
  ASSERT ( oDouble != NULLOBJID );
  nLockOld	=
    fnClientTransactionLockInsert ( oShortObjIdHeap, oShortObjIdHeap,
				    eshLockVectorWrite, oDouble,
				    eshDoubleFloatTag, -1 );
  if  ( (int) nLockOld < 0 ) {
    /* Locking the fresh allocated double float failed, e.g. because the 
       heap might be write-locked by someone else: */
    fnClientObjectDestroy ( oShortObjIdHeap, oDouble );
    oDouble	= NULLOBJID;
    RETURN ( nLockOld );
  }
  nLockOld	=
    fnClientObjectWriteDoubleFloat ( oShortObjIdHeap, oDouble, fFrom );
  ASSERT ( (int) nLockOld >= 0 );
  RETURN ( oDouble );
} EndFunction ( fnClientDbMakeDoubleFloat );

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
  ASSERT ( ABS ( nSizeInBits ) > 0 );

  nRead		=
    fnServerObjectReadBignum ( oShortObjIdHeap, oShortObjIdFormat,
			       oShortObjId, nSizeInBits,
			       fnMakeLispPointer ( pnBignum,
						   bDereferencePointer,
						   nUnmask,
						   nBignumDataOffset ) );

  RETURN ( (SHLOCK) nRead );
} EndFunction ( fnClientObjectReadBignum );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectReadDoubleFloat, "c-sh-read-double-float",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( DOUBLE_FLOAT, value_out,
			     pfDoubleFloat ) ) )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      RETURN ( (SHLOCK)
	       fnClientObjectReadValues ( oShortObjIdHeap, oShortObjId,
					  (SHORTOBJID) NULLOBJID,
					  eshDoubleFloatTag,
					  0, eshDoubleFloatTag,
					  1, (int *) pfDoubleFloat, 0, 0 ) );
    }
  }

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
  SHTYPETAG	nTypeTag;
  SHLOCK	nLockOld;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    nTypeTag	= eshFixnumTag;
    nLockOld	=
      fnClientObjectReadAtIndex ( oShortObjIdHeap, oShortObjId,
				  oExpectingClass, nExpectingTypeTag,
				  nIndex, pnFixnum, &nTypeTag );
    if ( nTypeTag != eshFixnumTag ) {
      ERROR (( szExpectedAtIndex, szFixnum, nIndex, oShortObjId,
	       nTypeTag ));
      RETURN ( eshGeneralError );
    }
    RETURN ( nLockOld );
  }
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
		  argument ( SINGLE_FLOAT, value_out,
			     pfSingleFloat ) ) )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      RETURN ( (SHLOCK)
	       fnClientObjectReadValues ( oShortObjIdHeap, oShortObjId,
					  (SHORTOBJID) NULLOBJID,
					  eshSingleFloatTag,
					  0, eshSingleFloatTag,
					  1, (int *) pfSingleFloat, 0, 0 ) );
    }
  }

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
  FIXNUM	nWritten;

  INITIALIZEPLOB;

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
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      RETURN ( (SHLOCK)
	       fnClientObjectWriteValues ( oShortObjIdHeap, oShortObjId,
					   (SHORTOBJID) NULLOBJID,
					   eshDoubleFloatTag,
					   0, eshDoubleFloatTag,
					   1, (int *) &fDoubleFloat, 0, 0 ) );
    }
  }

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
  RETURN ( fnClientObjectWriteAtIndex ( oShortObjIdHeap, oShortObjId,
					oExpectingClass, nExpectingTypeTag,
					nIndex, nFixnumWrite, eshFixnumTag ) );
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
  PHEAPOBJECTCACHE	pHeapObjectCache;
  SINGLE_FLOAT		fBuffer	= (SINGLE_FLOAT) fSingleFloat;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      RETURN ( (SHLOCK)
	       fnClientObjectWriteValues ( oShortObjIdHeap, oShortObjId,
					   (SHORTOBJID) NULLOBJID,
					   eshSingleFloatTag,
					   0, eshSingleFloatTag,
					   1, (int *) &fBuffer, 0, 0 ) );
    }
  }

  RETURN ( fnServerObjectWriteSingleFloat ( oShortObjIdHeap, oShortObjId,
					    fBuffer ) );
} EndFunction ( fnClientObjectWriteSingleFloat );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
