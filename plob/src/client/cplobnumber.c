/* -------------------------------------------------------------------------
| Module	cplobnumber.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/11/07
| Description	PLOB client source code.
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
			   fnMakeBignumPointer ( pnBignum, bDereferencePointer,
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
			       fnMakeBignumPointer ( pnBignum,
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
					  1, (int *) pfDoubleFloat ) );
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
					  1, (int *) pfSingleFloat ) );
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
				fnMakeBignumPointer ( pnBignum,
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
					   1, (int *) &fDoubleFloat ) );
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
					   1, (int *) &fBuffer ) );
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
