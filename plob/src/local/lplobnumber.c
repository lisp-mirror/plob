/* -------------------------------------------------------------------------
| Module	lplobnumber.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1998/07/03
| Description	PLOB local client source code.
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
	  fnMakeBignumPointer ( pnBignum, bDereferencePointer,
				nUnmask,
				nBignumDataOffset ) ));
#endif /* #if (LOGGING+0) & 0x01 */

  oBignum	=
    fnServerDbMakeBignum ( oShortObjIdHeap, oShortObjIdFormat,
			   nSizeInBits,
			   fnMakeBignumPointer ( pnBignum, bDereferencePointer,
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
			       fnMakeBignumPointer ( pnBignum,
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
