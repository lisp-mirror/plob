/* -------------------------------------------------------------------------
| Module	plobnumber.h.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/11/07
| Description	Foreign language interface to postore persistent heap.
|		Number handling
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

#if defined(LISP)
;;;; -------------------------------------------------------------------------
;;;; For further comments look into file plobnumber.h
;;;; -------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

/* -------------------------------------------------------------------------
| Floats
 ------------------------------------------------------------------------- */
DefineConstant ( eshSingleFloatValueSize,
		 "+single-float-value-size+", 1,
		 "Size of a single float number in words." );

#if ! defined(RPC)	/* client: */
DefineFunction ( DOUBLE_FLOAT,
		 fnShortToSingleFloat, "c-short-to-single-float",
		 ( argument ( SHORTOBJID, value_in, oShortFloat ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHORTOBJID,
		 fnShortMakeSingleFloat, "c-sh-make-single-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SINGLE_FLOAT, value_in, fFrom ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientDbMakeSingleFloat, "c-sh-make-single-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fFrom ) ) );
#endif	/* ! RPC */

DefineConstant ( eshDoubleFloatValueSize,
		 "+double-float-value-size+", 2,
		 "Size of a double float number in words." );

#if ! defined(LISP)	/* server: */
DefineFunction ( SHORTOBJID,
		 fnShortMakeDoubleFloat, "c-sh-make-double-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fFrom ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientDbMakeDoubleFloat, "c-sh-make-double-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fFrom ) ) );
#endif	/* ! RPC */

/* -------------------------------------------------------------------------
| Ratio
 ------------------------------------------------------------------------- */
BeginEnum ( SHRATIOIDX )
  enumerator ( eshRatioIdxNumerator, "+ratio-location-numerator+", 0,
	       "Index of plob ratio numerator." )
  and
  enumerator ( eshRatioIdxDenominator, "+ratio-location-denominator+", 1,
	       "Index of plob ratio denominator." )
  and
  enumerator ( eshRatioObjIdSize, "+ratio-size+", 2,
	       "Size of plob ratio in words." )
EndEnum ( SHRATIOIDX );

/* -------------------------------------------------------------------------
| Complex
 ------------------------------------------------------------------------- */
BeginEnum ( SHCOMPLEXIDX )
  enumerator ( eshComplexIdxRealPart, "+complex-location-real-part+", 0,
	       "Index of plob complex real part." )
  and
  enumerator ( eshComplexIdxImagPart, "+complex-location-imag-part+", 1,
	       "Index of plob complex imaginary part." )
  and
  enumerator ( eshComplexObjIdSize, "+complex-size+", 2,
	       "Size of plob complex in words." )
EndEnum ( SHCOMPLEXIDX );

/* -------------------------------------------------------------------------
| Complex
 ------------------------------------------------------------------------- */
BeginEnum ( SHBIGNUMIDX )
  enumerator ( eshBignumIdxFormat, "+bignum-location-format+", 0,
	       "Index of plob bignum format. This dereferences the symbol\
 \\lisp{:ALLEGRO}\\ for allegro-type bignums and\
 \\lisp{:LISPWORKS}\\ for \\lw-type bignums." )
  and
  enumerator ( eshBignumIdxSize, "+bignum-location-size+", 1,
	       "Index of plob bignum size. The size is in bits;\
 it is negative for negative bignums\
 and positive for positive bignums." )
  and
  enumerator ( eshBignumObjIdSize, "+bignum-size+", 2,
	       "Size of plob bignum in words." )
EndEnum ( SHBIGNUMIDX );

#if ! defined(LISP)	/* server: */
DefineFunction ( SHORTOBJID,
		 fnServerDbMakeBignum, "c-sh-make-bignum",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdFormat )
		   and
		   argument ( FIXNUM, value_in, nSizeInBits )
		   and
		   argument ( VECTOR
			      ( as_is,
				AlignBitsToWords ( ABS ( nSizeInBits ) ) ),
			      vector_in, pnBignum ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
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
		   argument ( FIXNUM, value_in, nBignumDataOffset ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectReadBignum, "c-sh-read-bignum",
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
			      vector_out, pnBignum ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
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
		   argument ( FIXNUM, value_in, nBignumDataOffset ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
	         fnServerObjectReadDoubleFloat, "c-sh-read-double-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( DOUBLE_FLOAT, value_out,
			      pfDoubleFloat ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
	         fnClientObjectReadDoubleFloat, "c-sh-read-double-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( DOUBLE_FLOAT, value_out,
			      pfDoubleFloat ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectReadFixnum, "c-sh-read-fixnum",
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
		   argument ( FIXNUM, value_out, pnFixnum ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
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
		   argument ( FIXNUM, value_out, pnFixnum ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectReadSingleFloat, "c-sh-read-single-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( SINGLE_FLOAT, value_out,
			      pfSingleFloat ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientObjectReadSingleFloat, "c-sh-read-single-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( SINGLE_FLOAT, value_out,
			      pfSingleFloat ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectWriteBignum, "c-sh-write-bignum",
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
			      vector_in, pnBignum ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
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
		   argument ( FIXNUM, value_in, nBignumDataOffset ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectWriteDoubleFloat, "c-sh-write-double-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fDoubleFloat ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientObjectWriteDoubleFloat, "c-sh-write-double-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( DOUBLE_FLOAT, value_in,
			      fDoubleFloat ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectWriteFixnum, "c-sh-write-fixnum",
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
		   argument ( FIXNUM, value_in, nFixnumWrite ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
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
		   argument ( FIXNUM, value_in, nFixnumWrite ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectWriteSingleFloat, "c-sh-write-single-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( SINGLE_FLOAT, value_in, fSingleFloat ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientObjectWriteSingleFloat, "c-sh-write-single-float",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( DOUBLE_FLOAT, value_in, fSingleFloat ) ) );
#endif	/* ! RPC */

#if defined(C2C)

/* -------------------------------------------------------------------------
| Symbol names
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR const char	szALLEGRO []		/* = "ALLEGRO" */;
extern DLLEXPORTVAR const char	szALLEGRO4 []		/* = "ALLEGRO4" */;
extern DLLEXPORTVAR const char	szALLEGRO5 []		/* = "ALLEGRO5" */;
extern DLLEXPORTVAR const char	szLISPWORKS []		/* = "LISPWORKS" */;
extern DLLEXPORTVAR const char	szLISPWORKS3 []		/* = "LISPWORKS3" */;
extern DLLEXPORTVAR const char	szLISPWORKS4 []		/* = "LISPWORKS4" */;

extern DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro/* = NULLOBJID */;
extern DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro4/* = NULLOBJID */;
extern DLLEXPORTVAR OBJID	oGlobalSymKeywordAllegro5/* = NULLOBJID */;
extern DLLEXPORTVAR OBJID	oGlobalSymKeywordLispworks/* = NULLOBJID */;
extern DLLEXPORTVAR OBJID	oGlobalSymKeywordLispworks3/* = NULLOBJID */;
extern DLLEXPORTVAR OBJID	oGlobalSymKeywordLispworks4/* = NULLOBJID */;

/* -------------------------------------------------------------------------
| ObjId <-> fixnum
 ------------------------------------------------------------------------- */

#define	fixnump( oSelf )			\
((((unsigned int)(oSelf))&nFixnumTagMask)==eshFixnumTag)

extern DLLEXPORTVAR const char	szObjIdNotFixnum []	/* = */
/* "The long objid %d is no immediate fixnum." */;

#define	OBJID2FIXNUM( oSelf )			\
(((int)(oSelf))>>nFixnumBitOffset)

extern DLLEXPORTVAR OBJID		__oFixnum__;

#define	ObjId2Fixnum( oSelf )			\
(__oFixnum__=(oSelf),				\
 (fixnump(__oFixnum__))?			\
 OBJID2FIXNUM(__oFixnum__):			\
 (ERROR((szObjIdNotFixnum,(__oFixnum__))),0))

#define	FIXNUM2OBJID( nFixnum )			\
(OBJID)((((unsigned int)(nFixnum))<<nFixnumBitOffset)|eshFixnumTag)

#define	Fixnum2ObjId( nFixnum )			FIXNUM2OBJID(nFixnum)

#define	IncObjId( oSelf, nIncrement )		\
oSelf=FIXNUM2OBJID(ObjId2Fixnum(oSelf)+(nIncrement))

#define	INCOBJID( oSelf, nIncrement )		\
oSelf=FIXNUM2OBJID(OBJID2FIXNUM(oSelf)+(nIncrement))

enum {
  oM2	= Fixnum2ObjId ( -2 ),
  oM1	= Fixnum2ObjId ( -1 ),
  o0	= Fixnum2ObjId (  0 ),
  o1	= Fixnum2ObjId (  1 ),
  o2	= Fixnum2ObjId (  2 )
};

/* -------------------------------------------------------------------------
| ObjId <-> float
 ------------------------------------------------------------------------- */

extern DLLEXPORTVAR const char	szObjIdNotShortFloat []	/* = */
/* "The long objid %d is no immediate short float." */;

extern DLLEXPORTVAR float	__fBuffer__;

#define	OBJID2SINGLEFLOAT( oSelf )		\
(*(unsigned int*)&__fBuffer__=			\
 (((unsigned int)oSelf)&~(unsigned int)nTagMask)<<(5-nTagBits),\
 __fBuffer__)

extern DLLEXPORTVAR OBJID	__oShortFloat__;

#define	ObjId2SingleFloat( oSelf )		\
(__oShortFloat__=(oSelf),			\
 (shortfloatp(__oShortFloat__))?		\
 OBJID2SINGLEFLOAT(__oShortFloat__):		\
 (ERROR((szObjIdNotShortFloat,(__oShortFloat__))),0))

#define	make_float( f )		fnMakeFloat(f)
#define	make_double( f )	fnMakeDouble(f)

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMakeFloat		( float fFrom );
OBJID DLLEXPORT	fnMakeDouble		( double fFrom );

/* -------------------------------------------------------------------------
| Float printing
 ------------------------------------------------------------------------- */
typedef enum {
  cExponentShortFloat	= 's',
  cExponentSingleFloat	= 'e',
  cExponentDoubleFloat	= 'd'
}	EXPCHAR, * PEXPCHAR;
LPSTR DLLEXPORT	fnPrintFloat		( LPSTR		lpszFloat,
					  double	fFloat,
					  EXPCHAR	cExponent );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitCommonNumberModule	( void );
void		fnInitializeNumberModule	( void );
void		fnDeinitializeNumberModule	( void );
void		fnDeinitCommonNumberModule	( void );

#endif /* #if defined(C2C) */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
