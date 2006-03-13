/* -------------------------------------------------------------------------
| Module	plobsequence.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		4.3.94
| Description	PLOB sequence functions.
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

#if defined(LISP)
;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plobsequ.h
;;;; --------------------------------------------------------------------------

#include	"plobconst.h"

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

/* -------------------------------------------------------------------------
| Cons
 ------------------------------------------------------------------------- */

DefineConstant ( eshConsTag, "+cons-type-tag+", hex ( 08 ),
		 "Type tag for objects of type CONS." );

BeginEnum ( SHCONSIDX )
  enumerator ( eshConsIdxCar, "+cons-location-car+", 0,
	       "Index of plob cons car." )
  and
  enumerator ( eshConsIdxCdr, "+cons-location-cdr+", 1,
	       "Index of plob cons cdr." )
  and
  enumerator ( eshConsObjIdSize, "+cons-size+", 2,
	       "Size of plob cons cell in words." )
EndEnum ( SHCONSIDX );

/* -------------------------------------------------------------------------
| Vector
 ------------------------------------------------------------------------- */

DefineConstant ( eshVectorTag, "+vector-type-tag+", hex ( 40 ),
		 "Type tag for plob objects of type VECTOR." );

BeginEnum ( SHVECTORIDX )
  enumerator ( eshVectorObjIdSize, "+vector-size+", 0,
	       "Fix size of plob vector in words." )
EndEnum ( SHVECTORIDX );

/* -------------------------------------------------------------------------
| IVectors
| IVectors are vectors holding elements of only one type.
 ------------------------------------------------------------------------- */

DefineConstant ( eshIVectorTag, "+ivector-type-tag+", hex ( C0 ),
		 "Type tag for plob objects of type IVECTOR. The 'I' means\
 'immediate', i.e. ivectors hold immediate values of a single type." );

BeginEnum ( IVECTORTAG )
  enumerator ( eshSignedByte2Tag, "+signed-byte-2-tag+",
	       bitwise_or ( hex ( 1700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (signed-byte 2)." )
  and
  enumerator ( eshSignedByte4Tag, "+signed-byte-4-tag+",
	       bitwise_or ( hex ( 2700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (signed-byte 4)." )
  and
  enumerator ( eshSignedByte8Tag, "+signed-byte-8-tag+",
	       bitwise_or ( hex ( 3700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (signed-byte 8)." )
  and
  enumerator ( eshSignedByte16Tag, "+signed-byte-16-tag+",
	       bitwise_or ( hex ( 4700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (signed-byte 16)." )
  and
  enumerator ( eshSignedByte32Tag, "+signed-byte-32-tag+",
	       bitwise_or ( hex ( 5700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (signed-byte 32)." )
  and
  enumerator ( eshUnsignedByte1Tag, "+unsigned-byte-1-tag+",
	       bitwise_or ( hex ( B700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (unsigned-byte 1)." )
  and
  enumerator ( eshUnsignedByte2Tag, "+unsigned-byte-2-tag+",
	       bitwise_or ( hex ( 6700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (unsigned-byte 2)." )
  and
  enumerator ( eshUnsignedByte4Tag, "+unsigned-byte-4-tag+",
	       bitwise_or ( hex ( 7700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (unsigned-byte 4)." )
  and
  enumerator ( eshUnsignedByte8Tag, "+unsigned-byte-8-tag+",
	       bitwise_or ( hex ( 8700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (unsigned-byte 8)." )
  and
  enumerator ( eshUnsignedByte16Tag, "+unsigned-byte-16-tag+",
	       bitwise_or ( hex ( 9700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (unsigned-byte 16)." )
  and
  enumerator ( eshUnsignedByte32Tag, "+unsigned-byte-32-tag+",
	       bitwise_or ( hex ( A700 ), ESHMARKERTAG ),
	       "Type tag for IVector with element type (unsigned-byte 32)." )
EndEnum ( IVECTORTAG );

BeginEnum ( IVECTORIDX )
  enumerator ( eshIVectorIdxType, "+ivector-location-type+", 0,
	       "Index of PLOB ivector type field. It contains a fixnum type\
 tag to which the IVector elements are specialized." )
  and
  enumerator ( eshIVectorIdxLength, "+ivector-location-length+", 1,
	       "Index of PLOB ivector length field. It contains the number\
 of elements the ivector contains." )
  and
  enumerator ( eshIVectorObjIdSize, "+ivector-size+", 2,
	       "Fix size of plob vector in words." )
EndEnum ( IVECTORIDX );

DefineFunction ( SHORTOBJID,
		 fnShortMakeIVector, "c-sh-make-ivector",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( FIXNUM, value_in, nElements ) ) );

/* -------------------------------------------------------------------------
| Array
 ------------------------------------------------------------------------- */
DefineConstant ( eshArrayTag, "+array-type-tag+", hex ( 20 ),
		 "Type tag for plob objects of type ARRAY." );

BeginEnum ( SHARRAYIDX )
  enumerator ( eshArrayIdxDataVector, "+array-location-data-vector+", 0,
	       "Index of plob array table field." )
  and
  enumerator ( eshArrayIdxFillPointer, "+array-location-fill-pointer+", 1,
	       "Index of plob array fill pointer field." )
  and
  enumerator ( eshArrayIdxDisplacedOffset,
	       "+array-location-displaced-offset+", 2,
	       "Index of plob array displaced to field." )
  and
  enumerator ( eshArrayIdxAdjustable, "+array-location-adjustable+", 3,
	       "Index of plob array adjustable field." )
  and
  enumerator ( eshArrayIdxRank, "+array-location-rank+", 4,
	       "Index of plob array dimensions field \
                (contains the rank of the array)." )
  and
  enumerator ( eshArrayIdxFirstDimension, "+array-location-first-dimension+", 5,
	       "Index of first element of the plob array dimension-table." )
  and
  enumerator ( eshArrayObjIdSize, "+array-size+", 5,
	       "Fix size of plob array in words." )
EndEnum ( SHARRAYIDX );

/* -------------------------------------------------------------------------
| String
 ------------------------------------------------------------------------- */

DefineConstant ( eshStringTag, "+string-type-tag+", hex ( 28 ),
		 "Type tag for plob objects of type STRING." );

BeginEnum ( SHSTRINGIDX )
  enumerator ( eshStringIdxLength, "+string-location-length+", 0,
	       "Index of plob string length field." )
  and
  enumerator ( eshStringObjIdSize, "+string-size+", 1,
	       "Fix size of plob string in words." )
EndEnum ( SHSTRINGIDX );

DefineFunction ( SHORTOBJID,
		 fnShortMakeString, "c-sh-make-string",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( CONST_STRING, vector_in, lpszFrom ) ) );

/* -------------------------------------------------------------------------
| Bit-vector
 ------------------------------------------------------------------------- */

DefineConstant ( eshBitVectorTag, "+bit-vector-type-tag+", hex ( 30 ),
		 "Type tag for plob objects of type BIT-VECTOR." );

BeginEnum ( SHBITVECTORIDX )
  enumerator ( eshBitVectorIdxLength, "+bit-vector-location-length+", 0,
	       "Index of plob bit-vector length field." )
  and
  enumerator ( eshBitVectorObjIdSize, "+bit-vector-size+", 1,
	       "Fix size of plob bit-vector in words." )
EndEnum ( SHBITVECTORIDX );

#if defined(C2C)

void		fnInitCommonSequModule		( void );
void		fnInitializeSequModule		( void );
void		fnDeinitializeSequModule	( void );
void		fnDeinitCommonSequModule	( void );

/* -------------------------------------------------------------------------
| Cons
 ------------------------------------------------------------------------- */

#define	consp( oSelf )		(typetagof(oSelf)==eshConsTag)
#define	car( oObjId )				\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshConsIdxCar),eshConsTag)
#define	cdr( oObjId )				\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshConsIdxCdr),eshConsTag)

/* -------------------------------------------------------------------------
| Vector
 ------------------------------------------------------------------------- */

#define	vectorp( oSelf )	(typetagof(oSelf)==eshVectorTag)
#define	vector_svref( oObjId, nIndex )			\
_SVREF_OBJID(oObjId,Cooked2RawIndex(nIndex),eshVectorTag)
#define	vector_length( oObjId )				\
(shvector_objids(oObjId)+ 				\
 eshSHvectorIdxFirstObjId-eshSHvectorIdxFirstData-eshVectorObjIdSize)
#define	make_vector( nElements )			\
fnMakeVector ( nElements )

/* -------------------------------------------------------------------------
| IVector
 ------------------------------------------------------------------------- */

#define	ivectorp( oSelf )	(typetagof(oSelf)==eshIVectorTag)
#define	make_ivector( nTypeTag, nElements )	\
fnMakeIVector ( nTypeTag, nElements )

/* -------------------------------------------------------------------------
| Array
 ------------------------------------------------------------------------- */

#define	array_data_vector( oObjId )					\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshArrayIdxDataVector),eshArrayTag)

#define	array_fill_pointer( oObjId )					\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshArrayIdxFillPointer),eshArrayTag)

#define	array_displaced_offset( oObjId )				\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshArrayIdxDisplacedOffset),eshArrayTag)

#define	array_adjustable( oObjId )					\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshArrayIdxAdjustable),eshArrayTag)

#define	array_rank( oObjId )						\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshArrayIdxRank),eshArrayTag)

#define	array_dimension( oObjId, nIndex )				\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshArrayIdxFirstDimension+(nIndex)),\
	     eshArrayTag)

/* -------------------------------------------------------------------------
| String
 ------------------------------------------------------------------------- */

#define	stringp( oSelf )	(typetagof(oSelf)==eshStringTag)

#define	string_length( oObjId )			\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshStringIdxLength),eshStringTag )

/* More string accessors are defined in cplobsequ.h and splobsequ.h */
#define	make_string( lpszFrom )			\
fnMakeString(lpszFrom)

/* Structure representing a PLOB object with type tag eshStringTag: */
typedef struct {
  PLOBHEADER	Header;
  OBJID		oLength;
  char		szString [ 4 ];
}	PLOBSTRING, * LPPLOBSTRING;

/* -------------------------------------------------------------------------
| Bit-vector
 ------------------------------------------------------------------------- */

#define	bit_vector_length( oObjId )					\
_SVREF_OBJID(oObjId,Cooked2RawIndex(eshBitVectorIdxLength),eshBitVectorTag )
/* More bit-vector accessors are defined in cplobsequ.h and splobsequ.h */

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMakeString		( LPCSTR lpszFrom );
OBJID DLLEXPORT	fnMakeVector		( int nElements );
OBJID DLLEXPORT	fnMakeIVector		( SHTYPETAG nTypeTag,
					  int nElements );

#endif

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
