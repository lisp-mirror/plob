/* ----------------------------------------------------------------------------
| Module	lwobject.h
| Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
|		kirschke@informatik.uni-hamburg.de
| Date		17.09.92	start
|		02.11.93	heavily rewritten & extended
| Description	Interface to low-level memory representation of LispWorks
|		Common LISP data types.
 --------------------------------------------------------------------------- */

/* ----------------------------------------------------------------------------

   A LispWorks object is an object fitting into exactly one memory word.
   Immediate objects type tags are encoded as follows:

   Bit indices: 31                             0
   Bit values:  ------------------------------00

   Bit 2..31 contain a 30 bit integer. This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *fixnum-poi-tag*, value 0
   *b-fixnum-poi-tag*, value 0
   *minus-fixnum-poi-tag*, value 0 (-0 would be correct :-)

   Bit indices: 31                             0
   Bit values:  ------------------------------01

   Bit 2..31 contain a pointer to a CONS cell, bit 0..1 have to be set to 0
   for dereferencing the CONS cell. This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *cons-poi-tag*, value 1
   *b-cons-poi-tag*, value 1
   *minus-cons-poi-tag*, value 1

   Bit indices: 31                             0
   Bit values:  ------------------------------11

   Bit 2..31 contain a pointer to a Non-immediate object, bit 0..1 have to be
   set to 0 for dereferencing the non-immediate object. The first dereferenced
   memory word is a header word containing some tag bits; see below at the
   description of the struct type LWCLASS1STRUCT, struct slot wHeader.
   This information was derived from following constants found in the
   LispWorks package CONSTANTS:

   *poi-tag*, value 3

   *other-poi-tag*, value 3
   *b-other-poi-tag*, value 3
   *minus-other-poi-tag*, value -3

   *othernum-poi-tag*, value 3
   *b-othernum-poi-tag*, value 3
   *minus-othernum-poi-tag*, value -3

   Bit indices: 31                             0
   Bit values:  -----------------------------010

   Bit 3..31 contain a pointer to a symbol representation or a
   funcallable lisp data object (i.e. a function). Bit 0..2 have to be set
   to 0 for dereferencing the symbol representation. A funcallable data
   object is identified by a NULL symbol name (see below at the description
   of the struct type LWCLASS1STRUCT, struct slot Symbol. This information
   was derived from following constants found in the LispWorks
   package CONSTANTS:

   *callable-poi-tag*, value 2
   *b-callable-poi-tag*, value 2
   *minus-callable-poi-tag*, value -2

   *symbol-poi-tag*, value 2
   *b-symbol-poi-tag*, value 2
   *minus-symbol-poi-tag*, value -2

   Bit indices: 31                             0
   Bit values:  ------------------------00000110

   Bit 8..15 contain a character, bit 16..20 hold the font information (i.e.
   number of character font) and bit 21..24 contain the control-meta-super-
   hyper bit mask. This information was derived from following constants
   found in the LispWorks package CONSTANTS:

   *character-poi-tag*, value 6
   *b-character-poi-tag*, value 6
   *minus-character-poi-tag*, value -6

   *char-font-mask*, value #x1f0000

   *char-control-bit-mask*, value #x200000
   *char-meta-bit-mask*, value #x400000
   *char-super-bit-mask*, value #x800000
   *char-hyper-bit-mask*, value #x1000000
   *char-bits-mask*, value #x1e00000
 
   Bit indices: 31                             0
   Bit values:  ------------------------00010110

   Object is a catch marker. This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *catch-marker*, value #x16 = #*00010110

   Bit indices: 31                             0
   Bit values:  ------------------------00100110

   Object is a special marker. This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *special-marker*, value #x26 = #*00100110

   Bit indices: 31                             0
   Bit values:  ------------------------00110110

   Object is a unbound marker. This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *unbound-marker*, value #x36 = #*00110110

   Bit indices: 31                             0
   Bit values:  ------------------------01000110

   Object is a slot unbound marker. This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *slot-unbound-marker*, value #x46 = #*01000110

   Bit indices: 31                             0
   Bit values:  ------------------------01100110

   Object is a bad pointer marker. This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *bad-pointer-marker*, value #x66 = #* 01100110

   Bit indices: 31                             0
   Bit values:  ------------------------01110110

   Object is a &rest marker (I guess it is pushed on stack as an extra
   argument preceeding the &rest list; so if at runtime a &rest-marker
   pops off the stack, the system knows that the next element on stack is
   a &rest argument list). This information was derived from
   following constants found in the LispWorks package CONSTANTS:

   *&rest-marker*, value #x76 = #*01110110

   Bit indices: 31                             0
   Bit values:  ----------------------------1110

   Bit 4..31 contain a 28 bit short float. This information was derived
   from following constants found in the LispWorks package CONSTANTS:

   *sfloat-poi-tag*, value 14 = #x e
   *b-sfloat-poi-tag*, value 14 = #x e
   *minus-sfloat-poi-tag*, value -14 = -#xe

 --------------------------------------------------------------------------- */

/* Type for LispWorks objects: */
typedef int	LWOBJECT, FAR * LPLWOBJECT;

/* Type describing the class of an LWOBJECT instance. Changes done here at
   type LWCLASS imply also changes in lwobject.c.
   NOTE: There are still missing some classes: end-marker, frame-marker,
   stack-header marker */
typedef enum {
  lwUnknown	= -1,
  /* Immediate objects, i.e. values. They fit into exact one memory
     word. Decoding is done with the low order bits: */
  lwInt		= 0,	/* 30 bit integer */
  lwCons,		/* Pointer to cons */
  lwSymbol,		/* Pointer to symbol */
  lwClass1,		/* Pointer to an Fist Class object */
  lwChar,		/* Character */
  lwMarkCatch,		/* Catch-marker */
  lwMarkSpecial,	/* Special-marker */
  lwMarkUnbound,	/* Unbound-marker */
  lwMarkSlotUnbound,	/* Slot-unbound-marker */
  lwMarkBadPointer,	/* Bad-pointer-marker */
  lwMarkRest,		/* &rest-marker */
  lwShortFloat,		/* 28 bit short float */
  /* Non-immediate objects: */
  lwTLatter,		/* TLatter */
  lwArray,		/* Array */
  lwString,		/* String */
  lwBitVector,		/* Bit vector */
  lwStruct,		/* Defstruct instance */
  lwVector,		/* Vector */
  lwClosObject,		/* CLOS object */
  lwSingleFloat,	/* Single float */
  lwLongFloat,		/* Long float */
  lwLongInt,		/* Long integer */
  lwRatio,		/* Ratio */
  lwComplex,		/* Complex */
  lwMaximum		/* Label to mark the maximum element of LWCLASS */
}	LWCLASS, FAR * LPLWCLASS;

/* ----------------------------------------------------------------------------
| Macros for accessing immediate objects:
 --------------------------------------------------------------------------- */

/* --- LWGETCLASS ( lwObject ) == lwInt: ----------------------------------- */
#define		LWINTP( w )	(LWGETCLASS2(w)==lwInt)
/* Convert a LISP fixnum to a C int: */
#define		LWINT( w )	(ASSERT(LWINTP(w)),\
				 (int)(((int)(w))<0?\
				       -(int)(-(int)(w)>>2):\
				       (int)(w)>>2))
/* Convert a LISP fixnum to a C unsigned int: */
#define		LWWORD( w )	(ASSERT(LWINTP(w)),((unsigned int)(w)>>2))
/* Convert a C int to a LISP fixnum: */
#define		LWFIXNUM( w )	((unsigned int)(w)<<2)

/* --- LWGETCLASS ( lwObject ) == lwChar: ---------------------------------- */
#define		LWCHARP( lwO )		(LWGETCLASS2(lwO)==lwChar)
#define		LWCHAR( lwO )		(ASSERT(LWCHARP(lwO)),\
					 (char)((unsigned int)(lwO)>>8))

/* --- LWGETCLASS ( lwObject ) == lwShortFloat: ---------------------------- */
#define		LWSHORTFLOATP( lwO )	(LWGETCLASS2(lwO)==lwShortFloat)
#define		LWSHORTFLOAT( lwO )	(ASSERT(LWSHORTFLOATP(lwO)),\
					 (float)((unsigned int)(lwO)>>8))

/* ----------------------------------------------------------------------------
| Macros for accessing CONS cells:
 --------------------------------------------------------------------------- */

/* --- LWGETCLASS ( lwObject ) == lwCons: ---------------------------------- */
#define		LWCONSP( lwO )		((lwO)==lwNil||\
					 LWGETCLASS(lwO)==lwCons)

#define		LWCAR( lwO )	\
(((lwO)==lwNil)?\
 lwNil:\
 (LWCLASS1STRUCTPTR(lwO,lwCons)->IsA.Cons.lwCar))

#define		LWCDR( lwO )	\
(((lwO)==lwNil)?\
 lwNil:\
 (LWCLASS1STRUCTPTR(lwO,lwCons)->IsA.Cons.lwCdr))


/* ----------------------------------------------------------------------------
| The memory layout of a LISP non-immediate object:
 --------------------------------------------------------------------------- */
typedef struct {
  /* wHeader contains some header bits describing the LISP data object. The
     'instance data' of the LISP data object are in the memory words
     following the header word.

       Bit       3           2            1
       indices: 1098 7654 3210 9876 5432 1098 7654 3210

                1||| |-|| --|| ---| |||- ---- ---- -|-|
                ^^^^  ^   ^    ^    ^^       ^       ^
                ||||  |   |    |    ||       |       |
                ||||  |   |    |    ||       |       Bits 0..2: Tag bits
                ||||  |   |    |    ||       Bits 3..13: Type info bits
                ||||  |   |    |    |Bit 14: Special action bit
                ||||  |   |    |    Bit 15: Collectable bit
                ||||  |   |    Bits 16..20: Character font bits
                ||||  |   Bits 21..24: Garbage collection counter
                ||||  Bits 25..27: Generation counter
                |||Bit 28: Mark-and-sweep bit
                ||Bit 29: Promoted bit
                |Bit 30: Free (i.e. unused) bit
                Bit 31: Always 1 for LISP data objects (?)

     Some further information about the used bits and the constants found in
     LispWorks package CONSTANTS:

     Bit 0..2	Tag bits. Possible values:

       #*010
       Object is a marker (end-marker, frame-marker)

       #*110
       Object is a LispWorks data object

     Bit 3..13: Type information. Possible values:

       *b-full-symbol-header*, value #x1000
       Object is a symbol.

       *b-full-funcallable-header*, value #x1001
       Object is a function.

       *b-full-tlatter-header*, value #x1006
       Object is a TLATTER. This is an internal structure created by the
       LispWorks Compiler and used by the LispWorks runtime system.

       *b-full-array-header*, value #x100a
       Object is an array as constructed with a call to make-array.

       *b-full-variable-instance-header*, value #x100b
       ??? Maybe the object is a CLOS instance.

       *b-full-structure-header*, value #x100e
       Object is a LISP structure instance.

       *b-full-k-vector-header*, value #x100e
       Object is a simple vector with elements of type T.

       *b-full-g-vector-header*, value #x100f
       Object is a vector containing executable code.

       *b-full-cons-header*, value #x1012
       Object is a CONS cell.

       *b-full-fix-instance-header*, value #x1013
       Object is a CLOS instance.

       *b-full-ratio-header*, value #x101e
       Object is a rational number.

       *b-full-complex-header*, value #x101f
       Object is a complex number.

     Bit 14: Special action bit.
       *special-action-bit-num*, value 14
       *special-action-mask*, value #x4000

     Bit 15: Collectable bit
       I guess this is an information whether the object is garbage-
       collectable or not.
       *collectable-bit-num*, value 15
       *collectable-mask*, value #x8000

     Bits 16..20: Character font bits
       The character font bits are only valid for string objects. They
       contain the character font number for all characters in the string.
       *char-font-mask*, value #x1f0000

     Bits 21..24: Garbage collection counter
       Incremented by one for each garbage collection; if the garbage
       collection counter reaches a threshold, the object is promoted to
       the next generation.
       *gc-count-mask*, value #x1e00000

     Bits 25..27: Generation counter
       Generation of the object. Currently generations ranging from 0 to
       3 (inclusive) are supported.
       *gen-num-full-shift*, value 19
       *gen-number-mask*, value #xe000000
       *mark-and-gen-num-mask*, value #x1e000000

     Bit 28: Mark-and-sweep bit
       Used by the garbage collector in the mark-phase. A marked object
       is referenced by another object.
       *mark-flag-bit-num*, value 28
       *mark-flag-mask*, value #x10000000

     Bit 29: Promoted bit
       ??? Perhaps set to 1 at garbage collection in the mark-phase to
       indicate that the object should be moved to the next generation in
       the sweep-phase.
       *promoted-bit-num*, value 29
       *promoted-mask*, value #x20000000

     Bit 30: Free (i.e. unused) bit
       An unused bit.
       *free-flag-bit-num*, value 30
       *free-flag-mask*, value #x40000000

     Bit 31: ??? Always 1 for LISP data objects (?), 0 for values
     (markers etc.)
  */
  WORD		wHeader;	/* LISP data object header bits */
  union {
    /* --- LWGETCLASS2 ( lwObject ) == lwCons: ----------------------------- */
    struct {
      LWOBJECT	lwCar;
      LWOBJECT	lwCdr;
    }	Cons;
    /* --- LWGETCLASS2 ( lwObject ) == lwSymbol: --------------------------- */
    struct {
      LWOBJECT	lwCode;		/* Symbol's code */
      LWOBJECT	lwConstant;	/* Symbol's constant */
      /* The package is a LISP struct (see below); the name of the package is
	 contained as a LISP string in index 1 of the struct data table: */
      LWOBJECT	lwPackage;	/* Symbol's package */
      LWOBJECT	lwPList;	/* Symbol's property list */
      /* lwsName is either a pointer to a LISP string or a NULL value (NULL
	 in the sense of C, i.e. (char *)NULL). The NULL value means that
	 the symbol is a funcallable function (or whatsoever): */
      LWOBJECT	lwsName;	/* Symbol's name */
      LWOBJECT	lwValue;	/* Symbol's Value */
      LWOBJECT	lwDummy;
    }	Symbol;
    /* --- LWGETCLASS1 ( lwObject ) == lwTLatter: -------------------------- */
    struct {
      LWOBJECT	lwKey;			/* TLatter key */
      LWOBJECT	lwValue;		/* TLatter value */
      LWOBJECT	lwNext;			/* Next TLatter */
    }	TLatter;
    /* --- LWGETCLASS1 ( lwObject ) == lwArray: ---------------------------- */
    struct {
      /* No of words in this cell: */
      LWOBJECT	lwnLength;		/* No. of array dimensions + 4 */
      /* Arrays are a rather baroque feature of LISP; they occur in many
	 variations:
	 - An array with fill pointer has to be 1-dimensional
	 - An Array with its element typed to integers which don't fit
	   into a memory word uses as data table a bitvector
	 - A normal array with its elements typed to T uses as data table
	   a simple LISP vector
	 - A displaced array is marked with a numeric value != NIL in
	   lwnDisplacedOffset; the data table of a displaced array points
	   direct to the referenced array.
      */
      LWOBJECT	lwData;			/* Vector with array table */
      /* For an exact description of the following 3 components see
	 Steele, Common LISP, 2nd edition, chapter arrays, section array
	 creation, p. 442-444: */
      LWOBJECT	lwnFillPointer;		/* NIL or the fill-pointer */
      LWOBJECT	lwnDisplacedOffset;	/* NIL or the displaced-index-offset */
      LWOBJECT	lwAdjustable;		/* NIL or T */
      /* lwnDimension [ i ] : 0 <= i < LWINT ( lwnLength ) - 4: */
      LWOBJECT	lwnDimension [ 1 ];	/* A size for each dimension */
    }	Array;
    /* --- LWGETCLASS1 ( lwObject ) == lwString: --------------------------- */
    struct {
      LWOBJECT	lwnLength;
      char	sData [ 4 ];
    }	String;
    /* --- LWGETCLASS1 ( lwObject ) == lwBitVector: ------------------------ */
    struct {
      LWOBJECT	lwnLength;	/* Number of bits */ 
      /* bData [ i ] : 0 <= i < ( LWINT ( lwnLength ) + 7 ) / 8: */
      BYTE	bData [ 4 ];
    }	BitVector;
    /* --- LWGETCLASS1 ( lwObject ) == lwStruct: --------------------------- */
    struct {
      LWOBJECT	lwnLength;	/* Total number of object slots */
      /* LISP structs are used **very heavy** in LispWorks Common LISP.
	 It has the following type description:

	  ----------------      -----------------
	 | LISP struct    |  ->| LISP struct     |
	 |----------------| |  |-----------------|  -> defstruct name (symbol)
	 | wHeader        | |  | wHeader         | |
	 | lwnLength      | |  | lwnLength = 26  | |
	 | lwStructDesc *-+-   | lwStructDesc    | |   list with defstruct-
	 | lwData [ 0 ]   |    | lwData [ 0 ]    | |   slot-description's,
	 | ...            |    | ...             | |   1 element for
	 | lwData         |    | lwData [ 9 ]  *-+-    each slot:
	 | [lwnLength-2]  |    | ...             |   ---    ---    ---
	  ----------------     | lwData [ 11 ] *-+->|*|*+->|*|*+->|*|*+->NIL
	  a defstruct          | ...             |   +--    +--    +--
	  instance             | lwData [ 24 ]   |   |      |
                                -----------------    |      |   defstruct-slot-
			        a defstruct-type     |       -> description of
			        description          |          slot 2
				                     |
				                     |    ----------------
				                      -->| LISP struct    |
				                         |----------------|
							 |   wHeader      |
			                                 |   lwnLength=7  |
						         |   lwStructDesc |
			       name of slot 1 (symbol) <-+-* lwData[0]    |
		      index of slot 1 (30 bit integer)   |   lwData[1]    |
	          accessor of slot 1 (function symbol) <-+-* lwData[2]    |
       default initialization of slot 1 (subtype of t) <-+-* lwData[3]    |
			  type of slot 1 (subtye of t) <-+-* lwData[4]    |
		 read-only flag of slot 1 ('nil or 't) <-+-* lwData[5]    |
						          ----------------
							  defstruct-slot-
							  description of
							  slot 1

         The only exception from this scheme are defstruct-slot-description
	 instances: the defstruct-slot-description contained in the
	 defstruct-type description pointed to by lwStructDesc of
	 defstruct-slot-description instances isn't a list but a vector
	 containing the slot names starting from index 1.
      */
      LWOBJECT	lwStructDesc;	/* Structure description */
      /* lwData [ i ] : 0 <= i < LWINT ( lwnLength ) - 2 */
      LWOBJECT  lwData [ 1 ];	/* Structure slots */
    }	Struct;
    /* --- LWGETCLASS1 ( lwObject ) == lwVector: --------------------------- */
    struct {
      LWOBJECT	lwnLength;
      /* lwData [ i ] : 0 <= i < LWINT ( lwnLength ) */
      LWOBJECT	lwData [ 1 ];
    }	Vector;
    /* --- LWGETCLASS1 ( lwObject ) == lwClosObject: ---------------------- */
    struct {
      /* The CLOS class wrapper is a vector with 4 elements:
         #(<a number> <list of effective instance slots>
	   <list of effective shared slots> <class object>).
	 The <class object> is itself a CLOS object. Assumed that the class
	 object is an instance of standard class or of a subclass of
	 standard-class (which is almost always true), the class name is
	 found as a symbol at index position 1 in the instance data of the
	 <class object>: */
      LWOBJECT	lwClassWrapper;	/* The CLOS class wrapper */
      /* The instance data is a vector with its length equal to the above
         mentioned <list of effective slots>: */
      LWOBJECT	lwInstanceData;	/* The instance data vector */
    }	ClosObject;
    /* --- LWGETCLASS1 ( lwObject ) == lwSingleFloat: ---------------------- */
    struct {
      int	nUnknown;
    }	SingleFloat;
    /* --- LWGETCLASS1 ( lwObject ) == lwLongFloat: ------------------------ */
    struct {
      int	nUnknown;
    }	LongFloat;
    /* --- LWGETCLASS1 ( lwObject ) == lwLongInt: -------------------------- */
    struct {
      LWOBJECT		lwnLength;	/* Number of ints */
      /* wData [ i ] : 0 <= i < LWINT ( lwnLength ) */
      unsigned int	wData [ 1 ];
    }	LongInt;
    /* --- LWGETCLASS1 ( lwObject ) == lwRatio: ---------------------------- */
    struct {
      LWOBJECT	lwEnumerator;	/* Instance of a numeric type */
      LWOBJECT	lwDenominator;	/* Instance of a numeric type */
    }	Ratio;
    /* --- LWGETCLASS1 ( lwObject ) == lwComplex: -------------------------- */
    struct {
      LWOBJECT	lwReal;		/* Instance of a numeric type */
      LWOBJECT	lwImaginary;	/* Instance of a numeric type */
    }	Complex;
  }		IsA;
}	LWCLASS1STRUCT, FAR * LPLWCLASS1STRUCT;

/* ----------------------------------------------------------------------------
| Intern macros & functions (not for direct use):
 --------------------------------------------------------------------------- */

extern LPLWCLASS	lplwClass2s;
LPLWCLASS	fnLWinitClass2s		( void );

#define		LWGETCLASS2( lwO )	(((lplwClass2s)?\
					  (LPLWCLASS)NULL:\
					  fnLWinitClass2s()),\
					 lplwClass2s[((unsigned int)(lwO))&\
						     0xFF])
LWCLASS		fnLWgetClass2		( LWOBJECT lwObject );

#define		LWCLASS1STRUCTPTRwoASSERT( lwObject )	\
((LPLWCLASS1STRUCT)((unsigned int)(lwObject)&~(unsigned int)3))

extern LPLWCLASS	lplwClass1s;
LPLWCLASS	fnLWinitClass1s		( void );

#define		LWGETCLASS1( lwO )	\
(((lplwClass1s)?(LPLWCLASS)NULL:fnLWinitClass1s()),\
 lplwClass1s[(unsigned int)(LWCLASS1STRUCTPTRwoASSERT(lwO)->\
			    wHeader)&0xFF])
LWCLASS		fnLWgetClass2		( LWOBJECT lwObject );

/* ----------------------------------------------------------------------------
| Extern macros & functions
 --------------------------------------------------------------------------- */

#define		ASSERTCLASS( lwObject, ExpectedClass )	\
(((ExpectedClass)==lwUnknown)?\
 TRUE:ASSERT(LWGETCLASS(lwObject)==(ExpectedClass)))

#define		LWCLASS1STRUCTPTR( lwObject, ExpectedClass )	\
((LPLWCLASS1STRUCT)(ASSERTCLASS(lwObject,ExpectedClass),\
		    LWCLASS1STRUCTPTRwoASSERT(lwObject)))

/* -------------------------------------------------------------------------
| Macro		LWGETCLASS
| Function	fnLWgetClass
| Arguments	  lwObject
|		The object whose class should be retrieved
| Return	The class of a LispWorks data object.
| Description	Retrieves the class of a LispWorks object.
 ------------------------------------------------------------------------- */
#define		LWGETCLASS( lwO )	((LWGETCLASS2(lwO)==lwClass1)?\
					 LWGETCLASS1(lwO):LWGETCLASS2(lwO))
LWCLASS		fnLWgetClass		( LWOBJECT lwObject );

/* -------------------------------------------------------------------------
| Function	fnLWclassToString
| Arguments	  lwClass
|		The type tag to convert into a string
|		  lpszBuf, nBuf
|		Pointer to string and size of pointed to memory which is
|		filled with the result string. If lpszBuf == NULL, a static
|		string buffer is filled and returned. nBuf should be >= 32.
| Return	( lpszBuf == NULL ) ? Address of static buffer : lpszBuf
| Description	Retrieves a string description for lwClass.
 ------------------------------------------------------------------------- */
LPSTR		fnLWclassToString	( LWCLASS lwClass, LPSTR lpszBuf,
					  size_t nBuf );

/* -------------------------------------------------------------------------
| Function	fnLWregisterConst
| Arguments	  lwObjectConst
|		The constant object to register
|		  constObj
|		A tag indicating which object is registered
| Description	Register constant LISP objects; must be called for module
|		initialisation. The only real needed registered constant
|		object is NIL to detect end of lists etc.
 ------------------------------------------------------------------------- */
typedef enum {
  lwNilConst	= 0,	/* Register NIL */
  lwTconst	= 1	/* Register T */
}	LWCONSTOBJ, FAR * LPLWCONSTOBJ;
extern LWOBJECT	lwNil;
extern LWOBJECT	lwT;
void		fnLWregisterConst	( LWOBJECT lwObjectConst,
					  LWCONSTOBJ constObj );

/* -------------------------------------------------------------------------
| Function	fnLWfput, fnLWput
| Arguments	  lwObject
|		The LispWorks LISP data object to print
|		  lpStream
|		The stream to which the object will be printed; it is
|		stdout for fnLWput
| Return	Number of printed objects
| Description	Print a LispWorks LISP data object. Circular references
|		are printed with the #n=, #n#-notation described in
|		Steele, Common LISP, 2nd edition, chapter input/output,
|		section standard dispatching macro character syntax,
|		p. 537.
 ------------------------------------------------------------------------- */
int		fnLWfput		( LWOBJECT lwObject,
					  FILE FAR * lpStream );
int		fnLWput			( LWOBJECT lwObject );
