/* -------------------------------------------------------------------------
| Module	plobmisc.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94
| Description	Foreign language interface to miscellaneous functions.
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
;;;; For further comments look into file plob.h
;;;; -------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#if defined(LISP)

DefineConstant ( FALSE, "+c-false+", 0, "not-T for C." );
DefineConstant ( TRUE, "+c-true+", 1, "T for C." );

#endif

#if defined(C2C) || defined(LISP)

/*
DefineFunction ( psint,
		 fnClientDbRestartClockGet, "c-sh-get-restart-clock",
		 ( voidArgument ) );
DefineFunction ( voidResult,
		 fnClientDbRestartClockSet, "c-sh-set-restart-clock",
		 ( argument ( psint, value_in, nTime ) ) );
*/

#if defined(LISP)
DefineFunction ( FIXNUM,
		 fnShiftLeftAndSet, "c-short-float-to-fixnum",
		 ( argument ( VECTOR ( as_is, 1 ), vector_in, wObject )
		   and
		   argument ( FIXNUM, value_in, nShiftLeft )
		   and
		   argument ( FIXNUM, value_in, wOrMask ) ) );

DefineFunction ( VECTOR ( as_is, 1 ),
		 fnShiftLeftAndSet, "c-fixnum-to-short-float",
		 ( argument ( FIXNUM, value_in, wObject )
		   and
		   argument ( FIXNUM, value_in, nShiftLeft )
		   and
		   argument ( FIXNUM, value_in, wOrMask ) ) );
#else
DefineFunction ( FIXNUM,
		 fnShiftLeftAndSet, "c-short-float-to-fixnum",
		 ( argument ( FIXNUM, value_in, wObject )
		   and
		   argument ( FIXNUM, value_in, nShiftLeft )
		   and
		   argument ( FIXNUM, value_in, wOrMask ) ) );
#endif

#if defined(LISPWORKS4)
DefineFunction ( POINTER,
		 fnUnmaskAndAdd, "c-lisp-object-to-pointer",
		 ( argument ( AS_IS, value_in, wObject )
		   and
		   argument ( FIXNUM, value_in, wUnmask )
		   and
		   argument ( FIXNUM, value_in, nAdd ) ) );
#endif

#endif

#include	"plobconst.h"

DefineConstant	( MAX_URL, "+max-url+", 256,
		  "Maximum length of an URL." );

BeginEnum ( DAYLIGHTSAVINGTIME )
  enumerator ( eDstNone, "+c-dst-none+", 0, "not on dst" )
  and
  enumerator ( eDstUsa, "+c-dst-usa+", 1, "USA style dst" )
  and
  enumerator ( eDstAust, "+c-dst-aust+", 2, "Australian style dst" )
  and
  enumerator ( eDstWet, "+c-dst-wet+", 3, "Western European dst" )
  and
  enumerator ( eDstMet, "+c-dst-met+", 4, "Middle European dst" )
  and
  enumerator ( eDstEet, "+c-dst-Eet+", 5, "Eastern European dst" )
  and
  enumerator ( eDstCan, "+c-dst-can+", 6, "Canada" )
  and
  enumerator ( eDstGb, "+c-dst-gb+", 7, "Great Britain and Eire" )
  and
  enumerator ( eDstRum, "+c-dst-rum+", 8, "Rumania" )
  and
  enumerator ( eDstTur, "+c-dst-tur+", 9, "Turkey" )
  and
  enumerator ( eDstAustAlt, "+c-dst-aust-alt+", 10,
               "Australian style with shift in 1986" )
EndEnum ( DAYLIGHTSAVINGTIME );

BeginEnum ( DEPENDENTMODE )
  enumerator ( flagDependentGet, "+flag-dependent-get+",
	       FLAGDEPENDENTGET,
	       "Get the current dependent flag." )
  and
  enumerator ( flagDependentNone, "+flag-dependent-none+",
	       FLAGDEPENDENTNONE,
	       "Unset the dependent flag." )
  and
  enumerator ( flagDependentRead, "+flag-dependent-read+",
	       FLAGDEPENDENTREAD,
	       "Set the dependent flag to read." )
  and
  enumerator ( flagDependentWrite, "+flag-dependent-write+",
	       FLAGDEPENDENTWRITE,
	       "Set the dependent flag to read." )
  and
  enumerator ( flagDependentReadWrite, "+flag-dependent-read-write+",
	       FLAGDEPENDENTREADWRITE,
	       "Set the dependent flag to read-write." )
EndEnum ( DEPENDENTMODE );

DefineFunction ( DEPENDENTMODE,
		 fnShortMakeDependent, "c-sh-dependent",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortSelf )
		   and
		   argument ( DEPENDENTMODE, value_in, nDependentMode ) ) );

#if ! defined(LISP)	/* server: */

DefineFunction ( SHLOCK,
		 fnServerObjectPeekSlots, "c-sh-peek-object-slots",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FIXNUM, value_in, hPeek )
		   and
		   argument ( FIXNUM, value_in, nWords )
		   and
		   argument ( VECTOR ( u_int, nWords ),
			      vector_out, pBuffer ) ) );

DefineFunction ( SHLOCK,
		 fnServerObjectPeekValues, "c-sh-peek-object-values",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FIXNUM, value_in, hPeek )
		   and
		   argument ( FIXNUM, value_in, nObjects )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_in, pnElementTypeTagsIn )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_in, pnSizesInElementsIn )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_out, pnElementTypeTagsOut )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_out, pnSizesInElementsOut )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue
				       ( nObjects, pnElementTypeTagsIn,
					 pnSizesInElementsIn ) ),
			      vector_out, pBuffer ) ) );

#if ! defined(RPC)	/* client: */

DefineFunction ( SHLOCK,
		 fnClientObjectPeekSlots, "c-sh-peek-object-slots",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FIXNUM, value_in, hPeek )
		   and
		   argument ( FIXNUM, value_in, nWords )
		   and
		   argument ( VECTOR ( u_int, nWords ),
			      vector_out, pBuffer ) ) );

DefineFunction ( SHLOCK,
		 fnClientObjectPeekValues, "c-sh-peek-object-values",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FIXNUM, value_in, hPeek )
		   and
		   argument ( FIXNUM, value_in, nObjects )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_in, pnElementTypeTags )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_in, pnSizesInElements )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue
				       ( nObjects, pnElementTypeTags,
					 pnSizesInElements ) ),
			      vector_out, pBuffer ) ) );

#endif	/* ! RPC */
#endif	/* ! LISP */

#if ! defined(LISP)	/* server: */
DefineFunction ( voidResult,
		 fnServerObjectPoke, "c-sh-poke-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( FIXNUM, value_in, nSlots )
		   and
		   argument ( VECTOR ( u_int, nSlots ), vector_in, pSlots )
		   and
		   argument ( SHTYPETAG, value_in, nElementTypeTag )
		   and
		   argument ( FIXNUM, value_in, nSizeInElements )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue(1,&nElementTypeTag,
							  &nSizeInElements ) ),
			      vector_in, pValues ) ) );
#if ! defined(RPC)	/* client: */
DefineFunction ( voidResult,
		 fnClientObjectPoke, "c-sh-poke-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( FIXNUM, value_in, nSlots )
		   and
		   argument ( VECTOR ( u_int, nSlots ), vector_in, pSlots )
		   and
		   argument ( SHTYPETAG, value_in, nElementTypeTag )
		   and
		   argument ( FIXNUM, value_in, nSizeInElements )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue(1,&nElementTypeTag,
							  &nSizeInElements ) ),
			      vector_in, pValues ) ) );
#endif	/* ! RPC */
#endif	/* ! LISP */

BeginEnum ( NUMERICSTDSTREAM )
  enumerator ( eshStdIn, "+c-numeric-stdin+", 0,
	       "Tag for PLOB C stream stdin." )
  and
  enumerator ( eshStdOut, "+c-numeric-stdout+", 1,
	       "Tag for PLOB C stream stdin." )
  and
  enumerator ( eshStdErr, "+c-numeric-stderr+", 2,
	       "Tag for PLOB C stream stderr." )
EndEnum ( NUMERICSTDSTREAM );

#define		FLUSH_NEVER				 0
#define		FLUSH_SELDOM				 1
#define		FLUSH_SOMETIMES				 2
#define		FLUSH_OFTEN				 3
#define		FLUSH_ALWAYS				 4

BeginEnum ( FLUSHMODE )
  enumerator ( flushGet, "+flush-get-mode+",		-1,
	       "Don't change current flush mode; just return current mode." )
  and
  enumerator ( flushNever, "+flush-never+",		FLUSH_NEVER,
	       "Flush never (mostly unsecure, but fastest possible mode)." )
  and
  enumerator ( flushSeldom, "+flush-seldom+", 		FLUSH_SELDOM,
	       "Flush seldom." )
  and
  enumerator ( flushSometimes, "+flush-sometimes+", 	FLUSH_SOMETIMES,
	       "Flush sometimes." )
  and
  enumerator ( flushOften, "+flush-often+",		FLUSH_OFTEN,
	       "Flush often." )
  and
  enumerator ( flushAlways, "+flush-always+",		FLUSH_ALWAYS,
	       "Flush always (mostly secure, but slowest possible mode)." )
  and
  enumerator ( flushDefault, "+flush-default-mode+",	FLUSH_NEVER,
	       "The default flush mode." )
EndEnum ( FLUSHMODE );

DefineFunction ( FLUSHMODE,
		 fnFlushMode, "c-sh-flush-mode",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FLUSHMODE, value_in, nMode ) ) );

BeginEnum ( FLAGMODE )
  enumerator ( flagGet, "+flag-get+",			-1,
	       "Don't change current flag; just return current value." )
  and
  enumerator ( flagSet, "+flag-set+",			 0,
	       "Set flag value" )
EndEnum ( FLAGMODE );

DefineFunction ( FIXNUM,
		 fnFlagWord, "c-sh-flag-word",
		 ( argument ( FLAGMODE, value_in, nGetOrSet )
		   and
		   argument ( FIXNUM, value_in, nFlagWord ) ) );

#if ! defined(RPC)	/* client: */

DefineConstant ( nMatchAny, "+match-any+", -1,
		 "Match-any value for the component of a host address." );
DefineFunction ( BOOL,
		 fnGetHostAddr, "c-sh-get-host-addr",
		 ( argument ( CONST_STRING, vector_in, szHost )
		   and
		   argument ( VECTOR ( int, 4 ), vector_out, pnAddr ) ) );

DefineFunction ( POINTER,
		 fnLISPmalloc, "c-malloc",
		 ( argument ( FIXNUM, value_in, nSizeInBytes ) ) );

DefineFunction ( voidResult,
		 fnLISPfree, "c-free",
		 ( argument ( POINTER, value_in, pMemory ) ) );

#endif	/* ! RPC */

#if defined(C2C)

void		fnInitCommonMiscModule		( void );
void		fnInitializeMiscModule		( void );
void		fnDeinitializeMiscModule	( void );
void		fnDeinitCommonMiscModule	( void );

/* -------------------------------------------------------------------------
| PEEKSLOTS
 ------------------------------------------------------------------------- */
/* The struct PEEKSLOTS is used for transferring a persistent object
   state with all its dependent objects from the server to the client
   with one single call to fnServerObjectPeek. The aim is to reduce the
   data traffic inherent to the RPC calls by transferring more than
   one object state in one call to fnServerObjectPeek.  One instance of
   PEEKSLOTS holds the state of one persistent object; fnServerObjectPeek
   writes the states of the peeked object and its dependent objects
   one after the other to the output buffers: */
typedef struct {
  /* The long objid of the object represented by this struct: */
  u_int	oSelf;
  /* The type tag of oSelf: */
  u_int	nTypeTag;
  /* The number of objids and type tags in Buffer: */
  u_int	nSlots;
  /* The type tag of the object's value elements: */
  u_int	nTypeTagValues;
  /* The number of the object's value elements: */
  u_int	nValues;
  /* The slot Buffer holds the object's state. 2*nSlots words hold on
     index 2*i+0 the objid contained in slot i, on index 2*i+1 the
     type tag of slot i: */
  u_int	Buffer [ 4 ];
}	PEEKSLOTS, * PPEEKSLOTS;

/* -------------------------------------------------------------------------
| Some accessor macros for comfortable(?) access to a PEEKSLOTS:
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Slot oSelf:
 ------------------------------------------------------------------------- */
#define	PPEEKSLOTSSELF( /* PPEEKSLOTS */ pPeek ) \
(OBJID *) ( &((PPEEKSLOTS)(pPeek))->oSelf )

#define	PEEKSLOTSSELF( /* PPEEKSLOTS */ pPeek ) \
* PPEEKSLOTSSELF ( pPeek )

#define	PeekSlotsSelf( /* PPEEKSLOTS */ pPeek ) \
* ( ASSERT ( (pPeek) != NULL ), PPEEKSLOTSSELF ( pPeek ) )

/* -------------------------------------------------------------------------
| Slot nTypeTag:
 ------------------------------------------------------------------------- */
#define	PPEEKSLOTSTYPETAG( /* PPEEKSLOTS */ pPeek ) \
(SHTYPETAG *) ( &((PPEEKSLOTS)(pPeek))->nTypeTag )

#define	PEEKSLOTSTYPETAG( /* PPEEKSLOTS */ pPeek ) \
* PPEEKSLOTSTYPETAG ( pPeek )

#define	PeekSlotsTypeTag( /* PPEEKSLOTS */ pPeek ) \
* ( ASSERT ( (pPeek) != NULL ), PPEEKSLOTSTYPETAG ( pPeek ) )

/* -------------------------------------------------------------------------
| Slot nSlots:
 ------------------------------------------------------------------------- */
#define	PPEEKSLOTSSLOTS( /* PPEEKSLOTS */ pPeek ) \
(u_int *) ( &((PPEEKSLOTS)(pPeek))->nSlots )

#define	PEEKSLOTSSLOTS( /* PPEEKSLOTS */ pPeek ) \
* PPEEKSLOTSSLOTS ( pPeek )

#define	PeekSlotsSlots( /* PPEEKSLOTS */ pPeek ) \
* ( ASSERT ( (pPeek) != NULL ), PPEEKSLOTSSLOTS ( pPeek ) )

/* -------------------------------------------------------------------------
| Slot nTypeTagValues:
 ------------------------------------------------------------------------- */
#define	PPEEKSLOTSTYPETAGVALUES( /* PPEEKSLOTS */ pPeek ) \
(SHTYPETAG *) ( &((PPEEKSLOTS)(pPeek))->nTypeTagValues )

#define	PEEKSLOTSTYPETAGVALUES( /* PPEEKSLOTS */ pPeek ) \
* PPEEKSLOTSTYPETAGVALUES ( pPeek )

#define	PeekSlotsTypeTagValues( /* PPEEKSLOTS */ pPeek ) \
* ( ASSERT ( (pPeek) != NULL ), PPEEKSLOTSTYPETAGVALUES ( pPeek ) )

/* -------------------------------------------------------------------------
| Slot nValues:
 ------------------------------------------------------------------------- */
#define	PPEEKSLOTSVALUES( /* PPEEKSLOTS */ pPeek ) \
(u_int *) ( &((PPEEKSLOTS)(pPeek))->nValues )

#define	PEEKSLOTSVALUES( /* PPEEKSLOTS */ pPeek ) \
* PPEEKSLOTSVALUES ( pPeek )

#define	PeekSlotsValues( /* PPEEKSLOTS */ pPeek ) \
* ( ASSERT ( (pPeek) != NULL ), PPEEKSLOTSVALUES ( pPeek ) )

/* -------------------------------------------------------------------------
| The objid contained in slot at index nIndex:
 ------------------------------------------------------------------------- */
#define PPEEKSLOTSSLOTOBJID( /* PPEEKSLOTS */ pPeek, /* int */ nIndex ) \
(OBJID *) ( &((PPEEKSLOTS)(pPeek))->Buffer[(nIndex)*2] )

#define PEEKSLOTSSLOTOBJID( /* PPEEKSLOTS */ pPeek, /* int */ nIndex ) \
* PPEEKSLOTSSLOTOBJID ( pPeek, nIndex )

#define	PeekSlotsSlotObjId( /* PPEEKSLOTS */ pPeek, /* int */ nIndex ) \
* ( ASSERT ( (pPeek) != NULL ), \
    ASSERT ( 0 <= (int)(nIndex) ), \
    ASSERT ( (int)(nIndex) < PEEKSLOTSSLOTS ( pPeek ) ), \
    PPEEKSLOTSSLOTOBJID ( pPeek, nIndex ) )
  
/* -------------------------------------------------------------------------
| The type tag contained in slot at index nIndex:
 ------------------------------------------------------------------------- */
#define PPEEKSLOTSSLOTTYPETAG( /* PPEEKSLOTS */ pPeek, /* int */ nIndex ) \
(SHTYPETAG *) ( &((PPEEKSLOTS)(pPeek))->Buffer[(nIndex)*2+1] )

#define PEEKSLOTSSLOTTYPETAG( /* PPEEKSLOTS */ pPeek, /* int */ nIndex ) \
* PPEEKSLOTSSLOTTYPETAG ( pPeek, nIndex )

#define	PeekSlotsSlotTypeTag( /* PPEEKSLOTS */ pPeek, /* int */ nIndex ) \
* ( ASSERT ( (pPeek) != NULL ), \
    ASSERT ( 0 <= (int)(nIndex) ), \
    ASSERT ( (int)(nIndex) < PEEKSLOTSSLOTS ( pPeek ) ), \
    PPEEKSLOTSSLOTTYPETAG ( pPeek, nIndex ) )
  
/* -------------------------------------------------------------------------
| Compute the size of a PEEKSLOTS in words:
 ------------------------------------------------------------------------- */
#define	PEEKSLOTSSizeBySlots( /* int */ nSlots ) \
(u_int) ( ( sizeof ( PEEKSLOTS ) - \
	    sizeof ( ((PPEEKSLOTS)NULL)->Buffer ) ) / \
	  nSizeOfPostoreWord + (nSlots) * 2 )

#define	PEEKSLOTSSIZE( /* PPEEKSLOTS */ pPeek ) \
PEEKSLOTSSizeBySlots ( PEEKSLOTSSLOTS ( pPeek ) )

#define	PeekSlotsSize( /* PPEEKSLOTS */ pPeek ) \
( ASSERT ( (pPeek) != NULL ), PEEKSLOTSSIZE ( pPeek ) )

/* ----------------------------------------------------------------------- */
typedef struct {
  /* The objid which was locked: */
  OBJID		oLocked;
  /* The current vector lock status of oLocked: */
  SHLOCK	nVectorLockNow;
  /* Size of objid buffer to allocate by the client; the buffer itself
     will contain persistent objects states as described at PEEKSLOTS: */
  unsigned int	nObjIdWords;
  /* Number of elements to copy out: */
  unsigned int	nValues;
  /* Number of objid buffer words and values already copied out to
     client: */
  unsigned int	nCopiedOut;
  /* A hash table containing all objids peeked so far: */
  HASHTABLE	ObjIds;
}	PEEKHANDLE, * PPEEKHANDLE;

/* For the communication between client and server on objects to be
   peeked, a HPEEK is used: */
typedef unsigned int	HPEEK, * PHPEEK;
enum {
  /* The always invalid peek handle: */
  NULLHPEEK	= (HPEEK) 0
};

/* ----------------------------------------------------------------------- */
HPEEK		fnPeekHandleCreate		( OBJID	oLocked,
						  SHLOCK nVectorLockNow );
HPEEK		fnPeekHandleDestroy		( HPEEK	hPeek );
int		fnPeekHandleDestroyAll		( void );

BOOL		fnPeekHandleInsert		( HPEEK	hPeek,
						  OBJID	oTail );
PPEEKHANDLE	fnPeekHandlePtr			( HPEEK	hPeek );

/* ----------------------------------------------------------------------- */
/* Mark oDependent as dependent. Returns the old dependent state: */
#define		makdependent( o, f )		fnMakeDependent(o,f)
DEPENDENTMODE DLLEXPORT	fnMakeDependent		( OBJID oDependent,
						  DEPENDENTMODE
						  nDependentMode );
DEPENDENTMODE DLLEXPORT	fnMakeDependentFromSymbol( OBJID	 oDependent,
						   OBJID	oSymbol );

/* Check if oDependentP is marked as being dependent: */
#define		dependentp( o )			fnDependentP(o)
DEPENDENTMODE DLLEXPORT	fnDependentP		( OBJID oDependentP );

/* ----------------------------------------------------------------------- */
extern DLLEXPORTVAR const char	szFormatURL []	/* = "%s://%s/%s" */;

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnSplitURL			( LPCSTR lpszURL,
						  LPSTR lpszHost,
						  LPSTR lpszTransport,
						  LPSTR lpszDirectory );
LPSTR DLLEXPORT	fnMergeURLs			( LPCSTR lpszURL1,
						  LPCSTR lpszURL2,
						  LPSTR lpszMergedURL,
						  int nMergedURL );

void		fnPLOBerrorCallback		( LPCSTR lpszErrorMsg );
void		fnPLOBsaveCallback		( void );
void		fnPLOBrestoreCallback		( void );
void		fnPLOBstabiliseCallback		( void );

#endif /* #if defined(C2C) */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
