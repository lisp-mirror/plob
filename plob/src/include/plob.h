/* -------------------------------------------------------------------------
| Module	plob.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		17.12.93
| Description	Foreign language interface to postore persistent heap.
|		The interface here is a low-level interface just for calling
|		the C functions provided by the persistent heap. The higher
|		and more LISP adequate interface is in package PLOB.
|		For a description of the stable store and stable heap
|		interface please look into the man pages.
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

/* Map psint to FIXNUM: */
MapType ( /* base type: */ FIXNUM, /* type to map: */ psint );

#if ! defined(RPC)
/* Map BOOL to FIXNUM: */
MapType ( /* base type: */ FIXNUM, /* type to map: */ BOOL );
#endif

#include	"plobconst.h"

/* -------------------------------------------------------------------------
| Constants & types
 ------------------------------------------------------------------------- */
/* Short (29 bit) objid's: */
DefineType ( /* base type: */ FIXNUM, /* defined type: */ SHORTOBJID );
/* Long (32 bit) objid's: */
DefineType ( /* base type: */ psint, /* defined type: */ OBJID );

DefineConstant ( NULLOBJID, "+null-objid+", 0,
		 "NULL (i.e. always invalid) objid." );

DefineConstant ( NULLTYPETAG, "+null-type-tag+", -1,
		 "NULL (i.e. always invalid) type tag." );

DefineConstant ( nBitsPerByte, "+bits-per-byte+", NBITSPERBYTE,
		 "Number of bits in a byte." );

DefineConstant ( nBitsPerBigit, "+bits-per-bigit+", 16,
		 "Number of bits in an Allegro bigit. A bigit is the atom\
 \\allegro\\ bignums are built from." );

DefineConstant ( nSizeOfPostoreWord, "+sizeof-postore-word+", 4,
		 "Size of a POSTORE memory word in bytes. Actually, this is\
 sizeof ( psint ).");

DefineConstant ( nFixnumBits, "+p-fixnum-bits+",
		 subtract ( mul ( NSIZEOFPOSTOREWORD, NBITSPERBYTE ),
			    NFIXNUMBITOFFSET ),
		 "Number of bits in a persistent fixnum." );

#if defined(C2C)

/* Mapping between 'raw' and 'cooked' indices: */
#define	Raw2CookedIndex( nRawIndex )	\
((nRawIndex)-eshSHvectorIdxFirstData)
#define	Cooked2RawIndex( nCookedIndex )	\
((nCookedIndex)+eshSHvectorIdxFirstData)

/* Access to sh-vector components: */
#define	shvector_lock( oObjId )			\
_SVREF_OBJID ( oObjId, eshSHvectorIdxLock, -1 )
#define	shvector_objids( oObjId )		\
_SVREF_OBJID ( oObjId, eshSHvectorIdxObjIds, -1 )
#define	shvector_size( oObjId )			\
_SVREF_OBJID ( oObjId, eshSHvectorIdxSize, -1 )
#define	shvector_type_tag( oObjId )		\
_SVREF_OBJID ( oObjId, eshSHvectorIdxTypeTag, -1 )

#endif

/* -------------------------------------------------------------------------
| Symbol
 ------------------------------------------------------------------------- */
BeginEnum ( SHSYMBOLIDX )
  enumerator ( eshSymbolIdxFunction, "+symbol-location-function+", 0,
	       "Index of plob symbol function." )
  and
  enumerator ( eshSymbolIdxPackage, "+symbol-location-package+", 1,
	       "Index of plob symbol package." )
  and
  enumerator ( eshSymbolIdxPList, "+symbol-location-plist+", 2,
	       "Index of plob symbol property list." )
  and
  enumerator ( eshSymbolIdxName, "+symbol-location-name+", 3,
	       "Index of plob symbol name field." )
  and
  enumerator ( eshSymbolIdxValue, "+symbol-location-value+", 4,
	       "Index of plob symbol value field." )
  and
  enumerator ( eshSymbolObjIdSize, "+symbol-size+", 5,
	       "Size of of plob symbol cell in words." )
EndEnum ( SHSYMBOLIDX );

/* -------------------------------------------------------------------------
| Function
 ------------------------------------------------------------------------- */
/* NOTE: Functions are represented in PLOB but the degree of support
   for it depends on the current level of implementation: */
BeginEnum ( SHFUNCTIONIDX )
  enumerator ( eshFunctionIdxName, "+function-location-name+", 0,
	       "Index of function name field." )
  and
  enumerator ( eshFunctionIdxLanguage, "+function-location-language+", 1,
	       "Index of function language field. :sun-o for .o object-code,\
 :wfasl for LISP .wfasl-code, :c for C source code." )
  and
  enumerator ( eshFunctionIdxCode, "+function-location-code+", 2,
	       "Index of function code field." )
  and
  enumerator ( eshFunctionSize, "+function-size+", 3,
	       "Size of of plob function in words." )
EndEnum ( SHFUNCTIONIDX );

/* -------------------------------------------------------------------------
| TLatter
| A TLatter is a kind of cons with 3 elements: a key, an associated datum
| and a reference to a next field. TLatters are used e.g. in LispWorks for
| implementing hash tables. They contain the hash key, the associated datum
| and a reference to the next TLatter.
 ------------------------------------------------------------------------- */
BeginEnum ( SHTLATTERIDX )
  enumerator ( eshTLatterIdxKey,
	       "+tlatter-location-key+", 0,
	       "Index of plob TLatter key field." )
  and
  enumerator ( eshTLatterIdxData,
	       "+tlatter-location-data+", 1,
	       "Index of plob TLatter data field." )
  and
  enumerator ( eshTLatterIdxNext,
	       "+tlatter-location-next+", 2,
	       "Index of plob TLatter next field." )
  and
  enumerator ( eshTLatterObjIdSize, "+tlatter-size+", 3,
	       "Size of plob TLatter in words." )
EndEnum ( SHTLATTERIDX );

/* -------------------------------------------------------------------------
| Type tags
 ------------------------------------------------------------------------- */

BeginEnum ( SHTYPETAG )
  /* Immediate types, encoded in lower three bits of a long (POSTORE)
     objid: */
  enumerator ( eshObjIdTag, "+objid-type-tag+",			hex ( 00 ),
	       "Bit 3..31 is a stable heap reference to another object." )
  and
  enumerator ( eshFixnumTag, "+fixnum-type-tag+",		hex ( 01 ),
	       "Bit 2..31 hold a 30 bit fixnum; also fixnum for tag 5." )
  and
  enumerator ( eshShortFloatTag, "+short-float-type-tag+",	hex ( 02 ),
	       "Bit 3..31 hold a short float." )
  and
  enumerator ( eshCharacterTag,	"+character-type-tag+",		hex ( 03 ),
	       "Bit 3..11 hold a 8 bit character." )
  and
  enumerator ( eshMarkerTag, "+marker-type-tag+",		ESHMARKERTAG,
	       "Object is a marker." )
  and
  enumerator ( eshUnboundTag, "+unbound-type-tag+",
	       bitwise_or ( hex ( 0100 ), ESHMARKERTAG ),
	       "Object is a unbound marker." )
  and
  enumerator ( eshUnstorableTag, "+unstorable-object-marker+",
	       bitwise_or ( hex ( 0200 ), ESHMARKERTAG ),
	       "A marker which represents unstorable objects." )
  and
  enumerator ( eshEndTag, "+end-tag+",
	       bitwise_or ( hex ( 0300 ), ESHMARKERTAG ),
	       "End-marker." )
  and
  enumerator ( eshMinTag, "+min-tag+",
	       bitwise_or ( hex ( 0400 ), ESHMARKERTAG ),
	       "Marker meaning minimum possible value." )
  and
  enumerator ( eshMaxTag, "+max-tag+",
	       bitwise_or ( hex ( 0500 ), ESHMARKERTAG ),
	       "Marker meaning maximum possible value." )
  and
  enumerator ( eshIgnoreSlotTag, "+ignore-slot-tag+",
	       bitwise_or ( hex ( 0600 ), ESHMARKERTAG ),
	       "Marker meaning to ignore a slot." )
  and
  enumerator ( eshAllowTag, "+allow-tag+",
	       bitwise_or ( hex ( 0700 ), ESHMARKERTAG ),
	       "Marker meaning :ALLOW." )
  and
  enumerator ( eshDenyTag, "+deny-tag+",
	       bitwise_or ( hex ( 0800 ), ESHMARKERTAG ),
	       "Marker meaning :DENY." )
  and
  enumerator ( eshMatchAnyTag, "+match-any-tag+",
	       bitwise_or ( hex ( 0900 ), ESHMARKERTAG ),
	       "Marker meaning match-any. A comparision with this marker\
 always means that both objects are equal." )
  and
  enumerator ( eshMatchNeverTag, "+match-never-tag+",
	       bitwise_or ( hex ( 0A00 ), ESHMARKERTAG ),
	       "Marker meaning match-never. A comparision with this marker\
 always means that both objects are not equal." )
  and
  enumerator ( eshBitmaskTag,
	       "+bitmask-type-tag+",				hex ( 06 ),
	       "Object is a 24 bit immediate bitmask." )
  and
  enumerator ( eshBuiltInTag,
	       "+built-in-type-tag+",				hex ( 07 ),
	       "Object is a built in class." )
  and
  /* Non-immediate types, encoded in the lower 8 bits of oTypeTag in
     PLOB object header, i.e. component oTypeTag in struct PLOBHEADER.
     The lower 3 bits of these type tags must be 0: */
  enumerator ( eshSymbolTag, "+symbol-type-tag+", 		hex ( 10 ),
	       "Type tag for plob objects of type SYMBOL." )
  and
  enumerator ( eshFunctionTag, "+function-type-tag+", 		hex ( 18 ),
	       "Type tag for plob objects of type ARRAY." )
  and
  enumerator ( eshSingleFloatTag, "+single-float-type-tag+", 	hex ( 50 ),
	       "Type tag for plob objects of type SINGLE-FLOAT." )
  and
  enumerator ( eshDoubleFloatTag, "+double-float-type-tag+", 	hex ( 58 ),
	       "Type tag for plob objects of type LONG-FLOAT." )
  and
  enumerator ( eshBignumTag, "+bignum-type-tag+", 		hex ( 60 ),
	       "Type tag for plob objects of type BIGNUM\
 (integers bigger than fixnums)." )
  and
  enumerator ( eshRatioTag, "+ratio-type-tag+", 		hex ( 68 ),
	       "Type tag for plob objects of type RATIO." )
  and
  enumerator ( eshComplexTag, "+complex-type-tag+", 		hex ( 70 ),
	       "Type tag for plob objects of type COMPLEX." )
  and
  enumerator ( eshTLatterTag, "+tlatter-type-tag+", 		hex ( B8 ),
	       "Type tag for plob objects of type TLATTER." )
  and
  /* A special tag for short objids referencing a persistent object;
     used only for passing references to PLOB objects between the LISP
     system and PLOB: */
  enumerator ( eshShortObjIdTag, "+short-objid-tag+",		hex ( 78 ),
	       "Type tag for plob objects referenced by a short objid." )
  and
  enumerator ( eshDynCStringPtrTag, "+dynamic-cstring-ptr-tag+", hex ( 80 ),
	       "Type tag for pointers to transient 0-terminated C strings\
 with dynamic and transient extent." )
  and
  enumerator ( eshDynCFloatPtrTag, "+dynamic-cfloat-ptr-tag+", hex ( C8 ),
	       "Type tag for pointers to transient C floats\
 with dynamic and transient extent." )
  and
  enumerator ( eshDynCDoublePtrTag, "+dynamic-cdouble-ptr-tag+", hex ( E0 ),
	       "Type tag for pointers to transient C double floats\
 with dynamic and transient extent." )
  and
  enumerator ( eshDynCQuadruplePtrTag, "+dynamic-cquadruple-ptr-tag+",
	       hex ( E8 ), "Type tag for pointers to transient C long\
 double floats with dynamic and transient extent." )
EndEnum ( SHTYPETAG );

/* -------------------------------------------------------------------------
| Error levels
| These error levels are returned from the server to the client to
| indicate error conditions. Additional values have to be added in
| global.h, type ERRORLEVEL:
 ------------------------------------------------------------------------- */
BeginEnum ( ERRLVL )
  enumerator ( errLvl0, "+error-level-0+",		0,
	       "No error was encountered." )
  and
  enumerator ( errLvlSuspended, "+error-level-suspended+",	1,
	       "This is no real error, the server is currently suspended.\
 The client should do the same call once more, after a bit of waiting\
 (this is already handled in the RPC client layer, so this error level is\
 never seen in the LISP client layer)." )
  and
  enumerator ( errLvlInfo, "+error-level-info+",	2,
	       "An information message was returned from the server." )
  and
  enumerator ( errLvlWarn, "+error-level-warn+",	3,
	       "A warning message was returned from the server." )
  and
  enumerator ( errLvlCError, "+error-level-cerror+",	4,
	       "A continuable error occurred on the server (the server\
 continued its operation)." )
  and
  enumerator ( errLvlError, "+error-level-error+",	5,
	       "An error occurred on the server (the offending remote\
 procedure called abended its operation)." )
  and
  enumerator ( errLvlFatal, "+error-level-fatal+",	6,
	       "A fatal error was encountered on the server.\
 The client should send a request to restart the server." )
EndEnum ( ERRLVL );

/* -------------------------------------------------------------------------
| Number of lower free bits in a long (POSTORE) objid. These bits are
| used as a type tag for immediate values (see comments at enum type
| SHTYPETAG):
 ------------------------------------------------------------------------- */
DefineConstant ( nTagBits, "+tag-bits+", NTAGBITS,
		 "Number of tag bits in a long (i.e. 32 bit) OBJID." );
DefineConstant ( nTagMask, "+tag-mask+",
		 subtract ( shift_left ( 1, NTAGBITS ), 1 ),
		 /* In C: ( 1 << nTagBits ) - 1 */
		 "And-mask for type tag bits of a long (i.e. 32 bit) OBJID." );

#if defined(RPC)
%
%/* Round up nSizeInBits to match a word boundary: */
%#if ! defined(AlignBitsToWords)
%#define	AlignBitsToWords( /* FIXNUM */ nSizeInBits ) \
  (FIXNUM) ( ( (nSizeInBits) + nSizeOfPostoreWord * nBitsPerByte - 1 ) / \
	     ( nSizeOfPostoreWord * nBitsPerByte ) )
%#endif	/* ! AlignBitsToWords */
%
%/* -------------------------------------------------------------------------
%| PLOB RPC initialization and RPC error handling
%------------------------------------------------------------------------- */
%
%extern DLLEXPORTVAR int	__bInitializePlob__	/* = TRUE */;
%extern DLLEXPORTVAR int	__bDeinitializePlob__	/* = FALSE */;
%void		fnServerInitializePlob	( int argc, char * argv [] );
%#if ! defined(INITIALIZEPLOB)
%void		fnInitializePlob	( void );
%#define	INITIALIZEPLOB		((__bInitializePlob__)?\
					 (fnInitializePlob(),TRUE):FALSE)
%#endif	/* ! INITIALIZEPLOB */
%
%#if	(RPC_SVC+0)
%
%extern DLLEXPORTVAR int	nGlobalStored;
%extern DLLEXPORTVAR OBJID	oGlobalSession;
%
%#if !defined(UNSTORESESSION)
%#define	UNSTORESESSION() \
(__bJmpBufErrorValid__=FALSE,nGlobalStored=0,oGlobalSession=NULLOBJID)
%#endif
%#if !defined(UnstoreSession)
%#define	UnstoreSession() \
((--nGlobalStored==0)?(UNSTORESESSION(),TRUE):FALSE)
%#endif
%
%extern DLLEXPORTVAR LPCSTR	__lpszErrorMessage__;
%extern DLLEXPORTVAR ERRLVL	__nErrorLevel__;
%
%/* Called once for server initialization: */
%#define	RPC_SERVER_INITIALIZE(argc,argv) \
  fnServerInitializePlob(argc,argv)
%
%/* Called on entry into a server procedure: */
%#define	RPC_SERVER_ENTRY() \
{\
   UNSTORESESSION ();\
   __nErrorLevel__	= errLvl0;\
   if ( __lpszErrorMessage__ != NULL ) {\
      free ( (LPVOID) __lpszErrorMessage__ );\
      __lpszErrorMessage__	= (LPCSTR) NULL;\
  }\
  INITIALIZEPLOB;\
}
%#define	RPC_SERVER_ERRLVL()		\
((__bDeinitializePlob__)?0:__nErrorLevel__)
%#define	RPC_SERVER_ERRMSG()		\
((__bDeinitializePlob__)?(LPCSTR) NULL:__lpszErrorMessage__)
%
%#include	<rpc/rpc.h>
%#include	<rpc/svc.h>
%
%void		fnInitializeAfterRegister	( void );
%static void fnSvcRun	( void )
%{
%  INITIALIZEPLOB;
%  fnInitializeAfterRegister ();
%  svc_run();
%} /* fnSvcRun */
%
%#ifdef  svc_run
%#undef	 svc_run
%#endif
%#define svc_run	fnSvcRun
%
%#endif	/* RPC_SVC */
%
%#if	(RPC_CLNT+0)
%
%#define	RPC_CLIENT_INITIALIZE()		INITIALIZEPLOB
%
%extern PCLIENT		fnClientCreate		( LPCSTR	pszHost,
%						  LPCSTR	pszTransport );
%#define	RPC_CLIENT_CREATE(host,transport)\
	fnClientCreate ( host, transport )
%
%extern PCLIENT		fnClientDestroy		( PCLIENT	pClient );
%#define	RPC_CLIENT_DESTROY(pclient)\
	fnClientDestroy ( pclient )
%
%#define	RPC_CLIENT_ERROR(msg)		\
((__bDeinitializePlob__)?\
 FALSE:\
 (fnLISPerrorCallback(errLvlError,NULL,msg),TRUE))
%#define	RPC_CLIENT_CERROR(cont,msg)	\
((__bDeinitializePlob__)?\
 FALSE:\
 (fnLISPerrorCallback(errLvlCError,cont,msg),TRUE))
%#define	RPC_CLIENT_SERROR(lvl,proc,msg)	\
  ((__bDeinitializePlob__)?\
   FALSE:\
   (fnLISPserrorCallback(lvl,proc,msg),TRUE))
%
%#endif	/* RPC_CLNT */
%
%#if	(RPC_SVC+0)||(RPC_CLNT+0)
%
%#define xdr_fnServerObjectReadValues_rets xdr_fnPatchedServerObjectReadValues_rets
%#define xdr_fnServerObjectWriteValues_args xdr_fnPatchedServerObjectWriteValues_args
%#define xdr_fnServerObjectPeekValues_rets xdr_fnPatchedServerObjectPeekValues_rets
%#define xdr_fnServerObjectPoke_args xdr_fnPatchedServerObjectPoke_args
%#include	"plobpxdr.h"
%
%#endif	/* RPC_SVC||RPC_CLNT */
%
#endif	/* RPC */

#if defined(C2C)

/* -------------------------------------------------------------------------
| Program options
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR const char	szOptionDirectory []	/* = "-directory" */;
extern DLLEXPORTVAR const char	szOptionPort	 []	/* = "-port" */;
extern DLLEXPORTVAR char	szPlobd [ MAX_FNAME ]	/* = "plobd" */;

/* -------------------------------------------------------------------------
| String constants
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR const char	szCantAddress []	/* = */
  /* "Can't get real address of object referenced by objid %d." */;
extern DLLEXPORTVAR const char	szCantIndex []		/* = */
  /* "Can't index object %s with index %d." */;
extern const char		szInvalidAlignment []	/* = */
  /* "Illegal aligned index %d; must be aligned to %d boundaries." */;

/* -------------------------------------------------------------------------
| PLOB caller maintenance
| Actual session (i.e. persistent heap) referenced:
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR OBJID	oGlobalSession		/* = NULLOBJID */;
extern DLLEXPORTVAR int		nGlobalStored		/* = 0 */;

#if !defined(StoreSession)
#define	StoreSession( session ) \
((nGlobalStored++==0)?(oGlobalSession=(session),TRUE):FALSE)
#endif

#if !defined(UNSTORESESSION)
#define	UNSTORESESSION() \
(__bJmpBufErrorValid__=FALSE,nGlobalStored=0,oGlobalSession=NULLOBJID)
#endif

#if !defined(UnstoreSession)
#define	UnstoreSession() \
((--nGlobalStored==0)?(UNSTORESESSION(),TRUE):FALSE)
#endif

/* -------------------------------------------------------------------------
| PLOB initialization
 ------------------------------------------------------------------------- */
extern int	nGlobalArgC			/* = 0 */;
extern char	**ppszGlobalArgV		/* = NULL */;
#if ! defined(INITIALIZEPLOB) && ! defined(C2RPC)
extern DLLEXPORTVAR int		__bInitializePlob__	/* = TRUE */;
extern DLLEXPORTVAR int		__bDeinitializePlob__	/* = FALSE */;
void			fnInitializePlob	( void );
#define	INITIALIZEPLOB	((__bInitializePlob__)?\
			 (fnInitializePlob(),TRUE):FALSE)
#endif	/* ! INITIALIZEPLOB */

/* -------------------------------------------------------------------------
| PLOB De-initialization
 ------------------------------------------------------------------------- */
void			fnDeinitializePlob	( void );

/* -------------------------------------------------------------------------
| objid and typetag
 ------------------------------------------------------------------------- */

enum {
  nRequiredKeyAlignment	= ( 1 << nTagBits )
};

typedef OBJID * 	LPOBJID;
typedef SHTYPETAG *	LPSHTYPETAG;

/* -------------------------------------------------------------------------
| Some utility macros
 ------------------------------------------------------------------------- */
/* Round up nSizeInBits to match a word boundary: */
#if ! defined(AlignBitsToWords) && ! defined(C2RPC)
#define	AlignBitsToWords( /* FIXNUM */ nSizeInBits ) \
(FIXNUM) ( ( (nSizeInBits) + nSizeOfPostoreWord * nBitsPerByte - 1 ) / \
	   ( nSizeOfPostoreWord * nBitsPerByte ) )
#endif	/* ! AlignBitsToWords */
     
#define	allowmarker		((OBJID)eshAllowTag)
#define	denymarker		((OBJID)eshDenyTag)
#define	endmarker		((OBJID)eshEndTag)
#define	matchanymarker		((OBJID)eshMatchAnyTag)
#define	matchnevermarker	((OBJID)eshMatchNeverTag)
#define	maxmarker		((OBJID)eshMaxTag)
#define	minmarker		((OBJID)eshMinTag)
#define	unbound			((OBJID)eshUnboundTag)

#define	boundp( oObjId )	((oObjId)!=NULLOBJID&&(oObjId)!=unbound)
#define	doublefloatp( oObjId )	(typetagof(oObjId)==eshDoubleFloatTag)
#define	endmarkerp( oObjId )	((oObjId)==endmarker)
#define	functionp( oObjId )	(typetagof(oObjId)==eshFunctionTag)
#define	immediatep( oObjId )	(((oObjId)&nTagMask)!=0)
#define	integerp( oObjId )	(typetagof(oObjId)==eshFixnumTag||\
				 typetagof(oObjId)==eshBignumTag)
#define markerp( oObjId )	(((oObjId)&nTagMask)==eshMarkerTag)
#define	matchanymarkerp( oObjId )	((oObjId)==eshMatchAnyTag)
#define	matchnevermarkerp( oObjId )	((oObjId)==eshMatchNeverTag)
#define	maxmarkerp( oObjId )	((oObjId)==maxmarker)
#define	minmarkerp( oObjId )	((oObjId)==minmarker)
#define	shortfloatp( oObjId )	(typetagof(oObjId)==eshShortFloatTag)
#define	singlefloatp( oObjId )	(typetagof(oObjId)==eshSingleFloatTag)

#define	makendmarker( oObjId )	(oObjId=eshEndTag)
#define	makmaxmarker( oObjId )	(oObjId=eshMaxTag)
#define	makminmarker( oObjId )	(oObjId=eshMinTag)
#define	makunbound( oObjId )	(oObjId=unbound)
#define	symbolp( oObjId )	(typetagof(oObjId)==eshSymbolTag)

/* -------------------------------------------------------------------------
| Object structure for persistent PLOB objects.
| Stable Heap SH-vector indices and corresponding structures:
 ------------------------------------------------------------------------- */
/* 'Raw' offsets: */
enum {
  eshSHvectorIdxLock		= -1,
  eshSHvectorIdxObjIds		=  0,
  eshSHvectorIdxSize		=  1,

  eshSHvectorIdxFirstObjId	=  2,

  eshSHvectorIdxTypeTag		=  2,
  eshSHvectorIdxLockedBy	=  3,

  eshSHvectorIdxFirstData	=  4
};

/* The components of the following struct's must have OFFSETs (for the
   meaning of the term OFFSET see _global.h) equal to its corresponding
   eshSHvectorIdx...-constant defined above: */

/* Structure describing a postore object format header information: */
typedef struct {
  psint	nObjIds;	/* Number of keys (i.e. objids) in object */
  psint	nSize;		/* Total size of object */
}	POSTOREHEADER, * LPPOSTOREHEADER;

/* Structure describing a PLOB object header information: */
typedef struct {
  POSTOREHEADER	PostoreHeader;
  OBJID		oTypeTag;	/* Lower 16 bit: Type tag. Rest: Flags */
  OBJID		oLockedBy;	/* PLOB object which locked the object */
}	PLOBHEADER, * LPPLOBHEADER;

/* -------------------------------------------------------------------------
| ObjId <-> characters
 ------------------------------------------------------------------------- */
#define	charp( oObjId )				\
((((unsigned int)(oObjId))&nTagMask)==eshCharacterTag)

extern DLLEXPORTVAR const char	szObjIdNotCharacter []	/* = */
/* "The long objid %d is no immediate character." */;

#define	OBJID2CHAR( oObjId )			\
(char)(((unsigned int)(oObjId))>>nTagBits)

#define	ObjId2Char( oObjId )			\
((charp(oObjId))?				\
 OBJID2CHAR(oObjId):				\
 (ERROR((szObjIdNotCharacter,(oObjId))),'\0'))

#define	CHAR2OBJID( cCharacter )		\
(OBJID)((((unsigned int)(cCharacter))<<nTagBits)|eshCharacterTag)

#define	Char2ObjId( cCharacter )		CHAR2OBJID(cCharacter)

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitCommonPlobModule		( void );
void		fnInitializePlobModule		( void );
void		fnDeinitializePlobModule	( void );
void		fnDeinitCommonPlobModule	( void );

/* -------------------------------------------------------------------------
| Process suspend/wakeup, implemented in c-plob-misc.c:
 ------------------------------------------------------------------------- */
BOOL		fnPLOBsuspendCallback		( OBJID oLockBy,
						  OBJID oToLock,
						  LPCSTR lpszReason );
BOOL		fnPLOBwakeupCallback		( OBJID oLockBy,
						  OBJID oToLock,
						  LPCSTR lpszReason );

#endif /* #if defined(C2C) */

DefineFunction ( BOOL,
		 fnClientObjectCanModify, "c-sh-can-modify",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );

#if ! defined(LISP)	/* server: */
DefineFunction ( voidResult,
		 fnServerDbClose, "c-sh-close",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bWithGarbageCollection ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( voidResult,
		 fnClientDbClose, "c-sh-close",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bWithGarbageCollection ) ) );
#endif	/* ! RPC */

DefineFunction ( BOOL,
		 fnClientDbConfiguration, "c-sh-configuration",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FIXNUM, value_out, pnFlags )
		   and
		   argument ( FIXNUM, value_out, pnMinimumKey )
		   and
		   argument ( FIXNUM, value_out, pnMaximumKey )
		   and
		   argument ( FIXNUM, value_out,
			      pnKeyAlignment ) ) );

#if ! defined(LISP)	/* server: */
DefineFunction ( SHORTOBJID,
		 fnServerDbCreateObject, "c-sh-create-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( FIXNUM, value_in, nExtraReferences )
		   and
		   argument ( SHTYPETAG, value_in, eTypeTagValues )
		   and
		   argument ( FIXNUM, value_in, nExtraValues ) ) );
DefineFunction ( FIXNUM,
		 fnServerDbCreateObjects, "c-sh-create-objects",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( FIXNUM, value_in, nExtraReferences )
		   and
		   argument ( SHTYPETAG, value_in, eTypeTagValues )
		   and
		   argument ( FIXNUM, value_in, nExtraValues )
		   and
		   argument ( FIXNUM, value_in, nObjIds )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_out, pObjIds ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientDbCreateObject, "c-sh-create-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( FIXNUM, value_in, nExtraReferences )
		   and
		   argument ( SHTYPETAG, value_in, eTypeTagValues )
		   and
		   argument ( FIXNUM, value_in, nExtraValues ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( voidResult,
		 fnServerObjectDestroy, "c-sh-destroy-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( voidResult,
		 fnClientObjectDestroy, "c-sh-destroy-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerObjectObjIdSize, "c-sh-objid-size",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientObjectObjIdSize, "c-sh-objid-size",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */

BeginEnum ( GETACTION )
  enumerator ( eGetPortPassive, "+get-port-passive+", 1,
	       "Get RPC port of an existing database,\
 do not start a server on the directory." )
  and
  enumerator ( eGetPortActive, "+get-port-active+", 2,
	       "Get RPC port of an existing database,\
 but only if there is a server running on the directory." )
  and
  enumerator ( eCreateDatabase, "+create-database+", 4,
	       "Create database if it does not exist." )
  and
  enumerator ( eStartServer, "+start-server+", 8,
	       "Start a server on the database directory,\
 if non is running." )
EndEnum ( GETACTION );

DefineFunction ( SHORTOBJID,
		 fnServerDbOpen, "c-sh-open",
		 ( argument ( CONST_STRING, vector_in, szDirectory )
		   and
		   argument ( CONST_STRING, vector_in, szUserName )
		   and
		   argument ( CONST_STRING, vector_in, szDescription )
		   and
		   argument ( FIXNUM, value_in, nMinAddrInK )
		   and
		   argument ( OBJID, value_out, poObjIdMin )
		   and
		   argument ( OBJID, value_out, poObjIdMax ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BOOL,
		 fnClientDbConnect, "c-sh-connect",
		 ( argument ( CONST_STRING, vector_in, szURL ) ) );

DefineFunction ( SHORTOBJID,
		 fnClientDbOpen, "c-sh-open",
		 ( argument ( CONST_STRING, vector_in, szDescription )
		   and
		   argument ( FIXNUM, value_in, nMinAddrInK ) ) );
#endif	/* ! RPC */

/* A type used for multi-level lock requests in function SH_set_lock: */
BeginEnum ( SHLOCK )
  /* Error returns: */
  enumerator ( eshKeyNotFound, "+error-key-not-found+", -10,
	       "The key was not found." )
  and
  enumerator ( eshLockErrorFirst, "+lock-error-first+",
	       ESHLOCKERRORFIRST,
	       "The first error value indicating a lock error." )
  and
  enumerator ( eshLockErrorLast, "+lock-error-last+",
	       ESHLOCKERRORLAST,
	       "The last error value indicating a lock error." )
  and
  enumerator ( eshLockDenied, "+lock-denied+", -9,
	       "The locking-object denied to lock an object-to-lock.\
 This error arises e.g. when a (locking) heap without an active\
 transaction is asked to lock an object (to lock)." )
  and
  enumerator ( eshUnknownLockMode, "+unknown-lock-mode+", -8,
	       "The current lock mode could not be determined." )
  and
  enumerator ( eshGeneralError, "+general-error+", -7,
	       "An error occurred." )
  and
  enumerator ( eshUnlockFailed, "+unlock-failed+", -6,
	       "The unlock could not be performed." )
  and
  enumerator ( eshWriteFailed, "+write-failed+", -5,
	       "Writing to an object failed." )
  and
  enumerator ( eshLockFailed, "+lock-failed+", -4,
	       "The requested lock level and mode could not be established.\
 This is signalled when a general lock error arises." )
  and
  enumerator ( eshLockConflictFirst, "+lock-conflict-first+",
	       ESHLOCKCONFLICTFIRST,
	       "The first error value indicating a lock conflict." )
  and
  enumerator ( eshLockConflictLast, "+lock-conflict-last+",
	       ESHLOCKCONFLICTLAST,
	       "The last error value indicating a lock conflict." )
  and
  enumerator ( eshLockConflictElement, "+lock-conflict-element+",
	       ESHLOCKCONFLICTELEMENT,
	       "A non-solvable conflict occurred at element (i.e. object\
 slot) locking. Repeating the offending locking at some time later might\
 be successful if the conflict was resolved in the meantime." )
  and
  enumerator ( eshLockConflictVector, "+lock-conflict-vector+",
	       ESHLOCKCONFLICTVECTOR,
	       "A non-solvable conflict occurred at vector (i.e. object)\
 locking. Repeating the offending locking at some time later might be\
 successful if the conflict was resolved in the meantime." )
  and
  enumerator ( eshLockConflictStore, "+lock-conflict-store+",
	       ESHLOCKCONFLICTSTORE,
	       "A non-solvable conflict occurred at store locking.\
 Repeating the offending locking at some time later might be successful\
 if the conflict was resolved in the meantime." )
  and
  /* Additional possible bitmasks for return values of SH_set_lock: */
  enumerator ( eshLockModeReadOnlyIntent, "+lock-mode-read-only-intent+",
	       ESHLOCKMODEREADONLYINTENT,
	       "Indicates that a sub-object has a read-only lock set." )
  and
  enumerator ( eshLockModeReadIntent, "+lock-mode-read-intent+",
	       ESHLOCKMODEREADINTENT,
	       "Indicates that a sub-object has a read lock set." )
  and
  enumerator ( eshLockModeWriteIntent, "+lock-mode-write-intent+",
	       ESHLOCKMODEWRITEINTENT,
	       "Indicates that a sub-object has a write lock set." )
  and
  /* Bit masks for setting multi level locks.
     A lock can be set on 4 levels with 3 modes:

      Level | constant            | description
     -------+---------------------+----------------------------------
        0   | eshLockLevelNothing | Lock nothing at all.
        1   | eshLockLevelElement | Lock one element of a sh-vector.
            |                     | Therefore, the index of the
 	    |                     | element to lock must be passed
	    |                     | to SH_set_lock.
        2   | eshLockLevelVector  | Lock a single sh-vector.
        3   | eshLockLevelStore   | Lock the whole stable heap.

      Mode      | constant            | description
     -----------+---------------------+-----------------------------------
      ---       | eshLockModeNothing  | Set no lock on passed level; just
                |                     | read old lock mode on passed
                |                     | level.
      read-only | eshLockModeReadOnly | Set a read-only lock on passed
                |                     | level.
      read      | eshLockModeRead     | Set a read lock on passed level.
      write     | eshLockModeWrite    | Set a write lock on passed level.

    When a lock can't be established because a non-solvable lock conflict
    would occurre, eshLockConflict is returned.
  */
  enumerator ( eshLockLevelNothing, "+lock-level-nothing+",
	       ESHLOCKLEVELNOTHING,
	       "Request no lock at all." )
  and
  enumerator ( eshLockLevelMin, "+lock-level-min+",
	       ESHLOCKLEVELELEMENT,
	       "Minimal possible lock level." )
  and
  enumerator ( eshLockLevelMax, "+lock-level-max+",
	       ESHLOCKLEVELSTORE,
	       "Maximal possible lock level." )
  and
  enumerator ( eshLockLevelElement, "+lock-level-element+",
	       ESHLOCKLEVELELEMENT,
	       "Request lock for a single element of a sh-vector." )
  and
  enumerator ( eshLockLevelVector, "+lock-level-vector+",
	       ESHLOCKLEVELVECTOR,
	       "Request lock for a single sh-vector." )
  and
  enumerator ( eshLockLevelStore, "+lock-level-store+",
	       ESHLOCKLEVELSTORE,
	       "Request lock for whole persistent storage." )
  and
  /* Values for lock modes (read resp. write lock mode): */
  enumerator ( eshLockModeNothing, "+lock-mode-nothing+",
	       ESHLOCKMODENOTHING,
	       "[Un]set no lock; just read old lock." )
  and
  enumerator ( eshLockModeReadOnly, "+lock-mode-read-only+",
	       ESHLOCKMODEREADONLY,
	       "[Un]set a read-only lock." )
  and
  enumerator ( eshLockModeRead, "+lock-mode-read+",
	       ESHLOCKMODEREAD,
	       "[Un]set a read lock." )
  and
  enumerator ( eshLockModeWrite, "+lock-mode-write+",
	       ESHLOCKMODEWRITE,
	       "[Un]set a write lock." )
  and
  /* Special bit masks for esoteric lock requests: */
  enumerator ( eshUnlock, "+lock-unlock+", hex ( 100 ),
	       "Unlock the object." )
  and
  enumerator ( eshLockForce, "+lock-force+", hex ( 200 ),
	       "Force the [un]lock. This is a kind of 'emergency [un]lock';\
 the [un]lock is always performed to the level specified regardless\
 to its previous lock state. May release locks held by other objects too." )
  and
  enumerator ( eshLockPeek, "+lock-and-peek+", hex ( 400 ),
	       "Do prepare object peeking at locking." )
  and
  /* Often used short-hands: */
  enumerator ( eshLockElementRead, "+lock-element-read+",
	       bitwise_or ( ESHLOCKLEVELELEMENT, ESHLOCKMODEREAD ),
	       "Request a read lock for a single element of a sh-vector." )
  and
  enumerator ( eshLockVectorRead, "+lock-vector-read+",
	       bitwise_or ( ESHLOCKLEVELVECTOR, ESHLOCKMODEREAD ),
	       "Request a read lock for a single sh-vector." )
  and
  enumerator ( eshLockStoreRead, "+lock-store-read+",
	       bitwise_or ( ESHLOCKLEVELSTORE, ESHLOCKMODEREAD ),
	       "Request a read lock for the whole stable heap." )
  and
  enumerator ( eshLockElementWrite, "+lock-element-write+",
	       bitwise_or ( ESHLOCKLEVELELEMENT, ESHLOCKMODEWRITE ),
	       "Request a write lock for a single element of a sh-vector." )
  and
  enumerator ( eshLockVectorWrite, "+lock-vector-write+",
	       bitwise_or ( ESHLOCKLEVELVECTOR, ESHLOCKMODEWRITE ),
	       "Request a write lock for a single sh-vector." )
  and
  enumerator ( eshLockStoreWrite, "+lock-store-write+",
	       bitwise_or ( ESHLOCKLEVELSTORE, ESHLOCKMODEWRITE ),
	       "Request a write lock for the whole stable heap." )
  and
  /* Internal used bit masks: */
  enumerator ( eshLockLevelMask, "+lock-level-mask+",
	       bitwise_or
	       ( ESHLOCKLEVELNOTHING,
		 bitwise_or
		 ( ESHLOCKLEVELELEMENT,
		   bitwise_or ( ESHLOCKLEVELVECTOR,
			        ESHLOCKLEVELSTORE ) ) ),
	       "Bitmask over all lock levels." )
  and
  enumerator ( eshLockModeMask, "+lock-mode-mask+",
	       bitwise_or
	       ( ESHLOCKMODENOTHING,
		 bitwise_or
		 ( ESHLOCKMODEREADONLY,
		   bitwise_or
		   ( ESHLOCKMODEREAD,
		     ESHLOCKMODEWRITE ) ) ),
	       "Bitmask over all lock modes." )
  and
  enumerator ( eshLockModeExcl, "+lock-mode-exclusive-mask+",
	       ESHLOCKMODEWRITE,
	       "Bitmask over all exclusive lock modes." )
EndEnum ( SHLOCK );

#if ! defined(RPC)	/* client: */
DefineFunction ( BOOL,
		 fnClientObjectFlush, "c-sh-flush-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   /* Flag if object should be removed from cache
		      after flushing: */
		   argument ( BOOL, value_in, bRemoveFromCache ) ) );
#endif	/* ! RPC */
#if ! defined(LISP)	/* server: */
DefineFunction ( BOOL,
		 fnServerObjectFlush, "c-sh-flush-object",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! LISP */

#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientObjectReadChars, "c-sh-read-chars",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( SHORTOBJID, value_in, oExpectingClass )
		   and
		   argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		   and
		   argument ( FIXNUM, value_in, nSizeInCharacters )
		   and
		   argument ( STRING ( nSizeInCharacters ),
			      vector_out, pszBuffer ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectReadAtIndex, "c-sh-read-index",
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
		   argument ( FIXNUM, value_out, pnValue )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTag ) ) );
DefineFunction ( FIXNUM,
		 fnServerObjectReadAtIndices, "c-sh-read-indices",
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
		   argument ( FIXNUM, value_in, nObjIds )
		   and
		   argument ( VECTOR ( int, nObjIds ),
			      vector_out, pObjIds )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_out, pnTypeTags ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientObjectReadAtIndex, "c-sh-read-index",
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
		   argument ( FIXNUM, value_out, pnValue )
		   and
		   argument ( SHTYPETAG, value_out, pnTypeTag ) ) );
DefineFunction ( FIXNUM,
		 fnClientObjectReadAtIndices, "c-sh-read-indices",
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
		   argument ( FIXNUM, value_in, nObjIds )
		   and
		   argument ( VECTOR ( int, nObjIds ),
			      vector_out, pObjIds )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_out, pnTypeTags ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectReadObjId, "c-sh-read-objid",
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
		   argument ( FIXNUM, value_out, pnObjId ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientObjectReadObjId, "c-sh-read-objid",
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
		   argument ( FIXNUM, value_out, pnObjId ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerObjectReadValues, "c-sh-read-values",
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
		   argument ( SHTYPETAG, value_in, nElementTypeTagIn )
		   and
		   argument ( FIXNUM, value_in, nSizeInElementsIn )
		   and
		   argument ( SHTYPETAG, value_out, pnElementTypeTagOut )
		   and
		   argument ( FIXNUM, value_out, pnSizeInElementsOut )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue(1,&nElementTypeTagIn,
							  &nSizeInElementsIn)),
			      vector_out, pBuffer ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientObjectReadValues, "c-sh-read-values",
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
		   argument ( SHTYPETAG, value_in, nElementTypeTag )
		   and
		   argument ( FIXNUM, value_in, nSizeInElements )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue(1,&nElementTypeTag,
							  &nSizeInElements ) ),
			      vector_out, pBuffer )
		   and
		   argument ( FIXNUM, value_in, nUnmask )
		   and
		   argument ( FIXNUM, value_in, nBufferOffset ) ) );
#endif	/* ! RPC */

DefineFunction ( BOOL,
		 fnClientDbStatistics, "c-sh-statistics",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FIXNUM, value_out, pnMaximumSpace )
		   and
		   argument ( FIXNUM, value_out, pnAllocatedSpace )
		   and
		   argument ( FIXNUM, value_out,
			      pnUnallocatedSpace )
		   and
		   argument ( FIXNUM, value_out,
			      pnUnusedAllocatedSpace )
		   and
		   argument ( FIXNUM, value_out,
			      pnAllocatedManagementSpace )
		   and
		   argument ( FIXNUM, value_out,
			      pnNumberOfObjects ) ) );

#if ! defined(LISP)	/* server: */
DefineFunction ( voidResult,
		 fnServerDbStabilise, "c-sh-stabilise",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( voidResult,
		 fnClientDbStabilise, "c-sh-stabilise",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHTYPETAG,
		 fnServerObjectTypeTag, "c-sh-type-tag",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHTYPETAG,
		 fnClientObjectTypeTag, "c-sh-type-tag",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerObjectValueSize, "c-sh-value-size",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientObjectValueSize, "c-sh-value-size",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! RPC */

#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientObjectWriteChars, "c-sh-write-chars",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( SHORTOBJID, value_in, oExpectingClass )
		   and
		   argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		   and
		   argument ( FIXNUM, value_in, nSizeInCharacters )
		   and
		   argument ( CONST_STRING, vector_in, pszBuffer ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectWriteAtIndex, "c-sh-write-index",
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
		   argument ( FIXNUM, value_in, nValue )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagValue ) ) );
DefineFunction ( FIXNUM,
		 fnServerObjectWriteAtIndices, "c-sh-write-indices",
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
		   argument ( FIXNUM, value_in, nObjIds )
		   and
		   argument ( VECTOR ( int, nObjIds ),
			      vector_in, pObjIds )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_in, pnTypeTags ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientObjectWriteAtIndex, "c-sh-write-index",
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
		   argument ( FIXNUM, value_in, nValue )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagValue ) ) );
DefineFunction ( FIXNUM,
		 fnClientObjectWriteAtIndices, "c-sh-write-indices",
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
		   argument ( FIXNUM, value_in, nObjIds )
		   and
		   argument ( VECTOR ( int, nObjIds ),
			      vector_in, pObjIds )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_in, pnTypeTags ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerObjectWriteObjId, "c-sh-write-objid",
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
		   argument ( SHORTOBJID, value_in, oShortObjIdWrite ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientObjectWriteObjId, "c-sh-write-objid",
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
		   argument ( SHORTOBJID, value_in, oShortObjIdWrite ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerObjectWriteValues, "c-sh-write-values",
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
		   argument ( SHTYPETAG, value_in, nElementTypeTag )
		   and
		   argument ( FIXNUM, value_in, nSizeInElements )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue(1,&nElementTypeTag,
							  &nSizeInElements ) ),
			      vector_in, pBuffer ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientObjectWriteValues, "c-sh-write-values",
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
		   argument ( SHTYPETAG, value_in, nElementTypeTag )
		   and
		   argument ( FIXNUM, value_in, nSizeInElements )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue(1,&nElementTypeTag,
							  &nSizeInElements ) ),
			      vector_in, pBuffer )
		   and
		   argument ( FIXNUM, value_in, nUnmask )
		   and
		   argument ( FIXNUM, value_in, nBufferOffset ) ) );
#endif	/* ! RPC */

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
