/* -------------------------------------------------------------------------
| Module	plobtype.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		21.1.94
| Description	Foreign language interface to postore persistent heap.
|		Type handling
|
| Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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
;;;; For further comments look into file plobtype.h
;;;; -------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

BeginEnum ( COMPARETAG )
  enumerator ( eshEqual, "+equal+", 4,
	       "Compare like LISP equal." )
  and
  enumerator ( eshEql, "+eql+", 0,
	       "Compare like LISP eql." )
  and
  enumerator ( eshEq, "+eq+", 3,
	       "Compare like LISP eq." )
  and
  enumerator ( eshNotEqual, "+not-equal+", 9,
	       "Compare result: element 1 not equal element 2." )
  and
  enumerator ( eshNotEql, "+not-eql+", 5,
	       "Compare result: element 1 not eql element 2." )
  and
  enumerator ( eshNotEq, "+not-eq+", 8,
	       "Compare result: element 1 not eq element 2." )
  and
  enumerator ( eshLessEqual, "+less-equal+", -2,
	       "Compare result: element 1 <= element 2." )
  and
  enumerator ( eshLess, "+less+", -1,
	       "Compare result: element 1 < element 2." )
  and
  enumerator ( eshGreater, "+greater+", 1,
	       "Compare result: element 1 > element 2." )
  and
  enumerator ( eshGreaterEqual, "+greater-equal+", 2,
	       "Compare result: element 1 >= element 2." )
EndEnum ( COMPARETAG );

/* -------------------------------------------------------------------------
| Type representation handling
 ------------------------------------------------------------------------- */
BeginEnum ( TYPEFLAGS )
  enumerator ( typeNoFlags, "+type-no-flags+",			hex(00),
	       "Empty type flag." )
  and
  enumerator ( typeImmediateP, "+type-immediate-p+",		hex(01),
	       "Immediate type" )
  and
  enumerator ( typeNotAllocatableP, "+type-not-alloctable-p+",	hex(02),
	       "Object cant be allocated persistently." )
  and
  enumerator ( typeTransientP, "+type-transient-p+",		hex(04),
	       "Instances of the type are transient." )
  and
  enumerator ( typeVarSizeObjIdP, "+type-var-size-objid-p+",	hex(08),
	       "Type with variable sized objid field." )
  and
  enumerator ( typeVarSizeValueP, "+type-var-size-value-p+",	hex(10),
	       "Type with an attached value field." )
  and
  enumerator ( typeRecycleP, "+type-recycle-p+",		hex(20),
	       "Destroyed objects are recycled for create." )
  and
  enumerator ( typeNotYetImplemented,
	       "+type-not-yet-implemented",			hex(40),
	       "Mark for not yet implemented/limited supported types." )
  and
  enumerator ( typeNotCachableP,
	       "+type-not-cachable-p",				hex(80),
	       "Instances of this type must not be allocated in a client's cache." )
#if defined(C2C)
  and
  typeFlagMin		= typeNoFlags,
  typeFlagMax		= typeNotYetImplemented,
  typeVarSizeMask	= ( typeVarSizeObjIdP | typeVarSizeValueP )
#endif
EndEnum ( TYPEFLAGS );

#if ! defined(RPC)
DefineFunction ( BOOL,
		 fnShortObjIdValidP, "c-sh-objid-valid-p",
		 ( argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( CONST_STRING,
		 fnServerDbTypeTagName, "c-sh-type-string",
		 ( argument ( SHTYPETAG, value_in, nTypeTag ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientDbTypeTagName, "c-sh-type-string",
		 ( argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( STRING ( nBuffer ), vector_out, pszBuffer )
		   and
		   argument ( FIXNUM, value_in, nBuffer ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerObjectPrettyPrint, "c-sh-pprint-objid",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( STRING ( nBuffer ),
			      vector_out, lpszBuffer )
		   and
		   argument ( FIXNUM, value_in, nBuffer ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientObjectPrettyPrint, "c-sh-pprint-objid",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTag )
		   and
		   argument ( STRING ( nBuffer ),
			      vector_out, lpszBuffer )
		   and
		   argument ( FIXNUM, value_in, nBuffer ) ) );
#endif	/* ! RPC */

DefineFunction ( FIXNUM,
		 fnShortPrintSymbol, "c-sh-pprint-symbol",
	         ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortSymbol )
		   and
		   argument ( STRING ( nBuffer ),
			      vector_out, lpszBuffer )
		   and
		   argument ( FIXNUM, value_in, nBuffer ) ) );

#if ! defined(RPC)	/* client: */

/* Enumerates all known PLOB types: */
DefineFunction ( BOOL,
		 fnLISPmapClassInfoFirst, "c-sh-map-class-info-first",
		 ( argument ( SHTYPETAG, value_out, pnTypeTag )
		   and
		   argument ( STRING ( nTypeName ),
			      vector_out, lpszTypeName )
 		   and
		   argument ( FIXNUM, value_in, nTypeName )
		   and
		   argument ( FIXNUM, value_out, pnObjIdSize )
		   and
		   argument ( FIXNUM, value_out, pnValueSize)
		   and
		   argument ( TYPEFLAGS, value_out, pnTypeFlags ) ) );

DefineFunction ( BOOL,
		 fnLISPmapClassInfoNext, "c-sh-map-class-info-next",
		 ( argument ( SHTYPETAG, value_out, pnTypeTag )
		   and
		   argument ( STRING ( nTypeName ),
			      vector_out, lpszTypeName )
 		   and
		   argument ( FIXNUM, value_in, nTypeName )
		   and
		   argument ( FIXNUM, value_out, pnObjIdSize )
		   and
		   argument ( FIXNUM, value_out, pnValueSize )
		   and
		   argument ( TYPEFLAGS, value_out, pnTypeFlags ) ) );

#endif

#if defined(C2C)

/* -------------------------------------------------------------------------
| ObjId <-> type tags
 ------------------------------------------------------------------------- */

#define	typetagp( oObjId )			\
((((unsigned int)(oObjId))&nTagMask)==eshBuiltInTag)

extern CONST char	szObjIdNotTypeTag []	/* = */
/* "The long objid %d is no immediate type tag." */;

/* Version without any checking: */
#define	OBJID2TYPETAG( oObjId )			\
(SHTYPETAG)((((unsigned int)(oObjId))>>nTagBits)&nTypeTagMask)

/* Version with checking: */
#define	ObjId2TypeTag( oObjId )			\
((typetagp(oObjId))?				\
 OBJID2TYPETAG(oObjId):				\
 (ERROR((szObjIdNotTypeTag,(oObjId))),(SHTYPETAG)0))

#define	TYPETAG2OBJID( nTypeTag )		\
(OBJID)(((((unsigned int)(nTypeTag))&nTypeTagMask)<<nTagBits)|eshBuiltInTag)

#define	TypeTag2ObjId( nTypeTag )		TYPETAG2OBJID(nTypeTag)

LPCSTR DLLEXPORT	fnTypeTagName		( SHTYPETAG	eTypeTag );
FIXNUM DLLEXPORT	fnTypeTagSizeValue	( FIXNUM	nObjects,
						  SHTYPETAG	* pnTypeTags,
						  FIXNUM	* pnElements );

/* -------------------------------------------------------------------------
| ObjId <-> Flags
 ------------------------------------------------------------------------- */
#define	ObjIdFlagP( oObjId )			\
(typetagp(oObjId))

extern CONST char	szObjIdNotFlag []	/* = */
/* "The long objid %d is no immediate flag word." */;

#define	OBJID2FLAG( oObjId )			\
((((unsigned int)(oObjId))>>nFlagRawBitOffset)&nFlagMask)

#define	ObjId2Flag( oObjId )			\
((ObjIdFlagP(oObjId))?				\
 OBJID2FLAG(oObjId):				\
 (ERROR((szObjIdNotFlag,(oObjId))),0))

#define	FLAG2OBJID( nFlag )			\
(OBJID)(((((unsigned int)(nFlag))&nFlagMask)<<nFlagRawBitOffset)|eshBuiltInTag)

#define	Flag2ObjId( nFlag )			FLAG2OBJID(nFlag)

#define	GETFLAGBIT( oObjId, nBitMask )		\
(((unsigned int)(oObjId)>>nFlagRawBitOffset)&(nBitMask)&nFlagMask)

#define	GetFlagBit( oObjId, nBitMask )		\
((ObjIdFlagP(oObjId))?				\
 GETFLAGBIT(oObjId,nBitMask):			\
 (ERROR((szObjIdNotFlag,(oObjId))),0))

#define	SETFLAGBIT( oObjId, nBitMask, bValue )	\
((bValue)?					\
 (oObjId|=((unsigned int)((nBitMask)&nFlagMask)<<\
	   nFlagRawBitOffset),TRUE):	\
 (oObjId&=~((unsigned int)((nBitMask)&nFlagMask)<<\
	    nFlagRawBitOffset),FALSE))

#define	SetFlagBit( oObjId, nBitMask, bValue )	\
((ObjIdFlagP(oObjId))?				\
 SETFLAGBIT(oObjId,nBitMask,bValue):		\
 (ERROR((szObjIdNotFlag,(oObjId))),FALSE))

/* -------------------------------------------------------------------------
| Object flag bit masks:
 ------------------------------------------------------------------------- */
enum {

  /* Flag if the object itself was modified since the last end
     of transaction: */
  flagDirty		= 0x01,

  /* Flag if not the object itself but one of its referenced objects
     changed since the last end of transaction: */
  flagChanged		= 0x02,

  /* Flag if the object depends on another object. `Depends' in this
     sense means that if the (parent) object referencing the depending
     object is referenced by the client, the chance is rather high
     that the depending object will be referenced too.

     The dependent flag will be evaluated at object locking and
     peeking: When a lock is set onto an object, all its dependent
     objects will be locked too. The lock is only granted to the
     parent object if all of the dependents also received the
     requested lock. At object peeking, all dependent objects will be
     transmitted to the client together with the parent object.

     For example, numbers are marked as being read dependent; so if a
     vector containing numbers is read by the client, the vector and
     all numbers it contains will be sent in one packet to the client: */

  /* 1996/11/12 HK: The definitions have been moved to type
     DEPENDENTMODE in plobmisc.h; the FLAGDEPENDENT* constants are
     defined in plobconst.h:
  flagDependentRead	= FLAGDEPENDENTREAD,
  flagDependentWrite	= FLAGDEPENDENTWRITE,
  */

  /* Flag if the object was created within the current transaction.
     For an object created within the current transaction, following
     assumptions are expected to be true: 1. No other object refers to
     the fresh created object and 2. the object's state at the begin
     of transaction is determined completely by its initialization,
     and not e.g. by state changes applied to the object by a database
     using process. This means for the current transaction, that the
     object's state need not to be stored to the transaction's undo
     log, since the object's state could be recomputed by initializing
     it: */
  flagCreated		= 0x10,

  /* Object is a temporary object, i.e., it may be deleted by Plob
     itself if it has finished some kind of action. For example, a
     btree mapping object might be deleted after the btree has been
     iterated. */
  flagTemporary		= 0x20

  /* Next flag bit masks are 0x40, 0x80, 0x100, 0x200 ... */
};

/* -------------------------------------------------------------------------
| Type tags
 ------------------------------------------------------------------------- */
enum {
  /* A type tag for 'zombie' (i.e. destroyed) objects: */
  eshZombieTag	= nTypeTagMask & ~nTagMask
};

/* -------------------------------------------------------------------------
| Type representation handling
 ------------------------------------------------------------------------- */
#define	IMMEDIATE_TYPE_INFO(eTypeTag,szPackageName,szName,nSize,szFormat) \
(SHTYPETAG)eTypeTag, \
  (LPCSTR)szPackageName,(LPCSTR)szName,0,nSize,(LPCSTR)szFormat, \
  (TYPEFLAGS)((unsigned int)typeImmediateP|(unsigned int)typeNotAllocatableP)

#define	MARKER_TYPE_INFO(eTypeTag,szName) \
IMMEDIATE_TYPE_INFO(eTypeTag,SZPLOB,szName,0,NULL)

typedef struct {
  SHTYPETAG	nTypeTag;
  /* LISP package containing the type name's symbol: */
  LPCSTR	lpszPackageName;
  LPCSTR	lpszTypeName;
  /* Size of objids contained in PLOB object in bits: */
  int		nFixSizeObjId;
  /* Size of values contained in PLOB object in bits: */
  int		nFixSizeValue;
  /* For immediate types: printf format string */
  LPCSTR	lpszFormat;
  TYPEFLAGS	nTypeFlags;
}	CLASSINFO, * LPCLASSINFO;

/* -------------------------------------------------------------------------
| Registering of types
 ------------------------------------------------------------------------- */
#define		RegisterPlobClass( lpClassInfo )	\
fnRegisterPlobClass(__szFile__,__szProc__,__LINE__,	\
		    lpClassInfo,sizeof(*(lpClassInfo)))
BOOL		fnRegisterPlobClass	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  LPCLASSINFO lpClassInfo,
					  int nSizeOfClassInfo );

/* -------------------------------------------------------------------------
| Object printing
 ------------------------------------------------------------------------- */
LPSTR DLLEXPORT	fnPrintImmediateObject	( FIXNUM	nState,
					  SHTYPETAG	nTypeTag,
					  LPSTR		lpszBuffer,
					  size_t	nBuffer );

LPCSTR DLLEXPORT fnCompareTag2String	( COMPARETAG	eFrom,
					  BOOL		bSwap );

LPSTR DLLEXPORT	gfnPrintObjectDetails	( OBJID oSelf,
					  LPSTR lpszBuffer,
					  size_t nBuffer );

/* -------------------------------------------------------------------------
| Module initialization
 ------------------------------------------------------------------------- */

void			fnInitCommonTypeModule		( void );
void			fnInitializeTypeModule		( void );
void			fnDeinitializeTypeModule	( void );
void			fnDeinitCommonTypeModule	( void );

#endif /* #if defined(C2C) */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
