/* -------------------------------------------------------------------------
| Module	splobstruct.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
 ------------------------------------------------------------------------- */

#include	"plobstruct.h"

#define	structurep( oSelf )	(typetagof(oSelf)==eshStructureTag)

#define	symbol_function( oObjId )	\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshSymbolIdxFunction ), eshSymbolTag )
#define	symbol_package( oObjId )	\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshSymbolIdxPackage ), eshSymbolTag )
#define	symbol_plist( oObjId )		\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshSymbolIdxPList ), eshSymbolTag )
#define	symbol_name( oObjId )		\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshSymbolIdxName ), eshSymbolTag )

#define	package_name( oObjId )		\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshPackageIdxName ), eshStructureTag )
#define	package_internals( oObjId )	\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshPackageIdxInternals ), \
	       eshStructureTag )
#define	package_externals( oObjId )	\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshPackageIdxExternals ), \
	       eshStructureTag )

extern const char	szFormatSlots []	/* = "slots=%d/%d" */;
extern const char	szFormatVersNum []	/* = "%d.%02.2d" */;

extern const char	szFormatErrorDescr []	/* = */
/* "Object %s\n"
   "       is no proper %s description object.\n"
   "       Reason: %s." */;
extern const char	szStructure []		/* = "structure" */;
extern const char	szInvalidTypeTag []	/* = "invalid type tag" */;
extern const char	szNotSelfRef []		/* = "not self-referencing" */;
extern const char	szNSlotsNoFixnum []
/* = "number of slots is no fixnum" */;

/* Iff oGlobalPackageDescr is bound, it contains the structure description
   for packages: */
extern OBJID		oGlobalPackageDescr	/* = NULLOBJID */;
extern OBJID		oGlobalStructDescr	/* = NULLOBJID */;
extern OBJID		oGlobalStructSlotDescr	/* = NULLOBJID */;

#define			LocateStructDescr( oSelf )		\
((!boundp(oGlobalStructDescr)||!boundp(oGlobalStructSlotDescr))? \
 (fnLocateStructDescr(oSelf),TRUE):				\
 FALSE)
void			fnLocateStructDescr	( OBJID oSelf );

LPSTR			fnAddName		( OBJID oName,
						  LPSTR lpszBuffer,
						  int nBuffer,
						  LPINT lpnIndex,
						  BOOL bAddLeadingBlank );

/* Structure accessors: */
#define	structure_slotref( oObjId, nIndexSlot )		\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( nIndexSlot ), \
	       (SHTYPETAG) eshStructureTag )
#define	structure_length( oObjId )			\
(shvector_objids(oObjId)+ 				\
 eshSHvectorIdxFirstObjId-eshSHvectorIdxFirstData-eshStructureObjIdSize)

/* ----------------------------------------------------------------------- */
/* Access to persistent packages and symbols; the objids filled by the
   poCache* pointers must be invalidated in fnInvalidateAllCaches: */

extern OBJID	oGlobalPkgCommonLisp	/* = NULLOBJID */;
extern OBJID	oGlobalSymNil		/* = NULLOBJID */;
extern OBJID	oGlobalSymT		/* = NULLOBJID */;

extern OBJID	oGlobalPkgKeyword	/* = NULLOBJID */;
extern OBJID	oGlobalSymKeywordRead	/* = NULLOBJID */;
extern OBJID	oGlobalSymKeywordWrite	/* = NULLOBJID */;
extern OBJID	oGlobalSymKeywordReadWrite	/* = NULLOBJID */;

#define find_package(/*LPOBJID*/poCache,/*LPCSTR*/pszName) \
(((poCache)!=NULL)? \
 ((boundp(*(poCache)))? \
  *(poCache): \
  (*(poCache)=fnFindPackage(pszName),*(poCache))): \
 fnFindPackage(pszName))

OBJID		fnFindClass	( OBJID		oSymbolClassName );

OBJID		fnFindPackage	( LPCSTR	pszPackageName );

OBJID		fnCreatePackage	( OBJID		oHeap,
				  LPCSTR	pszPackageName );

#define find_symbol(/*LPOBJID*/poCache,/*OBJID*/oPackage,/*LPCSTR*/pszName) \
(((poCache)!=NULL)? \
 ((boundp(*(poCache)))? \
  *(poCache): \
  (*(poCache)=fnFindSymbol(oPackage,pszName),*(poCache))): \
 fnFindSymbol(oPackage,pszName))

OBJID		fnFindSymbol	( OBJID		oPackage,
				  LPCSTR	pszSymbolName );

OBJID		fnCreateSymbol	( OBJID		oHeap,
				  OBJID		oPackage,
				  LPCSTR	pszSymbolName );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
