/* -------------------------------------------------------------------------
| Module	splobstruct.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
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
