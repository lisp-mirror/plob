/* -------------------------------------------------------------------------
| Module	plobintern.h
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		17.12.93
| Description	Contains the 'secret' constants, types & functions necessary
|		to implement PLOB built-in types.
|
| Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
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
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| ObjIds
| Range checking, short (29 bit) objid <-> long (32 bit) objid conversion
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR OBJID		__oObjId__;

#define	ObjId_is_valid( oObjId )		\
((BOOL)((oGlobalMinObjId<=(oObjId))&&((oObjId)<=oGlobalMaxObjId)))

#define	ASSERT_ObjId_is_valid( oObjId )		\
((ObjId_is_valid(oObjId))?1:			\
 (ERROR((szInvalidObjId,			\
	 LONG2SHORTOBJID(oObjId),		\
	 LONG2SHORTOBJID(oGlobalMinObjId),	\
	 LONG2SHORTOBJID(oGlobalMaxObjId))),0))

/* Change a short objid (29 bit) to a long objid (32 bit): */
/* Version without any checking: */
#define	SHORT2LONGOBJID( oShortObjId )		\
(((unsigned int)(oShortObjId))<<nTagBits)

/* Version with checking: */
#define	Short2LongObjId( oShortObjId )		\
(__oObjId__=SHORT2LONGOBJID(oShortObjId),	\
 (ObjId_is_valid(__oObjId__)?			\
  __oObjId__:					\
  (ERROR((szInvalidObjId,(oShortObjId),		\
	  LONG2SHORTOBJID(oGlobalMinObjId),	\
	  LONG2SHORTOBJID(oGlobalMaxObjId))),-1)))

/* Change a long objid (32 bit) to a short objid (29 bit): */
/* Version without any checking: */
#define	LONG2SHORTOBJID( oObjId )		\
(((unsigned int)(oObjId))>>nTagBits)

/* Version with checking: */
#define	Long2ShortObjId( oObjId )		\
(__oObjId__=oObjId,				\
 (ObjId_is_valid(__oObjId__)?			\
  LONG2SHORTOBJID(__oObjId__):			\
  (ERROR((szInvalidObjId,			\
	  LONG2SHORTOBJID(__oObjId__),		\
	  LONG2SHORTOBJID(oGlobalMinObjId),	\
	  LONG2SHORTOBJID(oGlobalMaxObjId))),	\
   (unsigned int)-1)))

OBJID DLLEXPORT 	fnImmediate2ObjId	( FIXNUM nImmediate,
						  SHTYPETAG *pnTypeTag );
FIXNUM DLLEXPORT	fnObjId2Immediate	( OBJID oObjId,
						  SHTYPETAG nTypeTag );
						  
/* -------------------------------------------------------------------------
| ObjId <-> type tags
 ------------------------------------------------------------------------- */
enum {
  /* Number of bits to be used for type tags: */
  nTypeTagBits		= 16,
  nTypeTagMask		= ( 1 << nTypeTagBits ) - 1
};

/* -------------------------------------------------------------------------
| ObjId <-> fixnum
 ------------------------------------------------------------------------- */
enum {
  nFixnumBitOffset	= NFIXNUMBITOFFSET,
  /* 1998/04/08 HK: Moved to plob.h:
    nFixnumBits		=
      sizeof ( psint ) * nBitsPerByte - nFixnumBitOffset,
  */
  nFixnumTagMask	= ( 1 << nFixnumBitOffset ) - 1
};

/* -------------------------------------------------------------------------
| ObjId <-> Flags
 ------------------------------------------------------------------------- */
enum {
  nFlagBitOffset	= nTypeTagBits,
  nFlagRawBitOffset	= nFlagBitOffset + nTagBits,
  nFlagBits		= sizeof ( psint ) * nBitsPerByte - nFlagBitOffset,
  nFlagMask		= ( 1 << nFlagBits ) - 1
};

/* -------------------------------------------------------------------------
| Module initialization
------------------------------------------------------------------------- */
#define	INITIALIZE_FUNCTIONS	\
{ fnInitCommonMiscModule,	\
  fnInitCommonFfModule,		\
  fnInitCommonTypeModule,	\
  fnInitCommonNumberModule,	\
  fnInitCommonPlobModule,	\
  fnInitCommonSequModule,	\
  fnInitCommonStructModule,	\
  fnInitCommonCLOSModule,	\
  fnInitCommonLockModule,	\
  fnInitCommonHeapModule,	\
  fnInitCommonRootModule,	\
  fnInitCommonBTreeModule,	\
  fnInitCommonAdminModule,	\
  fnInitializeMiscModule,	\
  fnInitializeFfModule,		\
  fnInitializeTypeModule,	\
  fnInitializeNumberModule,	\
  fnInitializePlobModule,	\
  fnInitializeSequModule,	\
  fnInitializeStructModule,	\
  fnInitializeCLOSModule,	\
  fnInitializeLockModule,	\
  fnInitializeHeapModule,	\
  fnInitializeRootModule,	\
  fnInitializeBTreeModule,	\
  fnInitializeAdminModule }

#define	Offset_matches_Index( StructType, StructComponent, nRawIndex ) \
(OFFSET(StructType,StructComponent)/sizeof(psint)==(nRawIndex))

#define	DEINITIALIZE_FUNCTIONS	\
{ fnDeinitializeAdminModule,	\
  fnDeinitializeBTreeModule,	\
  fnDeinitializeRootModule,	\
  fnDeinitializeHeapModule,	\
  fnDeinitializeLockModule,	\
  fnDeinitializeCLOSModule,	\
  fnDeinitializeStructModule,	\
  fnDeinitializeSequModule,	\
  fnDeinitializePlobModule,	\
  fnDeinitializeNumberModule,	\
  fnDeinitializeTypeModule,	\
  fnDeinitializeFfModule,	\
  fnDeinitializeMiscModule,	\
  fnDeinitCommonAdminModule,	\
  fnDeinitCommonBTreeModule,	\
  fnDeinitCommonRootModule,	\
  fnDeinitCommonHeapModule,	\
  fnDeinitCommonLockModule,	\
  fnDeinitCommonCLOSModule,	\
  fnDeinitCommonStructModule,	\
  fnDeinitCommonSequModule,	\
  fnDeinitCommonPlobModule,	\
  fnDeinitCommonNumberModule,	\
  fnDeinitCommonTypeModule,	\
  fnDeinitCommonFfModule,	\
  fnDeinitCommonMiscModule }

/* -------------------------------------------------------------------------
| Stable Heap administration
 ------------------------------------------------------------------------- */

/* Directory with currently opened stable heap: */
extern DLLEXPORTVAR char	szGlobalDirectory	[ MAX_FNAME ];

extern DLLEXPORTVAR OBJID	oGlobalMinObjId		/* = NULLOBJID */;
extern DLLEXPORTVAR OBJID	oGlobalMaxObjId		/* = NULLOBJID - 1 */;
extern DLLEXPORTVAR OBJID	oGlobalLastObjId	/* = NULLOBJID */;

extern int	nGlobalCallCallbacks	/* = 0 */;

/* Check if stable heap is open: */
#define		StableHeap_is_open	\
((BOOL)(oGlobalMinObjId<=oGlobalMaxObjId))

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR const char	szInvalidObjId []	/* =
"Objid %d is out of allowed range %d .. %d." */;

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
void DLLEXPORT		fnInvalidateAllCaches	( void );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
