/* -------------------------------------------------------------------------
| Module	cplob.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB client header file:
|		Macros and functions for usage by the PLOB client
 ------------------------------------------------------------------------- */

#include	"plob.h"

/* -------------------------------------------------------------------------
| Global constants
 ------------------------------------------------------------------------- */

enum {
  /* The block size to use for transferring large memory blocks
     between client and server: */
  nTransferBlockSizeInBytes	= 63 * 1024,

  nTransferBlockSizeInWords	= nTransferBlockSizeInBytes / sizeof ( psint )
};

/* ----------------------------------------------------------------------- */
extern const char	szExpectedAtIndex []	/* = */
/* "Expected object of class %s at\n"
   "       index %d in #<object short-objid=%u>; received tag %d" */;

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
#define	_SVREF_OBJID( oObjId, nRawIndex, eshTypeTag )	\
*fnObjId2ObjPtr((oObjId),(nRawIndex),(eshTypeTag),	\
		__szFile__,__szProc__,__LINE__)

#define	_VALUE_PTR( Type, oObjId, eshTypeTag )	\
((Type*)fnObjId2ValPtr((oObjId),(eshTypeTag),	\
		       __szFile__,__szProc__,__LINE__))

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
int DLLEXPORT		fnStartRemoteServer	( CONST_STRING	szURL,
						  GETACTION	eAction );
SHORTOBJID		fnOpen			( CONST_STRING	szURL,
						  GETACTION	eAction,
						  CONST_STRING
						  szDescription );
void			fnInvalidatePlobCache		( void );

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
OBJID FAR *	fnObjId2ObjPtr		( OBJID oObjId,
					  psint nRawIndex,
					  SHTYPETAG nTypeTag,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine );
LPVOID		fnObjId2ValPtr		( OBJID oObjId,
					  SHTYPETAG nTypeTag,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
