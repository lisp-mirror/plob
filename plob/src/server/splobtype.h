/* -------------------------------------------------------------------------
| Module	splobtype.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
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

#include	"plobtype.h"

/* -------------------------------------------------------------------------
| Type tag handling
 ------------------------------------------------------------------------- */
/* Multi threading restriction: */
extern OBJID		__oTypeTagOf__		/* = NULLOBJID */;
extern OBJID		oTypeTagCache		/* = NULLOBJID */;
extern SHTYPETAG	nTypeTagCache		/* = NULLTYPETAG */;

#define	typetagof( oSelf )	(__oTypeTagOf__=(oSelf),		\
				 ((oTypeTagCache!=NULLOBJID)&&		\
				  (__oTypeTagOf__==oTypeTagCache))?	\
			         nTypeTagCache:				\
				 fnTypeTagOf(__oTypeTagOf__))

SHTYPETAG DLLEXPORT	fnTypeTagOf		( OBJID		oObjId );
void DLLEXPORT		fnUnexpectedTypeTag	( LPCSTR lpszFile,
						  LPCSTR lpszProc,
						  int nLine,
						  OBJID oObjId,
						  psint nIndex,
						  SHTYPETAG nTypeTagExpected );

#define	UNEXPECTED_TYPE_TAG( oObjId, nIndex, nTypeTagExpected )	\
  fnUnexpectedTypeTag(__szFile__,__szProc__,__LINE__,		\
		      (oObjId),(nIndex),(SHTYPETAG)(nTypeTagExpected) )

#define	ASSERT_TYPE( oObjId, lpSelf, nTypeTag )			\
((ObjId2TypeTag(((psint FAR *)(lpSelf))[eshSHvectorIdxTypeTag])==	\
  (nTypeTag))?								\
 TRUE:									\
 (UNEXPECTED_TYPE_TAG((oObjId),-1,(nTypeTag)),FALSE))

/* Add all built in classes to class table: */
int		fnTypeAddBuiltInClasses	( OBJID	oHeap );

/* -------------------------------------------------------------------------
| Quoting a string
 ------------------------------------------------------------------------- */
LPSTR			fnBarifyString	( LPSTR lpszToBarify,
					  int nBarify );

/* -------------------------------------------------------------------------
| Comparing of object names
 ------------------------------------------------------------------------- */
COMPARETAG DLLEXPORT	fnNameCompare	( LPCSTR	lpsSelf,
					  int		nSelf,
					  OBJID		oCompare,
					  BOOL		bIgnoreCase );

/* Standard object initialization method. fnInitStandard is a
   standard initialization method suitable for most objects: */
BOOL		mfnInitStandard	( OBJID oObjId, psint FAR * lpSHvector,
				  LPCLASSINFO lpClassInfo );

/* Sample prototype for fnInit...-functions:
static BOOL	mfnInit	( OBJID oObjId, psint FAR * lpSHvector,
				  LPCLASSINFO lpClassInfo );
*/

/* -------------------------------------------------------------------------
| Object 'pretty printing'
 ------------------------------------------------------------------------- */
#define		PrintObject( oSelf, szBuffer ) \
fnPrintObject ( oSelf, szBuffer, sizeof ( szBuffer ) )
LPSTR DLLEXPORT	fnPrintObject		( OBJID oSelf,
					  LPSTR lpszBuffer,
					  size_t nBuffer );

/* -------------------------------------------------------------------------
| Symbol name 'pretty printing'
 ------------------------------------------------------------------------- */
LPSTR DLLEXPORT	fnPrintSymbol		( OBJID oSymbol,
					  LPSTR lpszBuffer,
					  size_t nBuffer );

/* -------------------------------------------------------------------------
| Generic functions
 ------------------------------------------------------------------------- */

/* Return the number of elements on value section. */
FIXNUM DLLEXPORT	gfnValues	( OBJID		oSelf );

/* Standard method for gfnValues. */
FIXNUM			mfnStandardValues	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );

/* Return the element type of the value section. */
SHTYPETAG DLLEXPORT	gfnValueTypeTag	( OBJID		oSelf );

/* Standard method for gfnValueTypeTag. */
SHTYPETAG		mfnStandardValueTypeTag	( OBJID		oSelf,
						  LPCLASSINFO	pClassInfo );

BOOL DLLEXPORT	gfnInitializeInstance	( OBJID oSelf );

/* Object count function; should return the number of objects
   contained in oSelf: */
unsigned int DLLEXPORT	gfnCount	( OBJID oSelf );


/* Is called when an object is to be destroyed; should return TRUE if the
   object has no reasons against being destroyed: */
BOOL DLLEXPORT	gfnDestroy		( OBJID oSelf,
					  BOOL bKill );

/* Object compare function: */
COMPARETAG DLLEXPORT	gfnCompare	( LPVOID	pSelf,
					  SHTYPETAG	eTypeTagSelf,
					  OBJID		oCompare );

/* Should compare in the sense of EQUAL: */
BOOL DLLEXPORT		gfnEqual	( OBJID oSelf,
					  OBJID oCompare );

/* Flush an object: */
void DLLEXPORT		gfnFlush	( OBJID oSelf );

/* Get the name of an object. If it has one at all, a pointer to a
   (not necessary 0-terminated) string is returned; the length of the
   name string is written to *lpnName. If the object has no name at all,
   NULL is returned and *lpnName keeps unchanged: */
LPSTR DLLEXPORT		gfnNameOf	( OBJID oSelf,
					  LPINT lpnName );

/* -------------------------------------------------------------------------
| Notifiers called in conjunction with object locking:
 ------------------------------------------------------------------------- */
/* Someone acquired a lock for oSelf on object oLocked; the type of
   lock is given by nLockNew, the old lock status resp. to oSelf is in
   nLockOld.  oSelf should return NULL if it has no reasons against
   locking oLocked; otherwise it should return a static string message
   describing the reason why the object could not be locked. In this
   case, oLocked will be unlocked: */
LPCSTR DLLEXPORT	gfnLockAcquired	( OBJID oSelf,
					  OBJID oLocked,
					  SHLOCK nLockOld,
					  int nLockCount,
					  SHLOCK nLockNew );
/* Someome released a lock for oSelf on object oLocked; the old lock
   status is in nLockOld, the new lock status in nLockNew: */
void DLLEXPORT		gfnLockReleased	( OBJID oSelf,
					  OBJID oLocked,
					  SHLOCK nLockOld,
					  int nLockCount,
					  SHLOCK nLockNew );
/* The state of the object oLocked locked by oSelf changed; the change
   to the sh-vector is only done if the method returns TRUE: */
BOOL DLLEXPORT		gfnObjectStateChanged	( OBJID oSelf,
						  OBJID oLocked );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
