/* -------------------------------------------------------------------------
| Module	splobmisc.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94 Derived from c-plob.c
| Description	
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

#include	<stdarg.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobff.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobtype.h"
#include	"splobnumber.h"
#include	"splobstruct.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
#define	LOGGING	0x00	/* 0 (no), 4 (peek) (messages) */

/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */
FLUSHMODE	nFlushModeCache			= flushGet;
ERRLVL		__nErrorLevel__			= errLvl0;
LPSTR		__lpszErrorMessage__		= (LPSTR) NULL;

/* ----------------------------------------------------------------------- */
static const char szInvalidPeekHandle []	= "Invalid peek handle %d";

/* ----------------------------------------------------------------------- */
static HPEEK		hPeekCounter		= NULLHPEEK + 1;
static LPHASHTABLE	pPeekHashTable		= NULL;

/* ----------------------------------------------------------------------- */
static LPSTR	fnServerMkLogPrompt		( LPSTR		lpszBuffer,
						  size_t	nBuffer );

/* -------------------------------------------------------------------------
| Error handling and LISP callbacks
 ------------------------------------------------------------------------- */
static int	fnErrorHandler			( ERRORLEVEL	eLevel,
						  LPCSTR	pszContinue,
						  LPCSTR	pszErrorMsg );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitializeMiscModule		( void )
{
  PROCEDURE	( fnInitializeMiscModule );

  fnSetFnMkLogPrompt ( fnServerMkLogPrompt );
  fnGlobalSetErrorHandler ( fnErrorHandler );

  pPeekHashTable	= HashCreate ( NULL, 16, sizeof ( PEEKHANDLE ) );

  RETURN ( VOID );
} /* fnInitializeMiscModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitializeMiscModule	( void )
{
  PROCEDURE	( fnDeinitializeMiscModule );

  pPeekHashTable	= fnHashDestroy ( pPeekHashTable );

  RETURN ( VOID );
} /* fnDeinitializeMiscModule */

/* ----------------------------------------------------------------------- */
static LPSTR	fnServerMkLogPrompt		( LPSTR		lpszBuffer,
						  size_t	nBuffer )
{
  PROCEDURE	( fnServerMkLogPrompt );

  if ( lpszBuffer != NULL ) {
    *lpszBuffer = '\0';
    if ( boundp ( oGlobalSession ) ) {
      if ( ObjId_is_valid ( oGlobalSession ) ) {
	fnPrintObject ( oGlobalSession, lpszBuffer, nBuffer );
      } else {
	sprintf ( lpszBuffer,
		  UNREADABLE_OBJECT_PREFIX 
		  "invalid session short-objid=%d"
		  UNREADABLE_OBJECT_SUFFIX,
		  LONG2SHORTOBJID ( oGlobalSession ) );
      }
    }
  }

  RETURN ( lpszBuffer );
} /* fnServerMkLogPrompt */

/* ----------------------------------------------------------------------- */
static int	fnErrorHandler			( ERRORLEVEL	eLevel,
						  LPCSTR	pszContinue,
						  LPCSTR	pszErrorMsg )
{
  PROCEDURE	( fnErrorHandler );

  fnLISPerrorCallback ( eLevel, pszContinue, pszErrorMsg );

  RETURN ( 0 );
} /* fnErrorHandler */

/* ----------------------------------------------------------------------- */
void		fnStoreError			( ERRLVL nErrorLevel,
						  LPCSTR lpszErrorMsg )
{
  char		szBuffer [ 80 ];

  PROCEDURE	( fnStoreError );
  INITIALIZEPLOB;

  __nErrorLevel__	= nErrorLevel;
  if ( __lpszErrorMessage__ ) {
    free ( __lpszErrorMessage__ );
    __lpszErrorMessage__	= (LPSTR) NULL;
  }
  if ( nErrorLevel > errLvl0 ) {
    if ( lpszErrorMsg ) {
      __lpszErrorMessage__	= strdup ( lpszErrorMsg );
    } else {
      sprintf ( szBuffer, "Unknown error with level %d.", nErrorLevel );
      __lpszErrorMessage__	= strdup ( szBuffer );
    }
  }
  RETURN ( VOID );
} /* fnStoreError */

/* ----------------------------------------------------------------------- */
BOOL		fnStoreErrorIf			( ERRLVL nErrorLevel,
						  LPCSTR lpszErrorMsg )
{
  BOOL	bStored;

  PROCEDURE	( fnStoreErrorIf );
  INITIALIZEPLOB;

  bStored	= (BOOL) ( __nErrorLevel__ < nErrorLevel );
  if ( bStored ) {
    fnStoreError ( nErrorLevel, lpszErrorMsg );
  }

  RETURN ( bStored );
} /* fnStoreErrorIf */

/* ----------------------------------------------------------------------- */
void		fnPLOBerrorCallback		( LPCSTR lpszErrorMsg )
{
  PROCEDURE	( fnPLOBerrorCallback );

  _ERROR ( "libpostore.a", NULL, -1, ( lpszErrorMsg ) );

  RETURN ( VOID );
} /* fnPLOBerrorCallback */

/* ----------------------------------------------------------------------- */
void		fnPLOBsaveCallback		( void )
{
  PROCEDURE	( fnPLOBsaveCallback );

  RETURN ( VOID );
} /* fnPLOBsaveCallback */

/* ----------------------------------------------------------------------- */
void		fnPLOBrestoreCallback		( void )
{
  PROCEDURE	( fnPLOBrestoreCallback );

  RETURN ( VOID );
} /* fnPLOBrestoreCallback */

/* ----------------------------------------------------------------------- */
void		fnPLOBstabiliseCallback		( void )
{
  PROCEDURE	( fnPLOBstabiliseCallback );

  RETURN ( VOID );
} /* fnPLOBstabiliseCallback */

/* ----------------------------------------------------------------------- */
BOOL		fnPLOBsuspendCallback		( OBJID oLockBy,
						  OBJID oToLock,
						  LPCSTR lpszReason )
{
  SHORTOBJID	oShortLockBy, oShortToLock;

  PROCEDURE	( fnPLOBsuspendCallback );

  oShortLockBy	= Long2ShortObjId ( oLockBy );
  oShortToLock	= Long2ShortObjId ( oToLock );

  RETURN ( fnLISPsuspendCallback ( oShortLockBy, oShortToLock, lpszReason ) );
} /* fnPLOBsuspendCallback */

/* ----------------------------------------------------------------------- */
BOOL		fnPLOBwakeupCallback		( OBJID oLockBy,
						  OBJID oToLock,
						  LPCSTR lpszReason )
{
  SHORTOBJID	oShortLockBy, oShortToLock;

  PROCEDURE	( fnPLOBwakeupCallback );

  oShortLockBy	= Long2ShortObjId ( oLockBy );
  oShortToLock	= Long2ShortObjId ( oToLock );

  RETURN ( fnLISPwakeupCallback ( oShortLockBy, oShortToLock, lpszReason ) );
} /* fnPLOBwakeupCallback */

/* -------------------------------------------------------------------------
| The PEEK administration functions
 ------------------------------------------------------------------------- */
HPEEK		fnPeekHandleCreate		( OBJID	oLocked,
						  SHLOCK nVectorLockNow )
{
  HPEEK		hPeek;
  PEEKHANDLE	PeekHandle;

  PROCEDURE	( fnPeekHandleCreate );
  INITIALIZEPLOB;

  hPeek	= hPeekCounter++;
  memset ( &PeekHandle, 0, sizeof ( PeekHandle ) );
  PeekHandle.oLocked		= oLocked;
  PeekHandle.nVectorLockNow	= nVectorLockNow;
  HashCreate ( &PeekHandle.ObjIds, 16, 0 );
  HashInsert ( pPeekHashTable, hPeek, &PeekHandle );

  RETURN ( hPeek );
} /* fnPeekHandleCreate */

/* ----------------------------------------------------------------------- */
static PPEEKHANDLE fnPeekHandleFree		( HPEEK	hPeek )
{
  PPEEKHANDLE	pPeekHandle;

  PROCEDURE	( fnPeekHandleFree );

  pPeekHandle	= (PPEEKHANDLE) HashGet ( pPeekHashTable, hPeek );
  if ( pPeekHandle != NULL ) {
    fnHashDestroy ( &pPeekHandle->ObjIds );
    memset ( pPeekHandle, 0, sizeof ( *pPeekHandle ) );
  }
  RETURN ( pPeekHandle );
} /* fnPeekHandleFree */

/* ----------------------------------------------------------------------- */
HPEEK		fnPeekHandleDestroy		( HPEEK	hPeek )
{
  PROCEDURE	( fnPeekHandleDestroy );

  INITIALIZEPLOB;

  if ( fnPeekHandleFree ( hPeek ) ) {
    fnHashDelete ( pPeekHashTable, hPeek );
    hPeek	= NULLHPEEK;
  }
  RETURN ( hPeek );
} /* fnPeekHandleDestroy */

/* ----------------------------------------------------------------------- */
int		fnPeekHandleDestroyAll		( void )
{
  int		nDestroyed	= 0;
  HPEEK		hPeek;
  BOOL		bMapped;

  PROCEDURE	( fnPeekHandleDestroyAll );

  INITIALIZEPLOB;

  for ( bMapped = fnHashFirst ( pPeekHashTable, (LPHASHKEY) &hPeek,
				(LPVOID *) NULL, (size_t *) NULL );
	bMapped;
	bMapped = fnHashNext ( pPeekHashTable, (LPHASHKEY) &hPeek,
			       (LPVOID *) NULL, (size_t *) NULL ) ) {
    if ( fnPeekHandleFree ( hPeek ) ) {
      nDestroyed++;
    }
  }
  fnHashClear ( pPeekHashTable );
  
  RETURN ( nDestroyed );
} /* fnPeekHandleDestroyAll */

/* ----------------------------------------------------------------------- */
BOOL		fnPeekHandleInsert		( HPEEK	hPeek,
						  OBJID	oTail )
{
  PPEEKHANDLE	pPeekHandle;
  BOOL		bInserted = FALSE;
  unsigned int	nSlots, nValues;
  LPOBJID	poTail;

  PROCEDURE	( fnPeekHandleInsert );

  INITIALIZEPLOB;

  pPeekHandle	= (PPEEKHANDLE) HashGet ( pPeekHashTable, hPeek );
  if ( pPeekHandle == NULL ) {
    ERROR (( "Invalid peek handle %d on\n"
	     "       inserting %s",
	     hPeek, fnPrintObject ( oTail, (LPSTR) NULL, 0 ) ));
    RETURN ( bInserted );
  }

  ASSERT_ObjId_is_valid ( oTail );
  bInserted	= (BOOL)
    ( HashInsert ( &pPeekHandle->ObjIds, oTail, NULL ) == hashInserted );
  if ( bInserted ) {
    poTail	= SH_key_to_address ( oTail );
    ASSERT ( poTail != NULL );
    nSlots	= poTail [ eshSHvectorIdxObjIds ] - 
      ( eshSHvectorIdxFirstData - eshSHvectorIdxFirstObjId );
    pPeekHandle->nObjIdWords	+= PEEKSLOTSSizeBySlots ( nSlots );
    pPeekHandle->nValues	+= gfnValues ( oTail );
  }

  RETURN ( bInserted );
} /* fnPeekHandleInsert */

/* ----------------------------------------------------------------------- */
PPEEKHANDLE	fnPeekHandlePtr			( HPEEK	hPeek )
{
  PPEEKHANDLE	pPeekHandle;

  PROCEDURE	( fnPeekHandlePtr );

  INITIALIZEPLOB;

  pPeekHandle	= (PPEEKHANDLE) HashGet ( pPeekHashTable, hPeek );

  RETURN ( pPeekHandle );
} /* fnPeekHandlePtr */

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
DEPENDENTMODE DLLEXPORT	fnMakeDependent		( OBJID oDependent,
						  DEPENDENTMODE
						  nDependentMode )
{
  LPOBJID	poDependent;
  DEPENDENTMODE	nOldDependentMode = (DEPENDENTMODE) 0;

  PROCEDURE	( fnMakeDependent );

  INITIALIZEPLOB;

  if ( ! boundp ( oDependent ) || immediatep ( oDependent ) ) {
    RETURN ( (DEPENDENTMODE) 0 );
  }

  ASSERT_ObjId_is_valid ( oDependent );
  poDependent	= SH_key_to_address ( oDependent );
  ASSERT ( poDependent != NULL );
  if ( GETFLAGBIT ( poDependent [ eshSHvectorIdxTypeTag ],
		    flagDependentRead ) ) {
    nOldDependentMode	= (DEPENDENTMODE)
      ( (unsigned int) nOldDependentMode | (unsigned int) flagDependentRead );
  }
  if ( GETFLAGBIT ( poDependent [ eshSHvectorIdxTypeTag ],
		    flagDependentWrite ) ) {
    nOldDependentMode	= (DEPENDENTMODE)
      ( (unsigned int) nOldDependentMode | (unsigned int) flagDependentWrite );
  }
  switch ( nDependentMode ) {
  case flagDependentNone:
  case flagDependentRead:	case flagDependentWrite:
  case flagDependentReadWrite:
    AtomicLock ( oDependent, oDependent );
    SETFLAGBIT ( poDependent [ eshSHvectorIdxTypeTag ], flagDependentRead,
		 ( nDependentMode & flagDependentRead ) );
    SETFLAGBIT ( poDependent [ eshSHvectorIdxTypeTag ], flagDependentWrite,
		 ( nDependentMode & flagDependentWrite ) );
    AtomicUnlock ( oDependent, oDependent );
    break;
  default:
    break;
  }

  RETURN ( nOldDependentMode );
} /* fnMakeDependent */

/* ----------------------------------------------------------------------- */
DEPENDENTMODE DLLEXPORT	fnMakeDependentFromSymbol( OBJID	oDependent,
						   OBJID	oSymbol )
{
  DEPENDENTMODE	nDependentMode = flagDependentNone;

  PROCEDURE	( fnMakeDependentFromSymbol );

  INITIALIZEPLOB;

  if ( boundp ( oSymbol ) ) {
    /* Find the symbols needed here: */
    find_symbol ( &oGlobalSymKeywordRead,
		  find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		  szREAD );
    find_symbol ( &oGlobalSymKeywordWrite,
		  find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		  szWRITE );
    find_symbol ( &oGlobalSymKeywordReadWrite,
		  find_package ( &oGlobalPkgKeyword, szKEYWORD ),
		  szREADWRITE );
    if ( oSymbol == oGlobalSymKeywordRead ) {
      nDependentMode	= (DEPENDENTMODE)
	( (unsigned int) nDependentMode | (unsigned int) flagDependentRead );
    } else if ( oSymbol == oGlobalSymKeywordWrite ) {
      nDependentMode	= (DEPENDENTMODE)
	( (unsigned int) nDependentMode | (unsigned int) flagDependentWrite );
    } else if ( oSymbol == oGlobalSymKeywordReadWrite ) {
      nDependentMode	= (DEPENDENTMODE)
	( (unsigned int) nDependentMode |
	  (unsigned int) flagDependentReadWrite );
    }
    if ( nDependentMode != flagDependentNone ) {
      RETURN ( makdependent ( oDependent, nDependentMode ) );
    }
  }
  RETURN ( nDependentMode );
} /* fnMakeDependentFromSymbol */

/* ----------------------------------------------------------------------- */
DEPENDENTMODE DLLEXPORT	fnDependentP		( OBJID oDependentP )
{
  LPOBJID	poDependentP;
  DEPENDENTMODE	nDependentMode = flagDependentNone;

  PROCEDURE	( fnDependentP );

  INITIALIZEPLOB;

  if ( ! boundp ( oDependentP ) || immediatep ( oDependentP ) ) {
    RETURN ( (DEPENDENTMODE) 0 );
  }

  if ( ! ObjId_is_valid ( oDependentP ) ) {
    ERROR (( "Invalid long objid %d (%s), allowed range %d .. %d\n"
	     "       encountered on oDependentP",
	     oDependentP, fnPrintObject ( oDependentP, (LPSTR) NULL, 0 ),
	     LONG2SHORTOBJID ( oGlobalMinObjId ),
	     LONG2SHORTOBJID ( oGlobalMaxObjId ) ));
    RETURN ( (DEPENDENTMODE) 0 );
  }
  poDependentP	= SH_key_to_address ( oDependentP );
  ASSERT ( poDependentP != NULL );
  if ( GETFLAGBIT ( poDependentP [ eshSHvectorIdxTypeTag ],
		    flagDependentRead ) ) {
    nDependentMode	= (DEPENDENTMODE)
      ( (unsigned int) nDependentMode | (unsigned int) flagDependentRead );
  }
  if ( GETFLAGBIT ( poDependentP [ eshSHvectorIdxTypeTag ],
		    flagDependentWrite ) ) {
    nDependentMode	= (DEPENDENTMODE)
      ( (unsigned int) nDependentMode | (unsigned int) flagDependentWrite );
  }

  RETURN ( nDependentMode );
} /* fnDependentP */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */

BeginFunction ( DEPENDENTMODE,
		fnShortMakeDependent, "c-sh-dependent",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortSelf )
		  and
		  argument ( DEPENDENTMODE, value_in, nDependentMode ) ) )
{
  DEPENDENTMODE	eMode;
  OBJID		oSelf;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( (DEPENDENTMODE) eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (DEPENDENTMODE) eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oSelf	= Short2LongObjId ( oShortSelf );
  eMode	= fnMakeDependent ( oSelf, nDependentMode );

  UnstoreSession ();
  RETURN ( eMode );
} EndFunction ( fnShortMakeDependent );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerObjectPeekSlots, "c-sh-peek-object-slots",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( FIXNUM, value_in, hPeek )
		  and
		  argument ( FIXNUM, value_in, nWords )
		  and
		  argument ( VECTOR ( u_int, nWords ),
			     vector_out, pBuffer ) ) )
{
  PPEEKHANDLE	pPeekHandle;
  SHLOCK	nVectorLockNow;
  OBJID		oPeeked, oPeekedSlot;
  LPOBJID	poPeeked;
  BOOL		bMapped;
  u_int		i, s, nIncr = 0, nSlots;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  pPeekHandle	= fnPeekHandlePtr ( hPeek );
  if ( pPeekHandle == NULL ) {
    ERROR (( szInvalidPeekHandle, hPeek ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

#if (LOGGING+0) & 0x04
  if ( GetFlagWord() & 0x04 ) {
    fprintf ( stderr, "%s(%d): %s: Handle %d, words %d\n",
	      __szFile__, __LINE__, __szProc__,
	      hPeek, nWords );
  }
#endif /* #if (LOGGING+0) & 0x04 */

  /* Put all objects to peek into the output buffer: */
  for ( i = 0,
	  bMapped = fnHashFirst ( &pPeekHandle->ObjIds, (LPHASHKEY) &oPeeked,
				  (LPVOID *) NULL, (size_t *) NULL );
	bMapped;
	i += nIncr,
	  bMapped = fnHashNext ( &pPeekHandle->ObjIds, (LPHASHKEY) &oPeeked,
				 (LPVOID *) NULL, (size_t *) NULL ) ) {
    ASSERT ( i < pPeekHandle->nObjIdWords );
    ASSERT_ObjId_is_valid ( oPeeked );
    poPeeked	= SH_key_to_address ( oPeeked );
    ASSERT ( poPeeked != NULL );
    nSlots	= poPeeked [ eshSHvectorIdxObjIds ] - 
      ( eshSHvectorIdxFirstData - eshSHvectorIdxFirstObjId );
    PEEKSLOTSSELF ( pBuffer + i )	= oPeeked;
    PEEKSLOTSTYPETAG ( pBuffer + i )	= typetagof ( oPeeked );
    PEEKSLOTSSLOTS ( pBuffer + i )	= nSlots;
    for ( s = 0; s < nSlots; s++ ) {
      oPeekedSlot	= poPeeked [ eshSHvectorIdxFirstData + s ];
      PEEKSLOTSSLOTOBJID ( pBuffer + i, s )	= oPeekedSlot;
      PEEKSLOTSSLOTTYPETAG ( pBuffer + i, s )	= typetagof ( oPeekedSlot );
    }
    PEEKSLOTSTYPETAGVALUES ( pBuffer + i )	= gfnValueTypeTag ( oPeeked );
    PEEKSLOTSVALUES ( pBuffer + i )		= gfnValues ( oPeeked );
    nIncr			= PEEKSLOTSSIZE ( pBuffer + i );
    pPeekHandle->nCopiedOut	+= nIncr;
  }
  ASSERT ( i <= nWords );

  nVectorLockNow	= pPeekHandle->nVectorLockNow;
  if ( pPeekHandle->nCopiedOut >=
       pPeekHandle->nObjIdWords + pPeekHandle->nValues ) {
    fnPeekHandleDestroy ( hPeek );
    pPeekHandle		= (PPEEKHANDLE) NULL;
  }

  UnstoreSession ();
  RETURN ( nVectorLockNow );
} EndFunction ( fnServerObjectPeekSlots );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
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
			     vector_out, pBuffer ) ) )
{
  PPEEKHANDLE	pPeekHandle;
  SHLOCK	nVectorLockNow;
  OBJID		oPeeked;
  LPOBJID	poPeeked;
  BOOL		bMapped;
  u_int		b, i, bIncr = 0, iIncr = 0;
  SHTYPETAG	nTypeTagValues;
  u_int		nSlots, nValues, nSizeInWords;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  /* Copy out dummy parameters for usage in plobpxdr.c: */
  if ( pnElementTypeTagsOut != NULL && nObjects > 0 ) {
    memcpy ( pnElementTypeTagsOut, pnElementTypeTagsIn,
	     nObjects * sizeof ( *pnElementTypeTagsOut ) );
  }
  if ( pnSizesInElementsOut != NULL && nObjects > 0 ) {
    memcpy ( pnSizesInElementsOut, pnSizesInElementsIn,
	     nObjects * sizeof ( *pnSizesInElementsOut ) );
  }

  pPeekHandle	= fnPeekHandlePtr ( hPeek );
  if ( pPeekHandle == NULL ) {
    ERROR (( szInvalidPeekHandle, hPeek ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

#if (LOGGING+0) & 0x04
  if ( GetFlagWord() & 0x04 ) {
    fprintf ( stderr, "%s(%d): %s: Handle %d, words %d\n",
	      __szFile__, __LINE__, __szProc__,
	      hPeek, nWords );
  }
#endif /* #if (LOGGING+0) & 0x04 */

  /* Put all objects to peek into the output buffer: */
  for ( b = 0, i = 0,
	  bMapped = fnHashFirst ( &pPeekHandle->ObjIds, (LPHASHKEY) &oPeeked,
				  (LPVOID *) NULL, (size_t *) NULL );
	bMapped;
	b += bIncr, i += iIncr,
	  bMapped = fnHashNext ( &pPeekHandle->ObjIds, (LPHASHKEY) &oPeeked,
				 (LPVOID *) NULL, (size_t *) NULL ) ) {
    bIncr	= 0;
    iIncr	= 0;
    nValues	= gfnValues ( oPeeked );
    if ( nValues > 0 ) {
      nTypeTagValues	= gfnValueTypeTag ( oPeeked );
      if ( nTypeTagValues != NULLTYPETAG ) {
	nSizeInWords	= fnTypeTagSizeValue ( 1, &nTypeTagValues, &nValues );
#if (LOGGING+0) & 0x04
	if ( GetFlagWord() & 0x04 ) {
	  char	szTypeTag1 [ 256 ], szTypeTag2 [ 256 ];
	  fprintf ( stderr, "%s(%d): %s:\n"
		    "\toPeeked = %d,\n"
		    "\ttype tag values %s (0x%X)\n"
		    "\tsize in words %d\n"
		    "\tnumber of values %d\n",
		    __szFile__, __LINE__, __szProc__,
		    LONG2SHORTOBJID ( oPeeked ),
		    PrintObject ( TypeTag2ObjId ( nTypeTagValues ),
				  szTypeTag1 ),
		    nTypeTagValues,
		    nSizeInWords,
		    nValues );
	}
#endif /* #if (LOGGING+0) & 0x04 */
	poPeeked	= SH_key_to_address ( oPeeked );
	ASSERT ( poPeeked != NULL );
	nSlots		= poPeeked [ eshSHvectorIdxObjIds ] - 
	  ( eshSHvectorIdxFirstData - eshSHvectorIdxFirstObjId );
	ASSERT ( nSizeInWords > 0 );
	memcpy ( (u_int*) pBuffer + b,
		 & poPeeked [ eshSHvectorIdxFirstData + nSlots ],
		 nSizeInWords * nSizeOfPostoreWord );
	bIncr			= nSizeInWords;
	iIncr			= 1;
	pPeekHandle->nCopiedOut	+= nValues;
      }
    }
  }

  nVectorLockNow	= pPeekHandle->nVectorLockNow;
  if ( pPeekHandle->nCopiedOut >=
       pPeekHandle->nObjIdWords + pPeekHandle->nValues ) {
    fnPeekHandleDestroy ( hPeek );
    pPeekHandle		= (PPEEKHANDLE) NULL;
  }

  UnstoreSession ();
  RETURN ( nVectorLockNow );
} EndFunction ( fnServerObjectPeekValues );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
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
			      vector_in, pValues ) ) )
{
  OBJID			oObjId, oHeap;
  register LPOBJID	lpSHvector;
  FIXNUM		nSelfSlots, i, nSizeInWords, nValues;
  BOOL			bChanged = FALSE;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( VOID );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( VOID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oObjId	= Short2LongObjId ( oShortObjId );
  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  lpSHvector	= SH_key_to_address ( oObjId );
  ASSERT ( lpSHvector != NULL );

  nSelfSlots	= lpSHvector [ eshSHvectorIdxObjIds ] +
    eshSHvectorIdxFirstObjId - eshSHvectorIdxFirstData;
  nSlots	= MIN ( nSlots, nSelfSlots );
  if ( nSlots > 0 ) {
    for ( i = 0; i < nSlots; i++ ) {
      if ( boundp ( pSlots [ i ] ) && ! immediatep ( pSlots [ i ] ) &&
	   ! ObjId_is_valid ( pSlots [ i ] ) ) {
	char	szSlot [ 256 ], szObject [ 256 ];
	ERROR (( "Encountered invalid objid %d (%s),\n"
		 "       allowed range %d .. %d\n"
		 "       in %s\n"
		 "       at slot location %d",
		 LONG2SHORTOBJID ( pSlots [ i ] ),
		 PrintObject ( pSlots [ i ], szSlot ),
		 LONG2SHORTOBJID ( oGlobalMinObjId ),
		 LONG2SHORTOBJID ( oGlobalMaxObjId ),
		 PrintObject ( oObjId, szObject ),
		 i ));
	UnstoreSession ();
	RETURN ( VOID );
      }
    }
    if ( memcmp ( & lpSHvector [ eshSHvectorIdxFirstData ],
		  pSlots, nSlots * sizeof ( psint ) ) != 0 &&
	 ( bChanged = gfnObjectStateChanged ( oHeap, oObjId ) ) ) {
      memcpy ( & lpSHvector [ eshSHvectorIdxFirstData ], pSlots,
	       nSlots * nSizeOfPostoreWord );
      nGlobalTouched++;
    }
  }

  nValues	= lpSHvector [ eshSHvectorIdxSize ] -
    lpSHvector [ eshSHvectorIdxObjIds ] - eshSHvectorIdxFirstObjId;
  nSizeInWords	= fnTypeTagSizeValue ( 1, &nElementTypeTag, &nSizeInElements );
  nValues	= MIN ( nValues, nSizeInWords );
  /* 1996/11/11 HK: Debug: */
#if (LOGGING+0) & 0x04
  if ( GetFlagWord() & 0x04 ) {
    fprintf ( stderr, "%s(%d): %s:\n"
	      "\tPoking objid %d, %d slots,\n"
	      "\ttype tag values 0x%X, %d values, %d words,\n"
	      "\tcopy %d words\n",
	      __szFile__, __LINE__, __szProc__,
	      oShortObjId, nSlots,
	      nElementTypeTag, nSizeInElements, nSizeInWords,
	      nValues );
    if ( nElementTypeTag == 0x58 ) {
      fprintf ( stderr, "\tdouble value %g\n",
		* (double*) pValues );
    }
  }
#endif /* #if (LOGGING+0) & 0x04 */
  if ( nValues > 0 ) {
    nSelfSlots	= lpSHvector [ eshSHvectorIdxObjIds ];
    if ( memcmp ( & lpSHvector [ nSelfSlots + eshSHvectorIdxFirstObjId ],
		  pValues, nValues * sizeof ( psint ) ) != 0 &&
	 ( bChanged ||
	   ( bChanged = gfnObjectStateChanged ( oHeap, oObjId ) ) ) ) {
      memcpy ( & lpSHvector [ nSelfSlots + eshSHvectorIdxFirstObjId ],
	       pValues, nValues * nSizeOfPostoreWord );
      nGlobalTouched++;
    }
  }

  UnstoreSession ();
  RETURN ( VOID );
} EndFunction ( fnServerObjectPoke );

/* ----------------------------------------------------------------------- */
BeginFunction ( FLUSHMODE,
	        fnFlushMode, "c-sh-flush-mode",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( FLUSHMODE, value_in, nMode ) ) )
{
  FLUSHMODE	nModeOld;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( (FLUSHMODE) eshGeneralError );
  }
  if ( oGlobalSession == NULLOBJID ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( (FLUSHMODE) eshGeneralError );
    }
  }

  if ( nFlushModeCache == flushGet )
    nFlushModeCache	= flushDefault;
   
  nModeOld	= nFlushModeCache;

  if ( nMode != flushGet ) {
    nFlushModeCache	= nMode;
  }

  UnstoreSession ();
  RETURN ( nModeOld );
} EndFunction ( fnFlushMode );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnFlagWord, "c-sh-flag-word",
	        ( argument ( FLAGMODE, value_in, nGetOrSet )
		  and
		  argument ( FIXNUM, value_in, nFlagWord ) ) )
{
  int	nFlagWordOld;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( 0 );
  }
  if ( oGlobalSession == NULLOBJID ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }

  nFlagWordOld	= nGlobalFlagWord;

  if ( nGetOrSet == flagSet ) {
    nGlobalFlagWord	= nFlagWord;
  }

  UnstoreSession ();
  RETURN ( nFlagWordOld );
} EndFunction ( fnFlagWord );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
