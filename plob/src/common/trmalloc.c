/* ----------------------------------------------------------------------------
| Module	trmalloc.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1997/06/27
| Description	Traced malloc
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
 --------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"trmalloc.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Static constants, types and variables
 ------------------------------------------------------------------------- */
static const char	szReturnNULL []		=
"Return a NULL pointer.";
static const char	szInvalidPointer []	=
"Invalid pointer 0x%X passed to %s().";

static MALLFLAGS	eGlobalMallFlags	= emFast;
static int		nGlobalAllocated	= 0;
static int		nGlobalMalloc		= 0;
static double		dGlobalMallocAmount	= 0.0;
static int		nGlobalReAlloc		= 0;
static double		dGlobalReAllocAmount	= 0.0;
static double		dGlobalReAllocIncrease	= 0.0;
static int		nGlobalFree		= 0;

typedef struct tagMALLHEADER {
  struct tagMALLHEADER *	pPrevious;
  struct tagMALLHEADER *	pNext;
  size_t			nSize;
  LPCSTR			pszModule;
  LPCSTR			pszProcedure;
  int				nLine;
  time_t			tTime;
  int				nLabel;
}	MALLHEADER, * PMALLHEADER;

static time_t		tGlobalStart;
static PMALLHEADER	pGlobalFirstHeader	= (PMALLHEADER) NULL;

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
static LPVOID	fnMallocChainIn		( PMALLHEADER	pHeader,
					  size_t	nSize,
					  LPCSTR	pszModule,
					  LPCSTR	pszProcedure,
					  int		nLine )
{
  LPVOID	lpData = NULL;

  PROCEDURE	( fnMallocChainIn );

  ASSERT ( pHeader != NULL );

  pHeader->pPrevious	= (struct tagMALLHEADER *) NULL;
  pHeader->pNext	= (struct tagMALLHEADER *) NULL;
  pHeader->nSize	= nSize;
  pHeader->pszModule	= pszModule;
  pHeader->pszProcedure	= pszProcedure;
  pHeader->nLine	= nLine;
  time ( &pHeader->tTime );
  pHeader->nLabel	= nGlobalAllocated;

  if ( pGlobalFirstHeader != NULL ) {
    pHeader->pNext			= pGlobalFirstHeader;
    pGlobalFirstHeader->pPrevious	= pHeader;
  }
  pGlobalFirstHeader	= pHeader;

  lpData		= (LPVOID)
    ( ( (LPBYTE) pHeader ) + sizeof ( *pHeader ) );

  RETURN ( lpData );
} /* fnMallocChainIn */

/* ----------------------------------------------------------------------- */
static BOOL	fnMallocChainOut	( PMALLHEADER	pHeader )
{
  BOOL		bDone	= FALSE;

  PROCEDURE	( fnMallocChainOut );

  ASSERT ( pHeader != NULL );

  if ( pHeader == pGlobalFirstHeader ) {
    /* Consistency check on the double-linked list: */
    ASSERT ( pHeader->pPrevious == NULL );
    /* Chain out the very first header: */
    pGlobalFirstHeader			= pHeader->pNext;
    if ( pGlobalFirstHeader != NULL ) {
      pGlobalFirstHeader->pPrevious	= (struct tagMALLHEADER *) NULL;
    }
  } else {
    /* Consistency check on the double-linked list: */
    ASSERT ( pHeader->pPrevious != NULL );
    pHeader->pPrevious->pNext	= pHeader->pNext;
    if ( pHeader->pNext != NULL ) {
      pHeader->pNext->pPrevious	= pHeader->pPrevious;
    }
  }

  pHeader->pPrevious		= (struct tagMALLHEADER *) NULL;
  pHeader->pNext		= (struct tagMALLHEADER *) NULL;

  RETURN ( bDone );
} /* fnMallocChainOut */

/* ----------------------------------------------------------------------- */
static PMALLHEADER	fnMallocSearch	( PMALLHEADER	pHeader )
{
  PMALLHEADER	pH, lpFound = (PMALLHEADER) NULL;

  PROCEDURE	( fnMallocSearch );

  ASSERT ( pHeader != NULL );

  for ( pH = pGlobalFirstHeader; pH != NULL && lpFound == NULL;
	pH = pH->pNext ) {
    if ( pH == pHeader ) {
      lpFound	= pHeader;
    }
  }

  RETURN ( lpFound );
} /* fnMallocSearch */

/* ----------------------------------------------------------------------- */
static int	fnMallocDumpBlocks	( LPCSTR	pszPrompt,
					  LPCSTR	pszModule,
					  LPCSTR	pszProcedure,
					  int		nLine  )
{
  time_t	tStop;
  char		szTimeStart [ 64 ], szTimeStop [ 64 ], szDuration [ 64 ];
  double	dDuration;
  int		nHours, nMinutes, nSeconds;
  PMALLHEADER	pH = (PMALLHEADER) NULL;
  int		nDumped = 0;
  FILE		* pLogFile;

  PROCEDURE	( fnMallocDumpBlocks );

  time ( &tStop );
  strftime ( szTimeStart, sizeof ( szTimeStart ),
	     szTimeFormat, localtime ( &tGlobalStart ) );
  strftime ( szTimeStop, sizeof ( szTimeStop ),
	     szTimeFormat, localtime ( &tStop ) );
  dDuration	= difftime ( tStop, tGlobalStart );
  nHours	= (int) ( ( dDuration + 0.5 ) / 3600.0 );
  nMinutes	= (int)
    ( ( dDuration - (double) nHours * 3600.0 + 0.5 ) / 60.0 );
  nSeconds	= (int)
    ( dDuration - (double) nHours * 3600.0 - (double) nMinutes * 60.0 + 0.5 );
  sprintf ( szDuration, "%d:%02d:%02d", nHours, nMinutes, nSeconds );

  _LOG ( pszModule, pszProcedure, nLine,
	 ( "Malloc() called %d times for a total of %g bytes,\n"
	   "       %g bytes per malloc().\n"
	   "       Realloc() called %d times for a total of %g bytes,\n"
	   "       increase factor %g\n"
	   "       Free() called %d times.\n"
	   "       Time from %s to %s = %s\n"
	   "%s%s%s"
	   "       Dump of %d allocated blocks:",

	   nGlobalMalloc,
	   dGlobalMallocAmount,
	   ( ( nGlobalMalloc > 0 ) ?
	     dGlobalMallocAmount / (double) nGlobalMalloc :
	     dGlobalMallocAmount ),

	   nGlobalReAlloc,
	   dGlobalReAllocAmount,
	   ( ( nGlobalReAlloc > 0 ) ?
	     dGlobalReAllocIncrease / (double) nGlobalReAlloc :
	     dGlobalReAllocIncrease ),

	   nGlobalFree,

	   szTimeStart, szTimeStop, szDuration,
	   ( ( pszPrompt != (LPCSTR) NULL && *pszPrompt != '\0' ) ?
	     "       " : szEmpty ),
	   ( ( pszPrompt != (LPCSTR) NULL && *pszPrompt != '\0' ) ?
	     pszPrompt : szEmpty ),
	   ( ( pszPrompt != (LPCSTR) NULL && *pszPrompt != '\0' ) ?
	     "\n" : szEmpty ),

	   nGlobalMalloc - nGlobalFree ));

  pLogFile	= fnLogOpen ();
  if ( pLogFile != NULL ) {
    if ( nGlobalMalloc == nGlobalFree ) {
      fputs ( "     * No blocks allocated.\n", pLogFile );
    } else if ( pGlobalFirstHeader == NULL ) {
      fputs ( "     * No further allocation information available.\n",
	      pLogFile );
    } else {
      for ( pH = pGlobalFirstHeader, nDumped = 0; pH != NULL;
	    pH = pH->pNext, nDumped++ ) {
	char	szTime [ 64 ];
	strftime ( szTime, sizeof ( szTime ),
		   szTimeFormat, localtime ( &pH->tTime ) );
	fprintf ( pLogFile,
		  "     * Block %d at 0x%X (label %d), size %d:\n"
		  "       Allocated by %s(%d), %s\n"
		  "       on %s\n",
		  nDumped,
		  (unsigned long) ( ( (LPBYTE) pH ) + sizeof ( MALLHEADER ) ),
		  pH->nLabel, pH->nSize,
		  ( pH->pszModule != (LPCSTR) NULL &&
		    *pH->pszModule != '\0' ) ?
		  pH->pszModule :
		  UNREADABLE_OBJECT_PREFIX "unknown module"
		  UNREADABLE_OBJECT_SUFFIX,
		  pH->nLine,
		  ( pH->pszProcedure != (LPCSTR) NULL &&
		    *pH->pszProcedure != '\0' ) ?
		  pH->pszProcedure :
		  UNREADABLE_OBJECT_PREFIX "unknown procedure"
		  UNREADABLE_OBJECT_SUFFIX,
		  szTime );
      }
    }
  }

  RETURN ( nDumped );
} /* fnMallocDumpBlocks */

/* -------------------------------------------------------------------------
| Extern function bodies
 ------------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnMalloc( size_t	nSize,
				  LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine )
{
  LPVOID	lpAllocated = NULL;
  PMALLHEADER	pHeader = (PMALLHEADER) NULL;

  PROCEDURE	( fnMalloc );
  INITIALIZECOMMON;

  nGlobalAllocated++;
  if ( eGlobalMallFlags >= emDebug ) {
    pHeader	= (PMALLHEADER) malloc ( nSize + sizeof ( MALLHEADER ) );
    if ( pHeader != NULL ) {
      /* Chain the block in, adjust the lpAllocated pointer: */
      lpAllocated	= fnMallocChainIn ( pHeader, nSize, pszModule,
					    pszProcedure, nLine );
    }
    if ( eGlobalMallFlags >= emVerbose ) {
      _LOG ( pszModule, pszProcedure, nLine,
	     ( "Allocated 0x%X (label %d), size %d; m %4d, f %4d",
	       (unsigned long) lpAllocated,
	       ( pHeader != NULL ) ? pHeader->nLabel : -1,
	       nSize, nGlobalMalloc, nGlobalFree ));
    }
  } else {
    lpAllocated	= malloc ( nSize );
  }

  if ( lpAllocated == NULL ) {
    _CERROR ( pszModule, pszProcedure, nLine,
	      ( szReturnNULL, "malloc() returned NULL for size %d.",
		nSize ) );
  } else {
    nGlobalMalloc++;
    dGlobalMallocAmount	+= (double) nSize;
  }

  RETURN ( lpAllocated );
} /* fnMalloc */

/* ----------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnReAlloc( LPVOID	lpAllocated,
				  size_t	nNewSize,
				  LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine )
{
  LPVOID	lpReallocated = NULL;
  PMALLHEADER	pOldHeader = (PMALLHEADER) NULL,
    pNewHeader = (PMALLHEADER) NULL;
  int		nOldLabel = -1;
  size_t	nOldSize = -1;

  PROCEDURE	( fnReAlloc );

  INITIALIZECOMMON;

  if ( eGlobalMallFlags >= emDebug ) {
    /* Chain the block out: */
    if ( lpAllocated != NULL ) {
      pOldHeader	= (PMALLHEADER)
	( ( (LPBYTE) lpAllocated ) - sizeof ( MALLHEADER ) );
      nOldLabel		= pOldHeader->nLabel;
      nOldSize		= pOldHeader->nSize;
      if ( fnMallocSearch ( pOldHeader ) == NULL ) {
	_CERROR ( pszModule, pszProcedure, nLine,
		  ( "malloc() a new pointer.", szInvalidPointer,
		    (unsigned long) lpAllocated, "realloc" ) );
	lpAllocated	= NULL;
      } else {
	fnMallocChainOut ( pOldHeader );
      }
    }
    pNewHeader	= ( pOldHeader == NULL ) ?
      (PMALLHEADER) malloc ( nNewSize + sizeof ( MALLHEADER ) ) :
      (PMALLHEADER) realloc ( pOldHeader, nNewSize + sizeof ( MALLHEADER ) );
    if ( pNewHeader != NULL ) {
      /* Chain the block in, adjust the lpReallocated pointer: */
      lpReallocated	= fnMallocChainIn ( pNewHeader, nNewSize, pszModule,
					    pszProcedure, nLine );
    }
    if ( eGlobalMallFlags >= emVerbose ) {
      _LOG ( pszModule, pszProcedure, nLine,
	     ( "Reallocated from 0x%X (label %d), size %d\n"
	       "       to 0x%X (label %d), size %d; m %4d, f %4d",
	       (unsigned long) lpAllocated, nOldLabel, nOldSize,
	       (unsigned long) lpReallocated,
	       ( pNewHeader != NULL ) ? pNewHeader->nLabel : -1,
	       nNewSize,
	       nGlobalMalloc, nGlobalFree ));
    }
  } else {
    lpReallocated	= ( lpAllocated == NULL ) ?
      malloc ( nNewSize ) : realloc ( lpAllocated, nNewSize );
  }

  if ( lpReallocated == NULL ) {
    _CERROR ( pszModule, pszProcedure, nLine,
	      ( szReturnNULL, "realloc() returned NULL for size %d.",
		nNewSize ) );
  } else {
    nGlobalReAlloc++;
    dGlobalReAllocAmount	+= nNewSize;
    dGlobalReAllocIncrease	+= ( nOldSize > 0 ) ?
      ( (double) nNewSize / (double) nOldSize ) : 0.0;
  }

  RETURN ( lpReallocated );  
} /* fnReAlloc */

/* ----------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnFree	( LPVOID	lpAllocated,
				  LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine )
{
  PMALLHEADER	pHeader = (PMALLHEADER) NULL;

  PROCEDURE	( fnFree );
  INITIALIZECOMMON;

  if ( eGlobalMallFlags >= emDebug ) {
    /* Chain the block out: */
    if ( lpAllocated != NULL ) {
      pHeader	= (PMALLHEADER)
	( ( (LPBYTE) lpAllocated ) - sizeof ( MALLHEADER ) );
      if ( fnMallocSearch ( pHeader ) == NULL ) {
	_CERROR ( pszModule, pszProcedure, nLine,
		  ( "Don't call free().", szInvalidPointer,
		    (unsigned long) lpAllocated, "free" ) );
	pHeader		= (PMALLHEADER) NULL;
      } else {
	fnMallocChainOut ( pHeader );
      }
    }
    if ( eGlobalMallFlags >= emVerbose ) {
      _LOG ( pszModule, pszProcedure, nLine,
	     ( "Free'd 0x%X; m %4d, f %4d.",
	       (unsigned long) lpAllocated,
	       nGlobalMalloc, nGlobalFree ));
    }
    if ( pHeader != NULL ) {
      free ( pHeader );
      pHeader		= (PMALLHEADER) NULL;
      lpAllocated	= NULL;
    }
  } else if ( lpAllocated != NULL ) {
    free ( lpAllocated );
    lpAllocated	= NULL;
  }
  if ( lpAllocated == NULL ) {
    nGlobalFree++;
  }
  RETURN ( lpAllocated );
} /* fnFree */

/* ----------------------------------------------------------------------- */
MALLFLAGS DLLEXPORT	fnMallocFlags	( MALLFLAGS	eNewFlags )
{
  PROCEDURE	( fnMallocFlags );
  INITIALIZECOMMON;
  if ( ( eNewFlags != emGetFlags ) &&
       ( ( eGlobalMallFlags >= emDebug && eNewFlags >= emDebug ) ||
	 nGlobalAllocated == 0 ) ) {
    eGlobalMallFlags	= eNewFlags;
  }
  RETURN ( eGlobalMallFlags );
} /* fnMallocFlags */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnMallocLabel	( LPVOID	lpAllocated )
{
  int		nLabel = -1;
  PMALLHEADER	pHeader = (PMALLHEADER) NULL;

  PROCEDURE	( fnMallocLabel );
  INITIALIZECOMMON;

  if ( lpAllocated != NULL && eGlobalMallFlags >= emDebug ) {
    nLabel	= 0;
    pHeader	= (PMALLHEADER)
      ( ( (LPBYTE) lpAllocated ) - sizeof ( MALLHEADER ) );
    if ( fnMallocSearch ( pHeader ) != NULL ) {
      nLabel	= pHeader->nLabel;
    }
  }
  RETURN ( nLabel );
} /* fnMallocLabel */

/* ----------------------------------------------------------------------- */
void DLLEXPORT	fnMallocLog	( LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine )
{
  PROCEDURE	( fnMallocLog );

  INITIALIZECOMMON;

  if ( eGlobalMallFlags >= emDebug ) {
    fnMallocDumpBlocks ( (LPCSTR) NULL, pszModule, pszProcedure, nLine );
  }
  RETURN ( VOID );
} /* fnMallocLog */

/* ----------------------------------------------------------------------------
| Module initialization / Deinitialization
 --------------------------------------------------------------------------- */
void		fnInitializeMallocModule	( void )
{
  PROCEDURE	( fnInitializeMallocModule );
  time ( &tGlobalStart );
  RETURN ( VOID );
} /* fnInitializeMallocModule */

/* ------------------------------------------------------------------------- */
void		fnDeinitializeMallocModule	( void )
{
  PROCEDURE	( fnDeinitializeMallocModule );

  if ( eGlobalMallFlags >= emDebug ) {
    if ( pGlobalFirstHeader != NULL || nGlobalMalloc != nGlobalFree ) {
      /* There are still allocated headers left: */
      fnMallocDumpBlocks ( "Found allocated blocks at program termination.",
			   __szFile__, __szProc__, __LINE__ );
    } else {
      fnMallocDumpBlocks ( (LPCSTR) NULL, __szFile__, __szProc__, __LINE__ );
    }
  }

  RETURN ( VOID );
} /* fnDeinitializeMallocModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
