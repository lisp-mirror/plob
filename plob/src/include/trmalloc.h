/* ---------------------------------------------------------------------------
| Module	trmalloc.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1997/06/27
| Description	Traced malloc
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
 -------------------------------------------------------------------------- */

#ifndef	TRMALLOC_H_INCLUDED
#define	TRMALLOC_H_INCLUDED

#include	<stdlib.h>
#include	"global.h"

/* ---------------------------------------------------------------------------
| Constants, types
 -------------------------------------------------------------------------- */
typedef enum {
  /* Fast operation, with minimal overhead: */
  emFast,
  /* Debug operation, restrictive tests on consistency: */
  emDebug,
  /* Debug plus verbose operation, restrictive tests on consistency
     plus writing any malloc(), free() call to log file: */
  emVerbose,
  /* Don't set but get malloc flags: */
  emGetFlags = -1
}	MALLFLAGS, * PMALLFLAGS;

/* ---------------------------------------------------------------------------
| Macros
 -------------------------------------------------------------------------- */
#define	Malloc( nSize )			\
fnMalloc ( (nSize), __szFile__, __szProc__, __LINE__ )

#define	ReAlloc( lpAllocated, nSize )	\
fnReAlloc ( (lpAllocated), (nSize), __szFile__, __szProc__, __LINE__ )

#define	ReAllocF( lpAllocated, nSize )	\
fnReAllocF ( (lpAllocated), (nSize), __szFile__, __szProc__, __LINE__ )

#define	Free( lpAllocated )		\
fnFree ( (lpAllocated), __szFile__, __szProc__, __LINE__ )

#define	MallocLog()			\
fnMallocLog ( __szFile__, __szProc__, __LINE__ )

/* ---------------------------------------------------------------------------
| Function protoypes
 -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Function	fnMalloc
| Arguments	see malloc()
| Return	New allocated memory
| Description	Allocate memory
 ------------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnMalloc( size_t	nSize,
				  LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine );

/* -------------------------------------------------------------------------
| Function	fnReAlloc
| Arguments	see realloc()
| Return	Reallocated memory
| Description	Reallocate memory
 ------------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnReAlloc( LPVOID	lpAllocated,
				  size_t	nNewSize,
				  LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine );

/* -------------------------------------------------------------------------
| Function	fnReAllocF
| Arguments	see reallocf()
| Return	Reallocated memory
| Description	Reallocate memory
 ------------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnReAllocF( LPVOID	lpAllocated,
				  size_t	nNewSize,
				  LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine );

/* -------------------------------------------------------------------------
| Function	fnFree
| Arguments	see free()
| Return	Free allocated memory
| Description	Free memory
 ------------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnFree	( LPVOID	lpAllocated,
				  LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine );

/* -------------------------------------------------------------------------
| Function	fnMallocFlags
| Description	Get/set current malloc flags. Setting is only possible 
|		before the first memory block is allocated. Default
|		operation modus is emFast.
 ------------------------------------------------------------------------- */
MALLFLAGS DLLEXPORT	fnMallocFlags	( MALLFLAGS	eNewFlags );

/* -------------------------------------------------------------------------
| Function	fnMallocLabel
| Description	Return the label of lpAllocated;
|		Returns -1 if no label information is available.
|		Returns 0 if lpAllocated doesn't point to an allocated area.
 ------------------------------------------------------------------------- */
int DLLEXPORT	fnMallocLabel	( LPVOID	lpAllocated );

/* -------------------------------------------------------------------------
| Function	fnMallocLog
| Description	Write current allocated blocks to the log file.
 ------------------------------------------------------------------------- */
void DLLEXPORT	fnMallocLog	( LPCSTR	pszModule,
				  LPCSTR	pszProcedure,
				  int		nLine );

/* ---------------------------------------------------------------------------
| Module initialization / Deinitialization
 -------------------------------------------------------------------------- */
void		fnInitializeMallocModule	( void );
void		fnDeinitializeMallocModule	( void );

#endif

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
