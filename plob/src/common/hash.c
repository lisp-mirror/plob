/* -------------------------------------------------------------------------
| Module	hash.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		28.10.93
| Description	Implements hash tables based on Coalesced chaining as
|		proposed in:
|			D. E. Knuth:
|			The Art of Computer Programmming
|			Vol. 3: Sorting and Searching
|			p. 515
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#if	!WIN32
#include	<unistd.h>
#endif

#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
LPHASHTABLE		__lpHashTableGet__	= (LPHASHTABLE) NULL;
HASHKEY			__dwHashKeyGet__	= 0;
LPVOID			__lpDataField__		= NULL;

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
#define	sizeofHASHENTRY( nSizeOfDataField )	\
		(sizeof(HASHENTRY)-sizeof(((LPHASHENTRY)NULL)->vUserData)+\
		 (nSizeOfDataField))
#define	indexHASHENTRY( lpHashEntries, nSizeOfDataField, nIndex )	\
		((LPHASHENTRY)((LPSTR)(lpHashEntries)+(nIndex)*\
			       sizeofHASHENTRY(nSizeOfDataField)))


/* -------------------------------------------------------------------------
| Static constants, types and variables
 ------------------------------------------------------------------------- */
/* When the hash table is full, it will be resized. The new size is computed
   by:
     <current size> * newsizeEnumerator / newsizeDenominator
*/
enum {
  newsizeEnumerator	= 2,
  newsizeDenominator	= 1
};
enum {
  /* Number of primes to compute additionally when the prime table
     'overflows': */
  primetableExtraPrimes	= 10
};
enum {
  eOutOfIteration	= -2,
  eZombieTable		= -3
};

static const char	szAccessToZombie [] =
  "Attempt to access %s.";

#define	ASSERT_NO_ZOMBIE( lpHashTable )	\
(((lpHashTable)==NULL||(lpHashTable)->nIterator==eZombieTable)?		\
 (ERROR((szAccessToZombie,fnHashGetName(lpHashTable,(LPSTR)NULL,0))),FALSE):\
 TRUE)

/* This value marks a hash entry in lpNext as a free entry: */
#define	hashEntryFree		((struct HASHENTRY_tag FAR *)0x00000000)
/* This value marks a hash entry in lpNext as a occupied entry: */
#define	hashEntryOccupied	((struct HASHENTRY_tag FAR *)0xFFFFFFFF)
/* Any other value of lpNext in a hash entry is a pointer to the next
   entry. */

/* -------------------------------------------------------------------------
| Static function prototypes
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Function	fnHashIsPrime
| Arguments	  dwNumber
|		The number to test if it is prime
| Return	TRUE if wNumber is prime.
| Description	Check if a number is prime.
 ------------------------------------------------------------------------- */
static BOOL	fnHashIsPrime		( DWORD dwNumber );

/* -------------------------------------------------------------------------
| Function	fnHashGetPrimeSize
| Arguments	  wIndex
|		The index of the size prime number which should be computed.
| Return	The found size prime number.
| Description	Retrieve size prime number for index wIndex.
 ------------------------------------------------------------------------- */
static DWORD	fnHashGetPrimeSize	( WORD wIndex );

/* -------------------------------------------------------------------------
| Function	fnHashCreateEntries
| Arguments	  nElements
|		  nSizeOfDataField
| Return	A pointer to a new allocated and initialized hash entry
|		table.
| Description	Allocate an entry table for nElements entries with a data
|		field of nSizeOfDataField bytes of data field.
 ------------------------------------------------------------------------- */
static LPHASHENTRY	fnHashCreateEntries	( LPHASHTABLE lpHashTable,
						  size_t nInitialElements,
						  size_t nSizeOfDataField );

/* -------------------------------------------------------------------------
| Function	fnHashResizeToIndex
| Arguments	  lpHashTable
| Return	
| Description	
 ------------------------------------------------------------------------- */
static size_t		fnHashResizeToIndex	( LPHASHTABLE lpHashTable,
						  WORD wSizeIndex );

/* -------------------------------------------------------------------------
| Function	fnHashReInsert
| Arguments	  lpUserData
|		The new hash table, into which the elements of the old
|		hash table are inserted.
| Return	Always hashContinue
| Description	Reinsert elements from an old into a new hash table.
 ------------------------------------------------------------------------- */
static HASHENUM		fnHashReInsert		( LPHASHTABLE lpHashTable,
						  HASHKEY dwHashKey,
						  LPVOID lpDataField,
						  size_t nSizeOfDataField,
						  LPVOID lpUserData );

/* -------------------------------------------------------------------------
| Static function bodies
 ------------------------------------------------------------------------- */
static BOOL	fnHashIsPrime		( DWORD dwNumber )
{
  register DWORD	dwN;
  register WORD		wDivisor;

  PROCEDURE	( fnHashIsPrime );

  dwN		= dwNumber;
  if ( ! ( dwN & 0x01 ) )
    /* Divideable by 2: */
    RETURN ( FALSE );

  wDivisor	= 3;
  while ( (DWORD) wDivisor * (DWORD) wDivisor <= dwN ) {
    if ( dwN % wDivisor == 0 )
      RETURN ( FALSE );
    wDivisor	+= 2;
  }
  RETURN ( TRUE );
} /* fnHashIsPrime */

/* ----------------------------------------------------------------------- */
static WORD	wGlobalPrimeSizes	= 0;
/* Table with prime sizes: */
static LPDWORD	pdwGlobalPrimeSizeTable	= (LPDWORD) NULL;

/* ----------------------------------------------------------------------- */
static DWORD	fnHashGetPrimeSize	( WORD wIndex )
{

  int			i, n;
  DWORD			dwNumber;

  PROCEDURE		( fnHashGetPrimeSize );

  if ( wIndex >= wGlobalPrimeSizes ) {
    n	= wIndex + primetableExtraPrimes;
    /* Enlarge the table: */
    if ( pdwGlobalPrimeSizeTable != NULL ) {
      pdwGlobalPrimeSizeTable	= (LPDWORD)
	ReAlloc ( pdwGlobalPrimeSizeTable, n *
		  sizeof ( *pdwGlobalPrimeSizeTable ) );
      ASSERT ( pdwGlobalPrimeSizeTable != NULL );
    } else {
      pdwGlobalPrimeSizeTable	= (LPDWORD)
	Malloc ( n * sizeof ( *pdwGlobalPrimeSizeTable ) );
      ASSERT ( pdwGlobalPrimeSizeTable != NULL );
      wGlobalPrimeSizes		= 1;
      /* With this size the hash table will start;
	 has to be a prime number: */
      pdwGlobalPrimeSizeTable [ 0 ]	= 11;
    }
    /* Compute the next prime numbers: */
    for ( i = wGlobalPrimeSizes; i < n; i++ ) {
      dwNumber = pdwGlobalPrimeSizeTable [ i - 1 ] * newsizeEnumerator /
	newsizeDenominator;
      if ( ! ( dwNumber & 0x01 ) )
	/* Divideable by 2: */
	dwNumber++;
      while ( ! fnHashIsPrime ( dwNumber ) )
	dwNumber	+= 2;
      pdwGlobalPrimeSizeTable [ i ]	= dwNumber;
    }
    wGlobalPrimeSizes	= n;
  }
  RETURN ( pdwGlobalPrimeSizeTable [ wIndex ] );
} /* fnHashGetPrimeSize */

/* ----------------------------------------------------------------------- */
static LPHASHENTRY	fnHashCreateEntries	( LPHASHTABLE lpHashTable,
						  size_t nElements,
						  size_t nSizeOfDataField )
{
  LPHASHENTRY	lpHashEntries;

  PROCEDURE	( fnHashCreateEntries );

  lpHashEntries	= (LPHASHENTRY)
    fnMalloc ( nElements * sizeofHASHENTRY ( nSizeOfDataField ),
	       ( lpHashTable != NULL &&
		 lpHashTable->pszFile != (LPCSTR) NULL ) ?
	       lpHashTable->pszFile : (LPCSTR) NULL,
	       ( lpHashTable != NULL &&
		 lpHashTable->pszProc != (LPCSTR) NULL ) ?
	       lpHashTable->pszProc : (LPCSTR) NULL,
	       ( lpHashTable != NULL ) ?
	       lpHashTable->nLine : 0 );
  ASSERT ( lpHashEntries != NULL );
  memset ( lpHashEntries, 0,
	   nElements * sizeofHASHENTRY ( nSizeOfDataField ) );
  RETURN ( lpHashEntries );
} /* fnHashCreateEntries */

/* ----------------------------------------------------------------------- */
static size_t		fnHashResizeToIndex	( LPHASHTABLE lpHashTable,
						  WORD wSizeIndex )
{
  HASHTABLE	NewHashTable;

  PROCEDURE	( fnHashResizeToIndex );

  ASSERT ( lpHashTable != NULL );

  /* Build up a new table: */
  NewHashTable			= *lpHashTable;
  NewHashTable.nElements	= fnHashGetPrimeSize ( wSizeIndex );
  ASSERT ( NewHashTable.nElements >= lpHashTable->nOccupied );
  if ( NewHashTable.nElements == lpHashTable->nElements ) {
    /* No change in size: */
    RETURN ( lpHashTable->nElements );
  }

  NewHashTable.nOccupied	= 0;
  NewHashTable.wSizeIndex	= wSizeIndex;
  NewHashTable.lpHashEntryLast	= (LPHASHENTRY) NULL;
  NewHashTable.lpEntries	=
    fnHashCreateEntries ( &NewHashTable, NewHashTable.nElements,
			  NewHashTable.nSizeOfDataField );
  ASSERT ( NewHashTable.lpEntries != NULL );

  /* Re-insert the elements of the old table into the new table: */
  fnHashEnum ( (LPHASHTABLE) lpHashTable, fnHashReInsert, &NewHashTable );

  /* Free the entries of the old table: */
  if ( lpHashTable->lpEntries != NULL ) {
    Free ( lpHashTable->lpEntries );
    lpHashTable->lpEntries	= (LPHASHENTRY) NULL;
  }

  *lpHashTable			= NewHashTable;

  RETURN ( lpHashTable->nElements );
} /* fnHashResizeToIndex */

/* ----------------------------------------------------------------------- */
static HASHENUM		fnHashReInsert		( LPHASHTABLE lpHashTable,
						  HASHKEY dwHashKey,
						  LPVOID lpDataField,
						  size_t nSizeOfDataField,
						  LPVOID lpUserData )
{
  PROCEDURE	( fnHashReInsert );

  ASSERT ( lpUserData != NULL );
  fnHashInsert ( (LPHASHTABLE) lpUserData, dwHashKey, lpDataField );
  RETURN ( hashContinue );
} /* fnHashReInsert */

/* ----------------------------------------------------------------------- */
static LPSTR	fnHashGetName	( LPHASHTABLE	lpHashTable,
				  LPSTR		pszName,
				  size_t	nName )
{
  static char	szHashTable [ 256 ];
  LPCSTR	pszZombie;

  PROCEDURE	( fnHashGetName );

  if ( lpHashTable == NULL ) {
    sprintf ( szHashTable, UNREADABLE_OBJECT_PREFIX 
	      "hashtable %s"
	      UNREADABLE_OBJECT_SUFFIX,
	      szNULL );
  } else {
    pszZombie	=  ( lpHashTable->nIterator == eZombieTable ) ?
      "zombie old-class=" : szEmpty;
    if ( lpHashTable->pszFile != (LPCSTR) NULL ) {
      sprintf ( szHashTable, UNREADABLE_OBJECT_PREFIX 
		"%shashtable 0x%X %d/%d %s(%d): %s"
		UNREADABLE_OBJECT_SUFFIX,
		pszZombie, (unsigned long) lpHashTable,
		lpHashTable->nOccupied, lpHashTable->nElements,
		lpHashTable->pszFile, lpHashTable->nLine,
		lpHashTable->pszProc );
    } else {
      sprintf ( szHashTable, UNREADABLE_OBJECT_PREFIX 
		"%shashtable 0x%X %d/%d"
		UNREADABLE_OBJECT_SUFFIX,
		pszZombie, (unsigned long) lpHashTable,
		lpHashTable->nOccupied, lpHashTable->nElements );
    }
  }
  if ( pszName != NULL ) {
    strncpy ( pszName, szHashTable, nName );
    RETURN ( pszName );
  } else {
    RETURN ( szHashTable );
  }
} /* fnHashGetName */

/* -------------------------------------------------------------------------
| Extern function bodies
 ------------------------------------------------------------------------- */
LPHASHTABLE DLLEXPORT	fnHashCreate	( LPHASHTABLE lpHashTable,
					  size_t nInitialElements,
					  size_t nSizeOfDataField,
					  LPCSTR pszFile,
					  LPCSTR pszProc,
					  int nLine )
{
  PROCEDURE	( fnHashCreate );
  INITIALIZECOMMON;

  if ( lpHashTable == NULL ) {
    lpHashTable	= (LPHASHTABLE) Malloc ( sizeof ( *lpHashTable ) );
    ASSERT ( lpHashTable != NULL );
    memset ( lpHashTable, 0, sizeof ( *lpHashTable ) );
    lpHashTable->nFlags	= (HASHFLAGS)
      ( (unsigned int) lpHashTable->nFlags | (unsigned int) eHashAllocated );
  } else {
    memset ( lpHashTable, 0, sizeof ( *lpHashTable ) );
  }
  lpHashTable->nIterator	= eOutOfIteration;
  lpHashTable->nSizeOfDataField	= nSizeOfDataField;
  lpHashTable->pszFile		= pszFile;
  lpHashTable->pszProc		= pszProc;
  lpHashTable->nLine		= nLine;
  if ( nInitialElements > 0 ) {
    while ( fnHashGetPrimeSize ( ++lpHashTable->wSizeIndex ) <
	    nInitialElements );
  }
  RETURN ( lpHashTable );
} /* fnHashCreate */

/* ----------------------------------------------------------------------- */
LPHASHTABLE DLLEXPORT	fnHashDestroy	( LPHASHTABLE lpHashTable )
{
  PROCEDURE	( fnHashDestroy );
  INITIALIZECOMMON;

  if ( lpHashTable == NULL ) {
    RETURN ( lpHashTable );
  }

  ASSERT_NO_ZOMBIE ( lpHashTable );

  if ( lpHashTable->nIterator != eOutOfIteration ) {
    char	szHashTable [ 256 ];
    ERROR (( "Attempt to destroy %s\n"
	     "       which is currently iterated at position %d.",
	     fnHashGetName ( lpHashTable, szHashTable,
			     sizeof ( szHashTable ) ),
	     lpHashTable->nIterator ));
  }

  if ( lpHashTable->lpEntries != NULL ) {
    Free ( lpHashTable->lpEntries );
    lpHashTable->lpEntries	= (LPHASHENTRY) NULL;
  }
  lpHashTable->nIterator	= eZombieTable;
  if ( ( lpHashTable->nFlags & eHashAllocated ) != 0 ) {
    Free ( lpHashTable );
    lpHashTable->nFlags		= (HASHFLAGS)
      ( (unsigned int) lpHashTable->nFlags & ~ (unsigned int) eHashAllocated );
    lpHashTable			= (LPHASHTABLE) NULL;
  }
  RETURN ( lpHashTable );
} /* fnHashDestroy */

/* ----------------------------------------------------------------------- */
size_t DLLEXPORT	fnHashResize	( LPHASHTABLE lpHashTable,
					  size_t nNewElements )
{
  WORD		wSizeIndex;

  PROCEDURE	( fnHashResize );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );

  if ( nNewElements <= lpHashTable->nOccupied ) {
    /* Resize can't be done because the new size is less than or equal
       to the number of elements currently in the table: */
    RETURN ( lpHashTable->nElements );
  }

  wSizeIndex	= 0;
  while ( fnHashGetPrimeSize ( ++wSizeIndex ) < nNewElements );

  RETURN ( fnHashResizeToIndex (  lpHashTable, wSizeIndex ) );
} /* fnHashResize */

/* ----------------------------------------------------------------------- */
LPHASHTABLE DLLEXPORT	fnHashClear	( LPHASHTABLE lpHashTable )
{
  PROCEDURE	( fnHashClear );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );

  if ( lpHashTable->lpEntries && lpHashTable->nOccupied > 0 )
    memset ( lpHashTable->lpEntries, 0,
	     lpHashTable->nElements *
	     sizeofHASHENTRY ( lpHashTable->nSizeOfDataField ) );
  lpHashTable->nOccupied	= 0;
  lpHashTable->lpHashEntryLast	= (LPHASHENTRY) NULL;
#ifndef	NODEBUG
  lpHashTable->dwProbes		= 0;
  lpHashTable->dwHits		= 0;
#endif /* #ifndef NODEBUG */
  RETURN ( lpHashTable );
} /* fnHashClear */

/* ----------------------------------------------------------------------- */
HASHRESULT DLLEXPORT	fnHashInsert	( LPHASHTABLE lpHashTable,
					  HASHKEY dwHashKey,
					  LPCVOID lpDataField )
{
  register int		i, j;
  register LPHASHENTRY	lpEntry, lpFree;

  PROCEDURE		( fnHashInsert );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );

  /* Check if table might overflow; if it seems so, enlarge it: */
  if ( lpHashTable->nOccupied >= lpHashTable->nElements ) {
    fnHashResizeToIndex ( lpHashTable, lpHashTable->wSizeIndex + 1 );
  }

  /* Check if the requested key is equal to the last key: */
  if ( lpHashTable->lpHashEntryLast &&
       lpHashTable->lpHashEntryLast->dwHashKey == dwHashKey ) {
    if ( lpDataField && lpHashTable->nSizeOfDataField ) {
      memcpy ( lpHashTable->lpHashEntryLast->vUserData,
	       lpDataField, lpHashTable->nSizeOfDataField );
    }
#ifndef	NODEBUG
    lpHashTable->dwHits++;
#endif /* #ifndef NODEBUG */
    RETURN ( hashUpdated );
  }

  ASSERT ( lpHashTable->lpEntries != NULL );

  /* Do the hash: */
  i		= dwHashKey % lpHashTable->nElements;
  lpEntry	= indexHASHENTRY ( lpHashTable->lpEntries,
				   lpHashTable->nSizeOfDataField,
				   i );

#ifndef	NODEBUG
  lpHashTable->dwProbes++;
#endif /* #ifndef NODEBUG */
  if ( lpEntry->lpNext == (struct HASHENTRYtag FAR *) hashEntryFree ) {
    /* Found a free hash entry: */
    lpEntry->dwHashKey	= dwHashKey;
    lpEntry->lpNext	= (struct HASHENTRYtag FAR *) hashEntryOccupied;
    if ( lpDataField && lpHashTable->nSizeOfDataField ) {
      memcpy ( lpEntry->vUserData, lpDataField,
	       lpHashTable->nSizeOfDataField );
    }
    lpHashTable->nOccupied++;
    lpHashTable->lpHashEntryLast    = lpEntry;
#ifndef	NODEBUG
  lpHashTable->dwHits++;
#endif /* #ifndef NODEBUG */
    RETURN ( hashInserted );
  }

  /* Traverse the links, look if key is found: */
  while ( TRUE ) {
    ASSERT ( lpEntry->lpNext != (struct HASHENTRYtag FAR *) hashEntryFree );
#ifndef	NODEBUG
    lpHashTable->dwProbes++;
#endif /* #ifndef NODEBUG */
    if ( lpEntry->dwHashKey == dwHashKey ) {
      /* Found hash key in linked list: */
      if ( lpDataField && lpHashTable->nSizeOfDataField ) {
	memcpy ( lpEntry->vUserData, lpDataField,
		 lpHashTable->nSizeOfDataField );
      }
      lpHashTable->lpHashEntryLast    = lpEntry;
#ifndef	NODEBUG
      lpHashTable->dwHits++;
#endif /* #ifndef NODEBUG */
      RETURN ( hashUpdated );
    }
    if ( lpEntry->lpNext == (struct HASHENTRYtag FAR *) hashEntryOccupied ) {
      /* Reached last element of the list: */
      break;
    }
    lpEntry	= lpEntry->lpNext;
  }

  lpFree	= (LPHASHENTRY) NULL;
  /* Search a free element in the hash table: */
  for ( j = i + 1; lpFree == NULL && j < lpHashTable->nElements;
        j++ ) {
    lpFree	= indexHASHENTRY ( lpHashTable->lpEntries,
				   lpHashTable->nSizeOfDataField,
				   j );
    if ( lpFree->lpNext != (struct HASHENTRYtag FAR *) hashEntryFree )
      lpFree	= (LPHASHENTRY) NULL;
  }
  if ( ! lpFree ) {
    for ( j = i - 1, lpFree = (LPHASHENTRY) NULL;
	  lpFree == NULL && j >= 0;
	  j-- ) {
      lpFree	= indexHASHENTRY ( lpHashTable->lpEntries,
				   lpHashTable->nSizeOfDataField,
				   j );
      if ( lpFree->lpNext != (struct HASHENTRYtag FAR *) hashEntryFree )
	lpFree	= (LPHASHENTRY) NULL;
    }
  }

  /* There has to be at least 1 free element because of the resize done
     at the start of the function: */
  ASSERT ( lpFree != NULL );

#ifndef	NODEBUG
  lpHashTable->dwProbes++;
  lpHashTable->dwHits++;
#endif /* #ifndef NODEBUG */

  /* Append the entry to the list: */
  lpEntry->lpNext	= lpFree;
  lpFree->dwHashKey	= dwHashKey;
  lpFree->lpNext	= (struct HASHENTRYtag FAR *) hashEntryOccupied;
  if ( lpDataField && lpHashTable->nSizeOfDataField ) {
    memcpy ( lpFree->vUserData, lpDataField,
	     lpHashTable->nSizeOfDataField );
  }
  lpHashTable->nOccupied++;
  lpHashTable->lpHashEntryLast    = lpFree;

  RETURN ( hashInserted );
} /* fnHashInsert */

/* ----------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnHashGet	( LPHASHTABLE lpHashTable,
					  HASHKEY dwHashKey )
{
  LPVOID		lpData;
  register LPHASHENTRY	lpEntry;

  PROCEDURE		( fnHashGet );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );

  /* Check if the requested key is equal to the last key: */
  if ( lpHashTable->lpHashEntryLast &&
       lpHashTable->lpHashEntryLast->dwHashKey == dwHashKey ) {
#ifndef	NODEBUG
    lpHashTable->dwHits++;
#endif /* #ifndef NODEBUG */
    RETURN ( ( lpHashTable->nSizeOfDataField > 0 ) ?
	     lpHashTable->lpHashEntryLast->vUserData :
	     (LPVOID) hashFound );
  }

  lpData	= NULL;
  lpEntry	= (LPHASHENTRY) NULL;

  if ( lpHashTable->nOccupied > 0 ) {
    /* Do the hash: */
    lpEntry	= indexHASHENTRY ( lpHashTable->lpEntries,
				   lpHashTable->nSizeOfDataField,
				   dwHashKey % lpHashTable->nElements );
    if ( lpEntry->lpNext != (struct HASHENTRYtag FAR *) hashEntryFree ) {
      /* Traverse the links, look if key is found: */
      while ( TRUE ) {
	ASSERT ( lpEntry->lpNext !=
		 (struct HASHENTRYtag FAR *) hashEntryFree );
#ifndef	NODEBUG
	lpHashTable->dwProbes++;
#endif /* #ifndef NODEBUG */
	if ( lpEntry->dwHashKey == dwHashKey ) {
	  /* Found hash key in linked list: */
	  lpData	=
	    ( lpHashTable->nSizeOfDataField > 0 ) ?
	      lpEntry->vUserData : (LPVOID) hashFound;
#ifndef	NODEBUG
	  lpHashTable->dwHits++;
#endif /* #ifndef NODEBUG */
	  lpHashTable->lpHashEntryLast    = lpEntry;
	  break;
	}
	if ( lpEntry->lpNext ==
	     (struct HASHENTRYtag FAR *) hashEntryOccupied ) {
	  /* Reached last element of the list: */
	  break;
	}
	lpEntry	= lpEntry->lpNext;
      }
    }
  }
  RETURN ( lpData );
} /* fnHashGet */

/* ----------------------------------------------------------------------- */
HASHENUM DLLEXPORT	fnHashEnum	( LPHASHTABLE lpHashTable,
					  LPFNHASHENUM lpfnEnumerate,
					  LPVOID lpUserData )
{
  register HASHENUM	eHashEnum;
  register int		i, nSizeOfEntry;
  register LPSTR	lpEntry;

  PROCEDURE		( fnHashEnum ); 
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );
  ASSERT ( lpfnEnumerate != NULL );
  ASSERT ( lpHashTable->nOccupied == 0 ||
	   lpHashTable->lpEntries != NULL );

  eHashEnum	= hashContinue;
  nSizeOfEntry	= sizeofHASHENTRY ( lpHashTable->nSizeOfDataField );

  for ( i = 0, lpEntry = (LPSTR) lpHashTable->lpEntries;
        eHashEnum == hashContinue && i < lpHashTable->nOccupied;
        lpEntry += nSizeOfEntry ) {
    if ( ((LPHASHENTRY)lpEntry)->lpNext !=
	 (struct HASHENTRYtag FAR *) hashEntryFree ) {
      i++;
      eHashEnum	=
	( * lpfnEnumerate ) ( lpHashTable,
			     ((LPHASHENTRY)lpEntry)->dwHashKey,
			     ( lpHashTable->nSizeOfDataField > 0 ) ?
			     ((LPHASHENTRY)lpEntry)->vUserData : NULL,
			     lpHashTable->nSizeOfDataField,
			     lpUserData );
    }
  }
  RETURN ( eHashEnum );
} /* fnHashEnum */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		fnHashFirst	( LPHASHTABLE lpHashTable,
					  LPHASHKEY lpdwHashKey,
					  LPVOID FAR * lplpDataField,
					  size_t FAR * lpnSizeOfDataField )
{
  PROCEDURE	( fnHashFirst );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );
  ASSERT ( lpHashTable->nOccupied == 0 ||
	   lpHashTable->lpEntries != NULL );
  
  if ( lpHashTable->nIterator != eOutOfIteration ) {
    char	szHashTable [ 256 ];
    lpHashTable->nIterator	= eOutOfIteration;
    ERROR (( "Call to fnHashFirst() on %s\n"
	     "       is not reentrent.",
	     fnHashGetName ( lpHashTable, szHashTable,
			     sizeof ( szHashTable ) ) ));
  }
  lpHashTable->nIterator	= -1;

  RETURN ( fnHashNext ( lpHashTable, lpdwHashKey,
			lplpDataField, lpnSizeOfDataField ) );
} /* fnHashFirst */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		fnHashNext	( LPHASHTABLE lpHashTable,
					  LPHASHKEY lpdwHashKey,
					  LPVOID FAR * lplpDataField,
					  size_t FAR * lpnSizeOfDataField )
{
  register int		i, nSizeOfEntry;
  register LPSTR	lpEntry;

  PROCEDURE	( fnHashNext );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );
  ASSERT ( lpHashTable->nOccupied == 0 ||
	   lpHashTable->lpEntries != NULL );

  if ( lpHashTable->nIterator == eOutOfIteration ) {
    char	szHashTable [ 256 ];
    ERROR (( "Called fnHashNext() on %s\n"
	     "      without a prior call to fnHashFirst().",
	     fnHashGetName ( lpHashTable, szHashTable,
			     sizeof ( szHashTable ) ) ));
  }
  nSizeOfEntry	= sizeofHASHENTRY ( lpHashTable->nSizeOfDataField );

  for ( i = lpHashTable->nIterator + 1,
	  lpEntry = (LPSTR) lpHashTable->lpEntries + i * nSizeOfEntry;
	i < lpHashTable->nElements;
        i++, lpEntry += nSizeOfEntry ) {
    if ( ((LPHASHENTRY)lpEntry)->lpNext !=
	 (struct HASHENTRYtag FAR *) hashEntryFree ) {
      if ( lpdwHashKey ) {
	*lpdwHashKey		= ((LPHASHENTRY)lpEntry)->dwHashKey;
      }
      if ( lplpDataField ) {
	*lplpDataField		= ( lpHashTable->nSizeOfDataField > 0 ) ?
	  &((LPHASHENTRY)lpEntry)->vUserData : NULL;
      }
      if ( lpnSizeOfDataField ) {
	*lpnSizeOfDataField	= lpHashTable->nSizeOfDataField;
      }
      lpHashTable->nIterator	= i;
      RETURN ( TRUE );
    }
  }
  fnHashLast ( lpHashTable );
  RETURN ( FALSE );
} /* fnHashNext */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT		fnHashLast	( LPHASHTABLE lpHashTable )
{
  BOOL		bInIteration;

  PROCEDURE	( fnHashLast );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );
  ASSERT ( lpHashTable->nOccupied == 0 ||
	   lpHashTable->lpEntries != NULL );

  bInIteration			= (BOOL)
    ( lpHashTable->nIterator != eOutOfIteration );
  lpHashTable->nIterator	= eOutOfIteration;

  RETURN ( bInIteration );
} /* fnHashLast */

/* ----------------------------------------------------------------------- */
HASHRESULT DLLEXPORT	fnHashDelete	( LPHASHTABLE lpHashTable,
					  HASHKEY dwHashKey )
{
  register int		nTail, i;
  register LPHASHENTRY	lpEntry, lpFound, lpLast, lpNext;
  register LPHASHENTRY	lpEntriesSaved, lpSaved;

  PROCEDURE		( fnHashDelete );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );

  if ( lpHashTable->lpEntries ) {
    /* Do the hash: */
    lpEntry	= indexHASHENTRY ( lpHashTable->lpEntries,
				   lpHashTable->nSizeOfDataField,
				   dwHashKey % lpHashTable->nElements );
    if ( lpEntry->lpNext == (struct HASHENTRYtag FAR *) hashEntryFree ) {
      RETURN ( hashNotFound );
    }
    /* Traverse the links, look if key is found: */
    lpFound	= (LPHASHENTRY) NULL;
    lpLast	= (LPHASHENTRY) NULL;
    lpNext	= (LPHASHENTRY) NULL;
    nTail	= 0;
    while ( TRUE ) {
      ASSERT ( lpEntry->lpNext != (struct HASHENTRYtag FAR *) hashEntryFree );
#ifndef	NODEBUG
      lpHashTable->dwProbes++;
#endif /* #ifndef NODEBUG */
      if ( lpFound != NULL )
	nTail++;
      lpNext	= lpEntry->lpNext;
      if ( lpEntry->dwHashKey == dwHashKey ) {
	/* Found hash key in linked list: */
#ifndef	NODEBUG
	lpHashTable->dwHits++;
#endif /* #ifndef NODEBUG */
	/* If the following ASSERT fails: A hash key occurred twice
	   in a hash table:*/
	ASSERT ( lpFound == NULL );
	/* Mark current entry as free: */
	lpFound		= lpNext;
	lpEntry->lpNext	= (struct HASHENTRYtag FAR *) hashEntryFree;
	lpHashTable->nOccupied--;
	if ( lpHashTable->lpHashEntryLast == lpEntry ) {
	  /* Invalidate the last entry 'in work' (because it was deleted
	     here): */
	  lpHashTable->lpHashEntryLast	= (LPHASHENTRY) NULL;
	}
	/* Set end-of-list to entry preceeding the current entry: */
	if ( lpLast )
	  lpLast->lpNext = (struct HASHENTRYtag FAR *) hashEntryOccupied;
      }
      if ( lpNext == (struct HASHENTRYtag FAR *) hashEntryOccupied )
	/* Reached last element of list: */
	break;
      lpLast	= lpEntry;
      lpEntry	= lpNext;
    }
    if ( lpFound ) {
      if ( nTail > 0 ) {
	/* Save tail of list and mark saved entries as free: */
	lpEntriesSaved	= (LPHASHENTRY)
	  Malloc ( sizeofHASHENTRY ( lpHashTable->nSizeOfDataField ) * nTail );
	for ( lpEntry = lpFound, i = 0; i < nTail; i++ ) {
	  lpSaved	= indexHASHENTRY ( lpEntriesSaved,
					   lpHashTable->nSizeOfDataField, i );
	  lpSaved->dwHashKey	= lpEntry->dwHashKey;
	  memcpy ( lpSaved->vUserData, lpEntry->vUserData,
		   lpHashTable->nSizeOfDataField );
	  lpNext		= lpEntry->lpNext;
	  lpEntry->lpNext	= (struct HASHENTRYtag FAR *) hashEntryFree;
	  lpHashTable->nOccupied--;
          if ( lpHashTable->lpHashEntryLast == lpEntry ) {
	    /* Invalidate the last entry 'in work' (because it was deleted
	       here): */
	    lpHashTable->lpHashEntryLast	= (LPHASHENTRY) NULL;
	  }
	  lpEntry		= lpNext;
	}
	/* Re-insert tail of list: */
	for ( i = 0; i < nTail; i++ ) {
	  lpSaved	= indexHASHENTRY ( lpEntriesSaved,
					   lpHashTable->nSizeOfDataField, i );
	  fnHashInsert ( lpHashTable, lpSaved->dwHashKey, lpSaved->vUserData );
	}
	Free ( lpEntriesSaved );
	lpEntriesSaved	= (LPHASHENTRY) NULL;
      }
    } else {
      /* Hash key wasn't found in table: */
      RETURN ( hashNotFound );
    }
  }
  /* The table should be made smaller if a certain threshold is reached,
     left for future implementation. The idea is similar to the method
     used at fnHashInsert to enlarge a table: allocate a new table with
     a smaller size and call fnHashEnum ( ..., fnHashReInsert, ... ). */
  RETURN ( hashDeleted );
} /* fnHashDelete */

/* ----------------------------------------------------------------------- */
size_t DLLEXPORT	fnHashOccupied	( LPHASHTABLE lpHashTable )
{
  PROCEDURE		( fnHashOccupied );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );

  RETURN ( lpHashTable->nOccupied );
} /* fnHashOccupied */

#ifndef	NODEBUG

/* ----------------------------------------------------------------------- */
static void	fnHashPrintEntries	( LPHASHTABLE lpHashTable,
					  BOOL bStringDataField )
{
  register int		i, nSizeOfEntry;
  register LPSTR	lpEntry;
  char			szLink [ 10 ];
  BOOL			bFirstEntry;

  PROCEDURE		( fnHashPrintEntries );
  INITIALIZECOMMON;

  ASSERT ( lpHashTable != NULL );
  ASSERT_NO_ZOMBIE ( lpHashTable );

  nSizeOfEntry	= sizeofHASHENTRY ( lpHashTable->nSizeOfDataField );
  if ( lpHashTable->lpEntries ) {
    for ( i = 0, lpEntry = (LPSTR) lpHashTable->lpEntries,
	  bFirstEntry = TRUE; i < lpHashTable->nElements;
	  i++, lpEntry += nSizeOfEntry ) {
      if ( ((LPHASHENTRY)lpEntry)->lpNext !=
	   (struct HASHENTRYtag FAR *) hashEntryFree ) {
	if ( ((LPHASHENTRY)lpEntry)->lpNext ==
	     (struct HASHENTRYtag FAR *) hashEntryOccupied ) {
	  szLink [ 0 ]	= '\0';
	} else {
	  sprintf ( szLink, "%lu",
		    ((LPHASHENTRY)lpEntry)->lpNext->dwHashKey );
	}
	if ( bFirstEntry ) {
	  bFirstEntry	= FALSE;
	  puts ( ( bStringDataField ) ?
		" Idx      Key     Link Data\n"
		"==========================================================" :
		" Idx      Key     Link    Data\n"
		"==============================" );
	}
	printf ( ( bStringDataField ) ?
		 " %3d %8lu %8.8s %s\n" : " %3d %8lu %8.8s -->0x%lX\n",
		 i, ((LPHASHENTRY)lpEntry)->dwHashKey, szLink, 
		 ( lpHashTable->nSizeOfDataField > 0 ) ?
		 ((LPHASHENTRY)lpEntry)->vUserData : NULL );
      }
    }
  }
  RETURN ( VOID );
} /* fnHashPrintEntries */

/* ----------------------------------------------------------------------- */
void		fnHashDebug	( LPHASHTABLE lpHashTable )
{
  char		szAnswer [ 80 ], szData [ 80 ];
  BOOL		bOwnTable, bContinue, bPrintHeader = TRUE;
  HASHKEY	dwHashKey;
  LPSTR		lpszData;

  PROCEDURE	( fnHashDebug );
  INITIALIZECOMMON;

  bOwnTable	= (BOOL) ( lpHashTable == NULL );
  if ( bOwnTable ) {
    lpHashTable	= HashCreate ( lpHashTable, 0, sizeof ( szData ) );
  }
  bContinue	= TRUE;
  puts ( "\n\nHash table debugging. Select operation on hash table:" );
  fflush ( stderr );
  fflush ( stdout );
  fflush ( stdin );
  while ( bContinue ) {
    if ( bPrintHeader ) {
      printf ( "\n%d element(s), %d max element(s),"
	       " %lu probe(s), %lu hit(s).\n",
	       (int) lpHashTable->nOccupied,
	       (int) lpHashTable->nElements,
	       lpHashTable->dwProbes,
	       lpHashTable->dwHits );
      fputs ( ( bOwnTable ) ?
	      "(I)nsert [<key> [<data>]] (G)et <key> (P)rint"
	      " (D)elete <key> (C)lear (R)eset\n(Q)uit: " :
	      "(I)nsert [<key>] (G)et <key> (P)rint"
	      " (D)elete <key> (C)lear (R)eset\n(Q)uit: ",
	      stdout );
    }
    bPrintHeader	= TRUE;
    szAnswer [ 0 ]	= '\0';
    fgets ( szAnswer, sizeof ( szAnswer ), stdin );
    switch ( szAnswer [ 0 ] ) {
    case 'i': case 'I':
      dwHashKey		= (DWORD) -1;
      if ( bOwnTable ) {
	szData [ 0 ]	= '\0';
	sscanf ( & szAnswer [ 1 ], "%lu%s", &dwHashKey, szData );
	if ( dwHashKey == (DWORD) -1 ) {
	  dwHashKey	= (DWORD) rand () % 1024;
	}
	if ( ! szData [ 0 ] )
	  sprintf ( szData, "item #%lu", dwHashKey );
	printf ( "fnHashInsert ( %lu, %s ) returns %d\n", dwHashKey, szData,
		fnHashInsert ( lpHashTable, dwHashKey, szData ) );
      } else {
	sscanf ( & szAnswer [ 1 ], "%lu", &dwHashKey );
	if ( dwHashKey == (DWORD) -1 ) {
	  dwHashKey	= (DWORD) rand () % 1024;
	}
	printf ( "fnHashInsert ( %lu, NULL ) returns %d\n", dwHashKey,
		fnHashInsert ( lpHashTable, dwHashKey, NULL ) );
      }
      break;
    case 'g': case 'G':
      dwHashKey		= 0;
      sscanf ( & szAnswer [ 1 ], "%lu", &dwHashKey );
      lpszData		= (LPSTR) fnHashGet ( lpHashTable, dwHashKey );
      printf ( ( bOwnTable ) ?
	       "fnHashGet ( %lu ) returns %s\n" :
	       "fnHashGet ( %lu ) returns 0x%lX\n", dwHashKey,
	       ( ! bOwnTable || lpszData ) ?
	       lpszData : "NULL" );
      break;
    case 'p': case 'P':
      fnHashPrintEntries (  lpHashTable, bOwnTable );
      break;
    case 'd': case 'D':
      dwHashKey		= 0;
      sscanf ( & szAnswer [ 1 ], "%lu", &dwHashKey );
      printf ( "fnHashDelete ( %lu ) returns %d\n", dwHashKey,
	       fnHashDelete ( lpHashTable, dwHashKey ) );
      break;
    case 'c': case 'C':
      printf ( "fnHashClear () returns 0x%lX\n",
	       (DWORD) fnHashClear ( lpHashTable ) );
      break;
    case 'r': case 'R':
      lpHashTable->dwProbes	= 0;
      lpHashTable->dwHits	= 0;
      break;
    case '\0': case EOF:
      puts ( "quit" );
    case 'q':  case 'Q':
      bContinue		= FALSE;
      break;
    default:
      bPrintHeader	= FALSE;
      break;
    }
  }
  if ( bOwnTable )
    lpHashTable	= fnHashDestroy ( lpHashTable );
  RETURN ( VOID );
} /* fnHashDebug */

#endif /* #ifndef NODEBUG */

/* ----------------------------------------------------------------------------
| Module initialization / Deinitialization
 --------------------------------------------------------------------------- */
void		fnInitializeHashModule	( void )
{
  PROCEDURE	( fnInitializeHashModule );

  ASSERT ( newsizeEnumerator > newsizeDenominator );

  RETURN ( VOID );
} /* fnInitializeHashModule */

/* ------------------------------------------------------------------------- */
void		fnDeinitializeHashModule	( void )
{
  PROCEDURE	( fnDeinitializeHashModule );

  if ( pdwGlobalPrimeSizeTable != NULL ) {
    Free ( pdwGlobalPrimeSizeTable );
    pdwGlobalPrimeSizeTable	= (LPDWORD) NULL;
  }

  RETURN ( VOID );
} /* fnDeinitializeHashModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
