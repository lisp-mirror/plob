/* -------------------------------------------------------------------------
| Module	hash.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		28.10.93
| Description	Implements hash tables. An entry into this table consists
|		of a hash key and an attached data field. The size of the
|		data field is fixed for each hash table and has to be
|		specified at hash table creation. The hash table grows
|		at reaching its capacity.
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

#ifndef	HASH_H_INCLUDED
#define	HASH_H_INCLUDED

/* -------------------------------------------------------------------------
| Types
 ------------------------------------------------------------------------- */
/* The type of all hash keys, a 32 bit unsigned int: */
typedef	DWORD	HASHKEY, * LPHASHKEY;

/* The (once opaque) hash table type: */
typedef struct HASHTABLEtag	* LPHASHTABLE;

/* Possible results from various hash functions: */
typedef enum {
  hashNotFound,	/* The key has not been found in the hash table. */
  hashFound,	/* The key has been found in the hash table. */
  hashInserted,	/* The key and data have been inserted into the hash table. */
  hashUpdated,	/* The key was found in the hash table; updated data field. */
  hashDeleted	/* The key was deleted from the hash table. */
}	HASHRESULT, * LPHASHRESULT;

/* Return type of enumeration callback function: */
typedef enum {
  hashBreak,	/* Stop enumeration. */
  hashContinue	/* Continue enumeration with next element. */
}	HASHENUM, * LPHASHENUM;
/* Function protoype type for fnHashEnum enumerate callback function: */
typedef HASHENUM ( * LPFNHASHENUM )	( LPHASHTABLE lpHashTable,
					  HASHKEY dwHashKey,
					  LPVOID lpDataField,
					  size_t nSizeOfDataField,
					  LPVOID lpUserData );

/* -------------------------------------------------------------------------
| Internal used structured; placed here for an efficient HashGet ()-macro
| (see below):
 ------------------------------------------------------------------------- */

/* Hash table flags: */
typedef enum {
  /* A flag if the hash table was allocated in hash.c: */
  eHashAllocated	= 0x01
}	HASHFLAGS, * PHASHFLAGS;

/* Hash table entries: */
typedef struct HASHENTRYtag {
  HASHKEY		dwHashKey;
  struct HASHENTRYtag *	lpNext;
  BYTE			vUserData [ 8 ];
}	HASHENTRY, * LPHASHENTRY;

typedef struct HASHTABLEtag {
  HASHFLAGS	nFlags;			/* Hash table flags */
  int		nIterator;		/* Current hash element iterated */
  size_t	nSizeOfDataField;	/* Size of data attached to entry */
  size_t	nOccupied;		/* Number of occupied entries */
  size_t	nElements;		/* Current size of table, prime */
  WORD		wSizeIndex;		/* Index of current size */
  LPHASHENTRY	lpHashEntryLast;	/* Last hash entry 'worked on' */
  LPHASHENTRY	lpEntries;		/* Pointer to hash entries */
  DWORD		dwProbes;		/* Number of probes into hash table */
  DWORD		dwHits;			/* Number of hits */
  LPCSTR	pszFile;		/* File which created the table */
  LPCSTR	pszProc;		/* Proc. which created the table */
  int		nLine;			/* Line in file */
}	HASHTABLE;

/* -------------------------------------------------------------------------
| Global variables used in macros
 ------------------------------------------------------------------------- */
extern LPHASHTABLE		__lpHashTableGet__;
extern HASHKEY			__dwHashKeyGet__;
extern LPVOID			__lpDataField__;

/* -------------------------------------------------------------------------
| Extern function prototypes
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Functions	fnHashCreate, fnHashDestroy
| Arguments	  fnHashCreate: lpHashTable
|		If == NULL, a hash table will be allocated by hash.c,
|		otherwise the passed pointer is interpreted as a user
|		allocated hash table.
|		  fnHasDestroy: lpHashTable
|		The hash table to be freed. It is user responsibility to
|		free any memory occupied by each data field.
|		  nInitialElements
|		The number of elements the hash table can hold initially.
|		Please note that the hash table will increase their size if
|		they reach their capacity.
|		  nSizeOfDataField
|		The size of the data attached to a HASHKEY in bytes.
| Return	A new hash table resp. NULL if free was successfull
| Description	Allocate resp. free a hash table.
 ------------------------------------------------------------------------- */
#define		HashCreate( /* LPHASHTABLE */ lpHashTable,\
			    /* size_t */ nInitialElements, \
			    /* size_t */ nSizeOfDataField ) \
  fnHashCreate ( lpHashTable, nInitialElements, nSizeOfDataField, \
		 __szFile__, __szProc__, __LINE__ )
LPHASHTABLE DLLEXPORT	fnHashCreate	( LPHASHTABLE lpHashTable,
					  size_t nInitialElements,
					  size_t nSizeOfDataField,
					  LPCSTR pszFile,
					  LPCSTR pszProc,
					  int nLine );
LPHASHTABLE DLLEXPORT	fnHashDestroy	( LPHASHTABLE lpHashTable );

/* -------------------------------------------------------------------------
| Function	fnHashResize
| Arguments	  lpHashTable
|		The hash table to resize
|		  nNewElements
|		Number of elements the hash table should be able to hold.
| Return	New size of hash table
| Description	Resizes a hash table.
 ------------------------------------------------------------------------- */
size_t DLLEXPORT	fnHashResize	( LPHASHTABLE lpHashTable,
					  size_t nNewElements );

/* -------------------------------------------------------------------------
| Function	fnHashClear
| Arguments	  lpHashTable
|		The hash table to clear.
| Return	lpHashTable
| Description	Clears a hash table, i.e. all elements are set to 0.
|		The allocated memory is not freed. It is user
|		responsibility to free any memory occupied by the data
|		field.
 ------------------------------------------------------------------------- */
LPHASHTABLE DLLEXPORT	fnHashClear	( LPHASHTABLE lpHashTable );

/* -------------------------------------------------------------------------
| Function	fnHashInsert
| Arguments	  lpHashTable
|		The hash table into which the element should be inserted.
|		  dwHashKey
|		The hash key
|		  lpDataField
|		Pointer to data which should be attached to the key. The
|		referenced data is copied 'by value' and not 'by reference'
|		into the hash entry. If the key is already in the table,
|		the data field is updated. It is user responsibility to
|		free any memory occupied by the data field.
| Return	See description at type HASHRESULT
| Remarks	fnHashInsert is a destructive operation and so invalidates
|		all pointers into the hash table which have been retrieved
|		by [fn]HashGet before the call to fnHashInsert.
| Description	Insert a key with attached data into a hash table.
 ------------------------------------------------------------------------- */

#define	HashInsert( lpHashTable, dwKey, lpDataField )		\
(__lpHashTableGet__=(lpHashTable),				\
 __dwHashKeyGet__=(dwKey),					\
 __lpDataField__=(lpDataField),					\
 (__lpHashTableGet__->lpHashEntryLast&&				\
  __lpHashTableGet__->lpHashEntryLast->dwHashKey==		\
  __dwHashKeyGet__)?						\
 ((__lpHashTableGet__->nSizeOfDataField>0&&			\
   __lpDataField__!=NULL)?					\
  memcpy((LPVOID)__lpHashTableGet__->lpHashEntryLast->vUserData,\
	 __lpDataField__,__lpHashTableGet__->nSizeOfDataField),	\
  hashUpdated:hashUpdated):					\
 fnHashInsert(__lpHashTableGet__,__dwHashKeyGet__,__lpDataField__))

HASHRESULT DLLEXPORT	fnHashInsert	( LPHASHTABLE lpHashTable,
					  HASHKEY dwHashKey,
					  LPCVOID lpDataField );

/* -------------------------------------------------------------------------
| Function	fnHashGet
| Arguments	  lpHashTable
|		The hash table to read an element.
|		  dwHashKey
|		The hash key
| Return	NULL if key wasn't found, otherwise pointer to data field.
|		If the hash entries have no attached data field,
|		((LPVOID)hashFound) is returned.
| Description	Read an element from a hash table.
 ------------------------------------------------------------------------- */

#define	HashGet( lpHashTable, dwKey )				\
(__lpHashTableGet__=(lpHashTable),				\
 __dwHashKeyGet__=(dwKey),					\
 (__lpHashTableGet__->lpHashEntryLast&&				\
  __lpHashTableGet__->lpHashEntryLast->dwHashKey==		\
  __dwHashKeyGet__)?						\
 ((__lpHashTableGet__->nSizeOfDataField>0)?			\
  (LPVOID)__lpHashTableGet__->lpHashEntryLast->vUserData:	\
  (LPVOID)hashFound):						\
 fnHashGet(__lpHashTableGet__,__dwHashKeyGet__))

LPVOID DLLEXPORT	fnHashGet	( LPHASHTABLE lpHashTable,
					  HASHKEY dwHashKey );

/* -------------------------------------------------------------------------
| Function	fnHashEnum
| Arguments	  lpHashTable
|		The hash table to enumerate its entries.
|		  lpfnEnumerate
|		The enumerate callback function, which is called for each
|		hash entry. The function should return hashContinue to
|		continue the enumeration or hashBreak to stop it.
|		  lpUserData
|		Any data which is passed uninterpreted as the last argument
|		to the enumerate callback function.
| Return	Last result of call to enumerate callback function.
| Remarks	Calling fnHashDelete or fnHashInsert on the table currently
|		iterated will have unpredictable results.
| Description	Enumerate all hash entries, i.e. call a callback function
|		for each entry in the hash table.
 ------------------------------------------------------------------------- */
HASHENUM DLLEXPORT	fnHashEnum	( LPHASHTABLE lpHashTable,
					  LPFNHASHENUM lpfnEnumerate,
					  LPVOID lpUserData );

/* -------------------------------------------------------------------------
| Function	fnHashFirst, fnHashNext, fnHashLast
| Arguments	  lpHashTable
|		The hash table to iterate on.
|		  lpdwHashKey
|		*lpdwHashKey will be set to the hash key found in the
|		table.
|		  lplpDataField
|		*lplpDataField will be set to a pointer to the data
|		entry in the hash table.
|		  lpnSizeOfDataField
|		*lpnSizeOfDataField will be set to the size of the
|		memory buffer *lplpDataField points to.
| Return	TRUE if an element was copied to the output arguments
| Remarks	Calling fnHashDelete or fnHashInsert on the table currently
|		iterated will have unpredictable results.
| Description	Iterate over all hash entries.
|		fnHashLast() marks a hash table as being completely
|		iterated through.
 ------------------------------------------------------------------------- */
BOOL DLLEXPORT		fnHashFirst	( LPHASHTABLE lpHashTable,
					  LPHASHKEY lpdwHashKey,
					  LPVOID * lplpDataField,
					  size_t * lpnSizeOfDataField );
BOOL DLLEXPORT		fnHashNext	( LPHASHTABLE lpHashTable,
					  LPHASHKEY lpdwHashKey,
					  LPVOID * lplpDataField,
					  size_t * lpnSizeOfDataField );
BOOL DLLEXPORT		fnHashLast	( LPHASHTABLE lpHashTable );

/* -------------------------------------------------------------------------
| Function	fnHashDelete
| Arguments	  lpHashTable
|		The hash table where the element should be deleted.
|		  dwHashKey
|		The hash key of the element to delete.
| Return	See description at type HASHRESULT
| Remarks	fnHashDelete is a destructive operation and so invalidates
|		all pointers into the hash table which have been retrieved
|		by [fn]HashGet before the call to fnHashDelete.
| Description	Delete an element from a hash table.
 ------------------------------------------------------------------------- */
HASHRESULT DLLEXPORT	fnHashDelete	( LPHASHTABLE lpHashTable,
					  HASHKEY dwHashKey );

/* -------------------------------------------------------------------------
| Function	fnHashOccupied
| Arguments	  lpHashTable
|		The hash table to return the number of occupied elements.
| Return	Number of occupied elements.
| Description	Return the number of occupied elements
 ------------------------------------------------------------------------- */
size_t DLLEXPORT	fnHashOccupied	( LPHASHTABLE lpHashTable );

#ifndef	NODEBUG
/* -------------------------------------------------------------------------
| Function	fnHashDebug
| Description	Debugging function for this module. Creates a hash table
|		if passed lpHashTable is NULL and lets user insert, print,
|		delete, ... entries.
 ------------------------------------------------------------------------- */
void		fnHashDebug	( LPHASHTABLE lpHashTable );
#else
#define		fnHashDebug( lpHashTable )
#endif /* #ifndef NODEBUG */

/* ----------------------------------------------------------------------------
| Module initialization / Deinitialization
 --------------------------------------------------------------------------- */
void		fnInitializeHashModule		( void );
void		fnDeinitializeHashModule	( void );

#endif	/* HASH_H_INCLUDED */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
