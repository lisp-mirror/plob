/* -------------------------------------------------------------------------
| Module	cplobheap.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB client header file:
|		Macros and functions for usage by the PLOB client
| The client module cplobheap does some caching of persistent objects
| referenced from the LISP client code. The idea is that transferring
| each single slot of an object by an extra RPC call is rather expensive.
| Instead, the slots of all objects are transferred once into the client's
| cache when the object gets a read- or a write lock for all slots of the
| object by the client (since these object's state can't be changed by
| anyone else). At the end of transaction, all locks are released by the
| server; the end-of-transaction call is trapped by the client and will
| delete all cahced objects too.
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
 ------------------------------------------------------------------------- */

#include	"plobheap.h"

/* -------------------------------------------------------------------------
| Types
 ------------------------------------------------------------------------- */

/* Cached data associated with a persistent object: */
typedef struct {
  /* The number of heaps referencing the object: */
  int		nReferences;
  /* The heap for which the object is write-locked: */
  SHORTOBJID	oHeap;
  /* The number of changes done to the object's state: */
  int		nChanges;
  /* The type tag of the object: */
  SHTYPETAG	nTypeTag;
  /* The number of slots in the object: */
  int		nSlots;
  /* A pointer to the object's slot states: */
  LPOBJID	poSlots;
  /* A pointer to the object's slot type tags: */
  LPSHTYPETAG	pnTypeTags;
  /* The type of values in the object: */
  SHTYPETAG	nTypeTagValues;
  /* The number of values in the object in elements: */
  int		nValues;
  /* A pointer to the object's values: */
  void		*pValues;
  /* A private buffer (with variable length) holding internal
     informations, i.e. the pointers declared above point into
     bBuffer: */
  BYTE		bBuffer [ 4 ];
}	OBJECTCACHE, * POBJECTCACHE;

/* Cached data associated with a persistent heap: */
typedef struct {
  /* The current transaction id: */
  OBJID		onTractId;
  /* The current lock mode of the heap w.r.t. level store: */
  OBJID		onLockStore;
  /* A hash table with short objids of the objects locked by the
     heap. Key is a short objid, data an instance of HEAPOBJECTCACHE: */
  HASHTABLE	Objects;
}	HEAPCACHE, * PHEAPCACHE;

/* Cached data associated with a persistent heap joined with a
   persistent object: */
typedef struct {
  /* The old lock mode of the object w.r.t. level vector. `Old' in
     this sense means the lock mode known to the LISP layer: */
  SHLOCK	nVectorLockOld;
  /* The current lock mode of the object w.r.t. level vector: */
  SHLOCK	nVectorLockNow;
  /* Counters for number of slots resp. values read from resp. written
     to. The object is removed from the cache if it was completely
     read resp. written: */
  unsigned int	nSlotsRead;
  unsigned int	nSlotsWritten;
  unsigned int	nValuesRead;
  unsigned int	nValuesWritten;
  /* A pointer to the cached object: */
  POBJECTCACHE	pObjectCache;
}	HEAPOBJECTCACHE, * PHEAPOBJECTCACHE;

/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */

/* If bGlobalDoCaching is FALSE, no caching will be done on the client
   side (switching off caching was a kind of emergency break when
   debugging the code ;-) bGlobalDoCaching must be only changed when
   there is guranteed no open session between the server and the
   client; this means it is always possible to set it at module
   startup (and almost never after the startup): */
extern BOOL		bGlobalDoCaching	/* = TRUE */;

extern const char	szFormatNotOpened []	/* = */
/* "Heap %s wasn't opened from this process." */;

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */

/* Close all heaps opened by the client: */
void		fnHeapCloseAll	( void );

/* Allocate a cache for the heap: */
PHEAPCACHE	fnCacheCreate	( SHORTOBJID	oShortObjIdHeap );

/* Destroy the cache associated with oShortObjIdHeap;
   passing NULLOBJID means to destroy all caches of all heaps.
   Returns the number of remaining heaps: */
FIXNUM		fnCacheDestroy	( SHORTOBJID	oShortObjIdHeap );

/* Clear the cache associated with oShortObjIdHeap;
   passing NULLOBJID means to clear all caches of all heaps.
   Returns the number of heaps: */
FIXNUM		fnCacheClear	( SHORTOBJID	oShortObjIdHeap );

/* Create an object in the cache: */
PHEAPOBJECTCACHE fnCacheCreateObject( SHORTOBJID	oShortObjIdHeap,
				      SHORTOBJID	oShortObjId,
				      SHTYPETAG		nTypeTag,
				      unsigned int	nSlots,
				      SHTYPETAG		nTypeTagValues,
				      unsigned int	nValues,
				      SHLOCK		nLockNow );

/* Read the objects from hPeek into the client's cache: */
FIXNUM		fnCacheInsert	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjIdLockBy,
				  HPEEK		hPeek,
				  FIXNUM	nObjIdWords );

/* Delete oShortObjId from the cache associated with oShortObjIdHeap;
   passing NULLOBJID for oShortObjIdHeap means to delete the object
   from all caches of all heaps, passing NULLOBJID for oShortObjId
   means to delete all objects from the heap's cache: */
HASHRESULT	fnCacheDelete	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjId );

/* Flush the cache associated with oShortObjIdHeap to the Stable Heap;
   passing NULLOBJID for oShortObjIdHeap means to flush all objects,
   passing an objid for oShortObjId means only to flush the object: */
FIXNUM		fnCacheFlush	( SHORTOBJID	oShortObjIdHeap,
				  SHORTOBJID	oShortObjId );

/* Read the heap cache referenced by oShortObjIdHeap: */
PHEAPCACHE	fnCacheGetHeap	( SHORTOBJID	oShortObjIdHeap );

/* Read the object referenced by oShortObjId from the cache associated
   with oShortObjIdHeap; if the object wasn't found in the cache, NULL
   is returned: */
PHEAPOBJECTCACHE fnCacheGetObject( SHORTOBJID	oShortObjIdHeap,
				   SHORTOBJID	oShortObjId );

/* Check if the object oShortObjId has been completely read
   resp. written; if it is, the object is flushed and deleted from the
   heap oShortObjIdHeap: */
BOOL		fnCacheCompletedP( SHORTOBJID	oShortObjIdHeap,
				   SHORTOBJID	oShortObjId );

/* Check if a lock of nLock is set onto object oShortObjIdToLock: */
SHLOCK		fnCacheLockP	( SHORTOBJID	oShortObjIdLockBy,
				  SHORTOBJID	oShortObjIdToLock,
				  SHLOCK	nLock );

/* Try to place lock nLock onto object oShortObjIdToLock
   only using the cache: */
SHLOCK		fnCacheInsertLock( SHORTOBJID	oShortObjIdLockBy,
				   SHORTOBJID	oShortObjIdToLock,
				   SHLOCK	nLock );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
