/* -------------------------------------------------------------------------
| Module	ploblock.h
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		11.1.94
| Description	Foreign language interface to PLOB heap locking.
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

#if defined(LISP)
;;;; --------------------------------------------------------------------------
;;;; For further comments look into file ploblock.h
;;;; --------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif
#if defined(C2C)

/* -------------------------------------------------------------------------
| Lock modes
 ------------------------------------------------------------------------- */
typedef enum {
  lmNothing		= -1,				/* -1 */ 
  lmMin,						/*  0 */

  /* -----------------------------------------------------------
  | `Real' locks:
   ---------------------------------------------------------- */
  lmReadOnly		= lmMin,			/*  0 */
  lmRead,						/*  1 */
  lmWrite,						/*  2 */

  lmIntent,						/*  3 */

  /* -----------------------------------------------------------
  | Intent locks:
   ---------------------------------------------------------- */
  lmReadOnlyIntent	= lmReadOnly	+ lmIntent,	/*  3 */
  lmReadIntent		= lmRead	+ lmIntent,	/*  4 */
  lmWriteIntent		= lmWrite	+ lmIntent,	/*  5 */

  lmMax			= lmWriteIntent,		/*  5 */
  lmNumberOf		= lmMax + 1			/*  6 */
}	LOCKMODE, * LPLOCKMODE;

/* -------------------------------------------------------------------------
| Header struct common to SUMLOCK and ONELOCK
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	OfPlob;
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oSelf;		/* The lock objid itself */
  OBJID		oNext;		/* Next lock object */
  OBJID		onTotalHold;	/* Number of total lock objects */
  OBJID		oTotalHolds	[ lmNumberOf ];
}	LOCKHEADER, * LPLOCKHEADER;

/* -------------------------------------------------------------------------
| PLOB summarizing lock instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  LOCKHEADER	Header;
  /* --- Persistent references: ------------------------------------------ */
  OBJID		onIndex;	/* Index of the element locked */
  OBJID		onLockMask;	/* Sum of all lock masks */
  OBJID		onActiveHold;	/* Number of active lock objects */
  OBJID		oActiveHolds	[ lmNumberOf ];
  OBJID		oOneLocks;	/* List with ONELOCK objects */
  /* --- Persistent values: ---------------------------------------------- */
}	SUMLOCK, * LPSUMLOCK;

/* -------------------------------------------------------------------------
| PLOB single lock instance type - lock info per each locking object
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  LOCKHEADER	Header;
  /* --- Persistent references: ------------------------------------------ */
  /* --- Persistent values: ---------------------------------------------- */
}	ONELOCK, * LPONELOCK;

/* -------------------------------------------------------------------------
| PLOB wait queue entry instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oSelf;
  OBJID		onPriority;
  OBJID		onLockMode;
  OBJID		oOneLock;
  OBJID		oNext;
  /* --- Persistent values: ---------------------------------------------- */
}	QUEUENTRY, * LPQUEUENTRY;

void			fnInitCommonLockModule		( void );
void			fnInitializeLockModule		( void );
void			fnDeinitializeLockModule	( void );
void			fnDeinitCommonLockModule	( void );

#endif /* #if defined(C2C) */

DefineConstant ( eshSumLockTag, "+sum-lock-tag+", hex ( A8 ),
		 "Type tag for plob objects of type summarizing lock." );

DefineConstant ( eshOneLockTag, "+one-lock-tag+", hex ( B0 ),
		 "Type tag for plob objects of type single lock." );

DefineConstant ( eshQueueEntryTag, "+queue-entry-tag+", hex ( D0 ),
		 "Type tag for plob objects of type queue entry." );

DefineFunction ( FIXNUM,
		 fnClientLockPrint, "c-sh-lock-print",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortSelf )
		   and
		   argument ( SHLOCK, value_in, nLevel )
		   and
		   argument ( NUMERICSTDSTREAM, value_in, nStdStream ) ) );

DefineFunction ( SHLOCK,
		 fnClientTransactionLockGet, "c-sh-get-lock",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockedByP )
		   and
		   argument ( SHLOCK, value_in, nLevelP )
		   and
		   argument ( SHORTOBJID, value_in, oShortLockedP )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagLockedP )
		   and
		   argument ( FIXNUM, value_in, nIndexP ) ) );

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerTransactionLockInsert, "c-sh-insert-lock",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		   and
		   argument ( SHLOCK, value_in, nLock )
		   and
		   argument ( SHORTOBJID, value_in, oShortToLock )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagToLock )
		   and
		   argument ( FIXNUM, value_in, nIndex )
		   and	/* Additional output for client caching: */
		   argument ( FIXNUM, value_out, phPeek )
		   and
		   argument ( FIXNUM, value_out, pnObjIdWords ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientTransactionLockInsert, "c-sh-insert-lock",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		   and
		   argument ( SHLOCK, value_in, nLock )
		   and
		   argument ( SHORTOBJID, value_in, oShortToLock )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagToLock )
		   and
		   argument ( FIXNUM, value_in, nIndex ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerTransactionLockSet, "c-sh-set-lock",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		   and
		   argument ( SHLOCK, value_in, nLock )
		   and
		   argument ( SHORTOBJID, value_in, oShortToLock )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagToLock )
		   and
		   argument ( FIXNUM, value_in, nIndex )
		   and	/* Additional output for client caching: */
		   argument ( FIXNUM, value_out, phPeek )
		   and
		   argument ( FIXNUM, value_out, pnObjIdWords ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientTransactionLockSet, "c-sh-set-lock",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockBy )
		   and
		   argument ( SHLOCK, value_in, nLock )
		   and
		   argument ( SHORTOBJID, value_in, oShortToLock )
		   and
		   argument ( SHTYPETAG, value_in, nTypeTagToLock )
		   and
		   argument ( FIXNUM, value_in, nIndex ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerTransactionUnlock, "c-sh-unlock",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		   and
		   argument ( SHLOCK, value_in, nLock )
		   and
		   argument ( SHORTOBJID, value_in, oShortToUnlock )
		   and
		   argument ( FIXNUM, value_in, nIndex )
		   and	/* Additional output for client caching: */
		   argument ( SHLOCK, value_out,
			      pnVectorLockNow ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnShortUnlock, "c-sh-unlock",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		   and
		   argument ( SHLOCK, value_in, nLock )
		   and
		   argument ( SHORTOBJID, value_in, oShortToUnlock )
		   and
		   argument ( FIXNUM, value_in, nIndex ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerTransactionUnlockAll, "c-sh-unlock-all",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		   and
		   argument ( SHORTOBJID, value_in, oShortToUnlock ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnShortUnlockAll, "c-sh-unlock-all",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockedBy )
		   and
		   argument ( SHORTOBJID, value_in, oShortToUnlock ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerTransactionUnlockAllAll, "c-sh-unlock-all-all",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockedBy ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnShortUnlockAllAll, "c-sh-unlock-all-all",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdLockedBy ) ) );
#endif	/* ! RPC */

BeginEnum ( READONLYMODE )
  enumerator ( eshReadOnlyP, "+read-only-p+", -1,
	       "Get current read-only mode." )
  and
  enumerator ( eshReadWrite, "+read-write+", 0,
	       "Set current mode of object to read-write." )
  and
  enumerator ( eshReadOnly, "+read-only+", 1,
	       "Set current mode of object to read-only." )
EndEnum ( READONLYMODE );

DefineFunction ( BOOL,
		 fnShortMakeReadOnly, "c-sh-read-only",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortSelf )
		   and
		   argument ( READONLYMODE, value_in, nReadOnlyP ) ) );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
