/* -------------------------------------------------------------------------
| Module	plobheap.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94
| Description	Foreign language interface to PLOB heaps.
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

#if defined(LISP)
;;;; -------------------------------------------------------------------------
;;;; For further comments look into file plobheap.h
;;;; -------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#if defined(C2C)

enum {
  /* Block size for transaction logs: */
  nTransactionLogBlockSize	= 65536 /* bytes */
};

/* The profiler showed that calls to ftell () to get the actual stream
   position are rather expensive; so I defined my own file structure
   here which keeps track of the actual file position. The functions
   for using the FILEPOS structure are fnTractOpen, fnTractClose,
   fnRead, fnWrite and fnSeek: */
typedef struct {
  LPFILE	lpFile;
  long		nPos;
  long		nMaxPos;	/* Max. position encountered up to now */
  LPVOID	pBuffer;	/* Buffer used with setvbuf onto stream */
}	FILEPOS, * LPFILEPOS;

/* -------------------------------------------------------------------------
| PLOB heap instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oSelf;			/* The heap objid itself */
  OBJID		oTractId;		/* Current transaction id */
  OBJID		oTractLogId;		/* Transaction log id */
  OBJID		oLockStore;		/* Mode of store lock, if set */
  OBJID		oGCcounter;		/* GC counter at transaction start */
  OBJID		oIdxWritten;		/* Number of records written to Idx */
  OBJID		oUser;			/* User owning the heap */
  OBJID		oMachine;		/* Machine where oUser resides */
  OBJID		oDescription;		/* String describing the heap obj */
  /* --- Persistent values: ---------------------------------------------- */
  time_t	timeTract;		/* Time the tr. log was opened */
  FILEPOS	Idx;			/* Transaction log index stream */
  FILEPOS	Log;			/* Transaction log heap stream */
  HASHTABLE	TractIdxTable;		/* Objids touched in a transaction */
}	PLOBHEAP, * LPPLOBHEAP;

#define	heapp( oSelf )	(typetagof(oSelf)==eshHeapTag)

void			fnInitCommonHeapModule		( void );
void			fnInitializeHeapModule		( void );
void			fnDeinitializeHeapModule	( void );
void			fnDeinitCommonHeapModule	( void );

#endif /* #if defined(C2C) */

/* Transaction ids: */
DefineType ( /* base type: */ FIXNUM, /* defined type: */ TRACTID );

DefineConstant ( NULLTRACTID, "+null-transaction-id+", 0,
		 "NULL (i.e. always invalid) transaction-id." );

BeginEnum ( SHHEAPIDX )
  enumerator ( eshHeapIdxSelf, "+heap-location-self+", 0,
	       "Index of plob heap heap field." )
  and
  enumerator ( eshHeapIdxTractId, "+heap-location-tract-id+", 1,
	       "Index of plob heap transaction id field." )
  and
  enumerator ( eshHeapIdxTractLogId, "+heap-location-tract-log-id+", 2,
	       "Index of plob heap transaction log id field." )
EndEnum ( SHHEAPIDX );

DefineConstant ( eshHeapTag, "+heap-type-tag+", hex ( 90 ),
		 "Type tag for plob objects of type HEAP." );

/* -------------------------------------------------------------------------
| Transaction-Support
 ------------------------------------------------------------------------- */
#if ! defined(LISP)	/* server: */
DefineFunction ( TRACTID,
		 fnServerTransactionBegin, "c-sh-begin-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bIgnoreError ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( TRACTID,
		 fnClientTransactionBegin, "c-sh-begin-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bIgnoreError ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( TRACTID,
		 fnServerTransactionCancel, "c-sh-cancel-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bIgnoreError ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( TRACTID,
		 fnClientTransactionCancel, "c-sh-cancel-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bIgnoreError ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( TRACTID,
		 fnServerTransactionEnd, "c-sh-end-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bIgnoreError ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( TRACTID,
		 fnClientTransactionEnd, "c-sh-end-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bIgnoreError ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( voidResult,
		 fnServerTransactionFlush, "c-sh-flush-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( voidResult,
		 fnClientTransactionFlush, "c-sh-flush-transaction",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( TRACTID,
		 fnServerDbTransactionP, "c-sh-in-transaction-p",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( TRACTID, value_in, nTractId ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( TRACTID,
		 fnClientDbTransactionP, "c-sh-in-transaction-p",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( TRACTID, value_in, nTractId ) ) );
#endif	/* ! RPC */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
