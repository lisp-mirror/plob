/* -------------------------------------------------------------------------
| Module	splobintern.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
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

#include	"plobintern.h"

/* -------------------------------------------------------------------------
| Error message formats
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR const char	szAlreadyLocked []	/* =
"There is already an atomic lock on %s." */;

extern DLLEXPORTVAR const char	szCantAddress []	/* =
"Can't get real address of object referenced by objid %d." */;

extern DLLEXPORTVAR const char	szCantIndex []	/* =
"Can't index object %s with index %d." */;

extern DLLEXPORTVAR const char	szClobberLock []	/* =
"Clobber the old lock." */;

extern DLLEXPORTVAR const char	szLockFailed []		/* =
"Atomic lock on %s failed." */;

extern DLLEXPORTVAR const char	szNotLocked []		/* =
"There is no atomic lock on %s." */;

extern DLLEXPORTVAR const char	szNotOpen []		/* =
"Stable heap isn't open." */;

/* -------------------------------------------------------------------------
| Masks & macros for handling of postore lock words for low-low-low-level
| atomic locking
 ------------------------------------------------------------------------- */
/* Mask to check if lock word is set: */
extern DLLEXPORTVAR const int	nAtomicLockSetMask	/* = 0x80000000 */;
/* Mask to get the lock value from a lock word: */
extern DLLEXPORTVAR const int	nAtomicLockValMask	/* = 0x00FFFFFF */;

/* -------------------------------------------------------------------------
| Low-low-low-level atomic locking
 ------------------------------------------------------------------------- */
extern DLLEXPORTVAR int		nGlobalTouched		/* = 0 */;
extern DLLEXPORTVAR OBJID	__oAtomicLock__;
extern DLLEXPORTVAR LPOBJID	__lpAtomicLock__;
extern DLLEXPORTVAR OBJID	__oAtomicUnlock__;
extern DLLEXPORTVAR LPOBJID	__lpAtomicUnlock__;

/* 1998/02/24 HK: Debug: */
#if 0

#define			AtomicLock( oToLock, oLockBy )		\
(__oAtomicLock__=(oToLock),					\
 _ASSERT(!immediatep(__oAtomicLock__),				\
	 "! immediatep ( " #oToLock " )"),			\
 ASSERT_ObjId_is_valid(__oAtomicLock__),			\
 nGlobalTouched++,						\
 __lpAtomicLock__=(LPOBJID)SH_key_to_address(__oAtomicLock__),	\
 (__lpAtomicLock__[eshSHvectorIdxLock]&nAtomicLockSetMask)?	\
 (CERROR((szClobberLock,szAlreadyLocked,			\
	  fnPrintObject(__oAtomicLock__,NULL,0))),		\
  __lpAtomicLock__):						\
 ((SH_set_lock(__oAtomicLock__)&nAtomicLockSetMask)?		\
  __lpAtomicLock__:						\
  (CERROR((szClobberLock,szLockFailed,				\
	   fnPrintObject(__oAtomicLock__,NULL,0))),		\
   __lpAtomicLock__)))

#define			AtomicUnlock( oLocked, oLockedBy )	\
(__oAtomicUnlock__=(oLocked),					\
 _ASSERT(!immediatep(__oAtomicUnlock__),			\
	 "! immediatep ( " #oLocked " )"),			\
 ASSERT_ObjId_is_valid(__oAtomicUnlock__),			\
 __lpAtomicUnlock__=						\
 (LPOBJID)SH_key_to_address(__oAtomicUnlock__),			\
 (__lpAtomicUnlock__[eshSHvectorIdxLock]&nAtomicLockSetMask)?	\
 (__lpAtomicUnlock__[eshSHvectorIdxLock]&=~nAtomicLockSetMask,	\
  TRUE):							\
 (CERROR((szClobberLock,szNotLocked,				\
	  fnPrintObject(__oAtomicUnlock__,NULL,0))),		\
  FALSE))

#else

#define			AtomicLock( oToLock, oLockBy )		\
fnAtomicLock ( (oToLock), (oLockBy), __szFile__, __szProc__, __LINE__ )

#define			AtomicUnlock( oLocked, oLockedBy )	\
fnAtomicUnlock ( (oLocked), (oLockedBy), __szFile__, __szProc__, __LINE__ )

#endif

/* ----------------------------------------------------------------------- */
LPOBJID	DLLEXPORT 	fnAtomicLock		( OBJID oToLock,
						  OBJID oLockBy,
						  LPCSTR lpszFile,
						  LPCSTR lpszProc,
						  int nLine );

BOOL DLLEXPORT		fnAtomicUnlock		( OBJID oLocked,
						  OBJID oLockedBy,
						  LPCSTR lpszFile,
						  LPCSTR lpszProc,
						  int nLine );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
