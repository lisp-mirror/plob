/* -------------------------------------------------------------------------
| Module	plobroot.h
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		11.1.94
| Description	Foreign language interface to PLOB root object.
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
;;;; For further comments look into file plobroot.h
;;;; --------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#include	"plobconst.h"
#include	"plobversion.h"

#if defined (C2C)

void			fnInitCommonRootModule		( void );
void			fnInitializeRootModule		( void );
void			fnDeinitializeRootModule	( void );
void			fnDeinitCommonRootModule	( void );

#define	rootp( oObjId )		(typetagof(oObjId)==eshRootTag)

int DLLEXPORT		fnGetNextTractId	( void );

extern OBJID		oFreeListCache		/* = NULLOBJID */;
#define			GetRootFreeList()	\
((oFreeListCache)?oFreeListCache:fnGetRootFreeList())
OBJID DLLEXPORT		fnGetRootFreeList	( void );

OBJID DLLEXPORT		fnSetRootFreeList	( OBJID oFreeList );

extern OBJID		oRootLockQueueCache	/* = NULLOBJID */;
#define			GetRootLockQueue()	\
((oRootLockQueueCache)?oRootLockQueueCache:fnGetRootLockQueue())
OBJID DLLEXPORT		fnGetRootLockQueue	( void );

extern OBJID		oRootLockCache		/* = NULLOBJID */;
#define			GetRootLock()	\
((oRootLockCache)?oRootLockCache:fnGetRootLock())
OBJID DLLEXPORT		fnGetRootLock		( void );

OBJID DLLEXPORT		fnSetRootLock		( OBJID oLock );

int DLLEXPORT		fnIncGCcounter		( void );

extern int		nGCcounterCache		/* = -1 */;
#define			GetGCcounter()		\
((nGCcounterCache>=0)?nGCcounterCache:fnGetGCcounter())
int DLLEXPORT		fnGetGCcounter		( void );

LPCSTR DLLEXPORT	fnGetVersionString ( int	nVersion,
					     LPSTR	pszVersion,
					     size_t	nSizeOfVersion );
#define		GetVersionString( nVersion, szVersion ) \
fnGetVersionString ( (nVersion), (szVersion), sizeof ( szVersion ) )

LPCSTR DLLEXPORT	fnGetVersionDateString	( void );
LPCSTR DLLEXPORT	fnGetCopyrightString	( void );
LPCSTR DLLEXPORT	fnGetEmailString	( void );
LPCSTR DLLEXPORT	fnGetCompileDateString	( void );
LPCSTR DLLEXPORT	fnGetCompileTimeString	( void );
LPCSTR DLLEXPORT	fnGetCompileOpsysString	( void );

DefineConstant ( PlobVersion, "+plob-version+", PLOBVERSION,
		 "PLOB version number." );

/* -------------------------------------------------------------------------
| PLOB root instance type
 ------------------------------------------------------------------------- */

/* Stable heap root object type. This is the first object referenced
   in a stable heap by the built-in postore root object: */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;		/* PLOB header information */
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oRootUser;	/* User's root object */
  OBJID		oPlobVersion;	/* Version number which created the root */
  OBJID		oSelf;		/* The PLOB-root objid itself */
  /* Up to here the PLOBROOT type layout MUST NOT BE CHANGED! */
  OBJID		oLastTractId;	/* Last transaction id */
  OBJID		oFreeList;	/* List with recyclable obj */
  OBJID		oLockQueue;	/* Queue with lock requests */
  OBJID		oLock;		/* Lock object for whole stable heap */
  OBJID		oGCcounter;	/* Garbage collection counter */
  OBJID		oUsers;		/* BTree with all known users */
  OBJID		oGroups;	/* BTree with all known groups */
  OBJID		oMachines;	/* BTree with all known machines */
  OBJID		oSessions;	/* BTree with all active sessions */
  OBJID		oAdminUser;	/* Admin user */
  OBJID		oAdminMach;	/* Admin machine */
  /* --- Persistent values: ---------------------------------------------- */
}	PLOBROOT, * LPPLOBROOT;

/* -------------------------------------------------------------------------
| PLOB user instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;		/* PLOB header information */
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oszName;	/* The users' name */
  OBJID		onID;		/* The users' UID */
}	PLOBUSER, * LPPLOBUSER;

/* -------------------------------------------------------------------------
| PLOB machine instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;		/* PLOB header information */
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oszName;	/* The machine's name */
  OBJID		onAddr [ 4 ];	/* The machine's Internet IP-Adress */
  OBJID		oLoginP;	/* A flag if the machine may login */
}	PLOBMACH, * LPPLOBMACH;

#elif defined(LISP)

(defparameter +plob-version+
              parse_integer(STRINGINIZE(PLOBVERSION) keyword_radix 10)
              "PLOB version number")

#endif /* #if defined (C2C) */

DefineConstant ( eshRootTag, "+root-type-tag+", hex ( 88 ),
		 "Type tag for plob objects of type root." );

DefineConstant ( eshUserTag, "+user-type-tag+", hex ( 100 ),
		 "Type tag for plob objects of type user." );

DefineConstant ( eshMachTag, "+machine-type-tag+", hex ( 110 ),
		 "Type tag for plob objects of type machine." );

DefineFunction ( SHORTOBJID,
		 fnClientDbReadRoot, "c-sh-read-root",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );

#if ! defined(LISP)	/* server: */
DefineFunction ( SHORTOBJID,
		 fnServerDbWriteRoot, "c-sh-write-root",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientDbWriteRoot, "c-sh-write-root",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjId ) ) );
#endif	/* ! RPC */

DefineFunction ( SHORTOBJID,
		 fnClientDbSessions, "c-sh-sessions",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );

BeginEnum ( GETVERSION )
  enumerator ( esvDatabase, "+get-database-version+", 1,
	       "Get the version number of the currently opened database." )
  and
  enumerator ( esvServerCode, "+get-server-code-version+", 2,
	       "Get the version number of the server's code." )
  and
  enumerator ( esvClientCcode, "+get-client-c-code-version+", 3,
	       "Get the version number of the client's C code." )
  and
  enumerator ( esvClientLispCode, "+get-client-lisp-code-version+", 4,
	       "Get the version number of the client's LISP code." )
#if defined (C2C)
  and
  esvVersionMin	= esvDatabase,
  esvVersionMax	= esvClientLispCode,
  esvVersionLen	= esvVersionMax - esvVersionMin + 1
#endif
EndEnum ( GETVERSION );

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerGetVersion, "c-sh-get-version",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( GETVERSION, value_in, eWhat ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( FIXNUM,
		 fnClientGetVersion, "c-sh-get-version",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( GETVERSION, value_in, eWhat ) ) );
#endif	/* ! RPC */

BeginEnum ( MACHLOGINP )
  enumerator ( eshLoginIgnore, "+login-ignore+", 0,
	       "Leave it unspecified if the login from the machine\
 is allowed or denied." )
  and
  enumerator ( eshLoginAllow, "+login-allow+", 1,
	       "Allow logins from machine." )
  and
  enumerator ( eshLoginDeny, "+login-deny+", 2,
	       "Deny logins from machine." )
  and
  enumerator ( eshLoginGet, "+login-get-flag+", 3,
	       "Get login allow/deny flag of machine." )
EndEnum ( MACHLOGINP );

DefineFunction ( SHORTOBJID,
		 fnClientDbCreateMachine, "c-sh-create-machine",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( VECTOR ( int, 4 ), vector_in, nAddr )
		   and
		   argument ( MACHLOGINP, value_in, eLoginP ) ) );

DefineFunction ( MACHLOGINP,
		 fnClientMachineLoginP, "c-sh-machine-loginp",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortMachine )
		   and
		   argument ( MACHLOGINP, value_in, eLoginP ) ) );

DefineFunction ( BOOL,
		 fnClientMachineAddr, "c-sh-machine-addr",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortMachine )
		   and
		   argument ( VECTOR ( int, 4 ), vector_out,
			      pnAddr ) ) );

DefineFunction ( SHORTOBJID,
		 fnClientDbMachineSearch, "c-sh-search-machine",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( VECTOR ( int, 4 ), vector_in, nAddr ) ) );

DefineFunction ( SHORTOBJID,
		 fnClientDbMachineDelete, "c-sh-delete-machine",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( VECTOR ( int, 4 ), vector_in, nAddr ) ) );

DefineFunction ( SHORTOBJID,
		 fnClientDbMachineInsert, "c-sh-insert-machine",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortMachine ) ) );

DefineFunction ( SHORTOBJID,
		 fnClientDbMachines, "c-sh-machines",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) );

/* -------------------------------------------------------------------------
|  Here are the indices into the LISP root object:
 ------------------------------------------------------------------------- */

/* Predefined structure: lisproot */

BeginEnum ( LISPROOTIDX )
  enumerator ( eshLispRootIdxVersion,
	       "+root-location-version+",
	       add ( STRUCTIDXOFFSET, 0 ),
	       "Index into LISP root object containing the\
 version number actual when the root was formatted.\
 It is a fixnum." )
  and
  enumerator ( eshLispRootIdxLispFormat,
	       "+root-location-lisp-formatted+",
	       add ( STRUCTIDXOFFSET, 1 ),
	       "Index into LISP root object containing the\
 LISP system which formatted the root. It is a persistent keyword symbol." )
  and
  enumerator ( eshLispRootIdxTimeFormat,
	       "+root-location-time-formatted+",
	       add ( STRUCTIDXOFFSET, 2 ),
	       "Index into LISP root object containing the\
 time the LISP root was formatted. Its format is Common LISP Universal Time,\
 i.e. a bignum." )
  and
  enumerator ( eshLispRootIdxNamePkgTable,
	       "+root-location-name->package-table+",
	       add ( STRUCTIDXOFFSET, 3 ),
	       "Index into LISP root object containing the\
 table mapping names (i.e. strings) to package objects." )
  and
  enumerator ( eshLispRootIdxSymbolClassTable,
	       "+root-location-symbol->class-table+",
	       add ( STRUCTIDXOFFSET, 4 ),
	       "Index into LISP root object containing the\
 table mapping symbols to structure resp. class descriptions." )
  and
  enumerator ( eshLispRootIdxStructDescr,
	       "+root-location-structure-description+",
	       add ( STRUCTIDXOFFSET, 5 ),
	       "Index into LISP root object containing the\
 structure description object." )
  and
  enumerator ( eshLispRootIdxStructSlotDescr,
	       "+root-location-structure-slot-description+",
	       add ( STRUCTIDXOFFSET, 6 ),
	       "Index into LISP root object containing the\
 structure slot description object." )
  and
  enumerator ( eshLispRootIdxPkgDescr,
	       "+root-location-package-description+",
	       add ( STRUCTIDXOFFSET, 7 ),
	       "Index into LISP root object containing the\
 package description object." )
  and
  enumerator ( eshLispRootIdxPlobDescr,
	       "+root-location-plob-description+",
	       add ( STRUCTIDXOFFSET, 8 ),
	       "Index into LISP root object containing the\
 plob description object." )
  and
  enumerator ( eshLispRootIdxClassDescr,
	       "+root-location-class-description+",
	       add ( STRUCTIDXOFFSET, 9 ),
	      "Index into LISP root object containing the\
 class description object." )
  and
  enumerator ( eshLispRootIdxSlotDescr,
	       "+root-location-slot-description+",
	       add ( STRUCTIDXOFFSET, 10 ),
	       "Index into LISP root object containing the\
 slot description object." )
  and
  enumerator ( eshLispRootIdxDirSlotDescr,
	       "+root-location-direct-slot-description+",
	       add ( STRUCTIDXOFFSET, 11 ),
	       "Index into LISP root object containing the\
 direct slot description object." )
  and
  enumerator ( eshLispRootIdxEffSlotDescr,
	       "+root-location-effective-slot-description+",
	       add ( STRUCTIDXOFFSET, 12 ),
	       "Index into LISP root object containing the\
 effective slot description object." )
  and
  enumerator ( eshLispRootIdxMethodDescr,
	       "+root-location-method-description+",
	       add ( STRUCTIDXOFFSET, 13 ),
	       "Index into LISP root object containing the\
 method description object." )
  and
  enumerator ( eshLispRootIdxPFS,
	       "+root-location-pfs+",
	       add ( STRUCTIDXOFFSET, 14 ),
	       "Index into LISP root object containing the\
 Persistent File System." )
  and
  enumerator ( eshLispRootSize,
	       "+root-size+",
	       add ( STRUCTIDXOFFSET, 15 ),
	       "Size of LISP root vector." )
EndEnum ( LISPROOTIDX );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
