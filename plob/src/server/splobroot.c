/* -------------------------------------------------------------------------
| Module	splobroot.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		11.1.94 Derived from c-plob.c
| Description	
| Compile	gcc -g -c c-plob-root.c
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>

#if !WIN32
#include	<unistd.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>
#include	<netdb.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"splob.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobtype.h"
#include	"splobnumber.h"
#include	"splobsequ.h"
#include	"splobbtree.h"
#include	"splobheap.h"
#include	"splobroot.h"
#include	"splobadmin.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
OBJID			oFreeListCache		= NULLOBJID;
OBJID			oRootLockQueueCache	= NULLOBJID;
OBJID			oRootLockCache		= NULLOBJID;
int			nGCcounterCache		= -1;

OBJID			oUsersCache		= NULLOBJID;
OBJID			oMachsCache		= NULLOBJID;
OBJID			oSessionsCache		= NULLOBJID;

/* -------------------------------------------------------------------------
| Static function declarations
 ------------------------------------------------------------------------- */
static BOOL		mfnInitRoot		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnPrintRoot		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static LPPLOBROOT	fnGetPlobRoot		( BOOL bIgnoreError );

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitUser		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnUserNameOf		( OBJID oSelf,
						  LPINT lpnName );
static LPSTR		mfnPrintUser		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static COMPARETAG	mfnUserCompare		( LPVOID	pSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );
static LPPLOBUSER	fnGetPlobUser		( OBJID oUser );

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitMach		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo );
static LPSTR		mfnMachNameOf		( OBJID oSelf,
						  LPINT lpnName );
static LPSTR		mfnPrintMach		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer );
static COMPARETAG	mfnMachCompare		( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare );
static LPPLOBMACH	fnGetPlobMach		( OBJID oMachine );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeRootModule		( void )
{
  PROCEDURE	( fnInitializeRootModule );

  RegisterMethod ( eshRootTag, gfnInitializeInstance, mfnInitRoot );
  RegisterMethod ( eshRootTag, gfnPrintObjectDetails, mfnPrintRoot );

  RegisterMethod ( eshUserTag, gfnInitializeInstance, mfnInitUser );
  RegisterMethod ( eshUserTag, gfnNameOf, mfnUserNameOf );
  RegisterMethod ( eshUserTag, gfnPrintObjectDetails, mfnPrintUser );
  RegisterMethod ( eshUserTag, gfnCompare, mfnUserCompare );

  RegisterMethod ( eshMachTag, gfnInitializeInstance, mfnInitMach );
  RegisterMethod ( eshMachTag, gfnNameOf, mfnMachNameOf );
  RegisterMethod ( eshMachTag, gfnPrintObjectDetails, mfnPrintMach );
  RegisterMethod ( eshMachTag, gfnCompare, mfnMachCompare );

  RETURN ( VOID );
} /* fnInitializeRootModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeRootModule	( void )
{
  PROCEDURE	( fnDeinitializeRootModule );

  RETURN ( VOID );
} /* fnDeinitializeRootModule */

/* -------------------------------------------------------------------------
| Methods
 ------------------------------------------------------------------------- */
static BOOL		mfnInitRoot		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitRoot );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );
  ((LPPLOBROOT)lpSHvector)->oPlobVersion	= Fixnum2ObjId ( PlobVersion );
  ((LPPLOBROOT)lpSHvector)->oSelf		= oObjId;
  ((LPPLOBROOT)lpSHvector)->oLastTractId	= o1;
  ((LPPLOBROOT)lpSHvector)->oGCcounter		= o0;

  RETURN ( TRUE );
} /* fnInitRoot */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintRoot		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  int		nVersion;

  PROCEDURE	( mfnPrintRoot );

  nVersion	= OBJID2FIXNUM ( ((LPPLOBROOT)lpSHvector)->oPlobVersion );
  sprintf ( lpszBuffer, "%d.%2.2d", nVersion / 100, nVersion % 100 );
  RETURN ( lpszBuffer );
} /* mfnPrintRoot */

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitUser		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitUser );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );

  RETURN ( TRUE );
} /* mfnInitUser */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnUserNameOf		( OBJID oSelf,
						  LPINT lpnName )
{
  LPPLOBUSER	lpUser;
  LPSTR		lpszName;

  PROCEDURE	 ( mfnUserNameOf );

  lpUser	= fnGetPlobUser ( oSelf );
  ASSERT ( lpUser != NULL );
  if ( stringp ( lpUser->oszName ) ) {
    lpszName	= string_ptr ( lpUser->oszName );
  } else if ( boundp ( lpUser->oszName ) ) {
    lpszName	= UNREADABLE_OBJECT_PREFIX 
      "invalid username"
      UNREADABLE_OBJECT_SUFFIX;
  } else {
    lpszName	= UNREADABLE_OBJECT_PREFIX 
      "unbound username"
      UNREADABLE_OBJECT_SUFFIX;
  }
  if ( lpnName ) {
    *lpnName	= strlen ( lpszName );
  }
  RETURN ( lpszName );
} /* mfnUserNameOf */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintUser		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  PROCEDURE	( mfnPrintUser );

  if ( stringp ( ((LPPLOBUSER)lpSHvector)->oszName ) ) {
    strncpy ( lpszBuffer, string_ptr ( ((LPPLOBUSER)lpSHvector)->oszName ),
	      nBuffer );
  }
  RETURN ( lpszBuffer );
} /* mfnPrintUser */

/* ----------------------------------------------------------------------- */
static COMPARETAG	mfnUserCompare		( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  LPCSTR	psName = NULL;
  int		nName = 0;

  PROCEDURE	( mfnUserCompare );

  psName	= mfnUserNameOf ( * (LPOBJID) poSelf, &nName );

  RETURN ( fnNameCompare ( psName, nName, oCompare, TRUE ) );
} /* mfnUserCompare */

/* ----------------------------------------------------------------------- */
static BOOL		mfnInitMach		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo )
{
  PROCEDURE	( mfnInitMach );

  mfnInitStandard ( oObjId, lpSHvector, lpClassInfo );

  RETURN ( TRUE );
} /* mfnInitMach */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnMachNameOf		( OBJID oSelf,
						  LPINT lpnName )
{
  LPPLOBMACH	lpMachine;
  LPSTR		lpszName;

  PROCEDURE	 ( mfnMachNameOf );

  lpMachine	= fnGetPlobMach ( oSelf );
  ASSERT ( lpMachine != NULL );
  if ( stringp ( lpMachine->oszName ) ) {
    lpszName	= string_ptr ( lpMachine->oszName );
  } else if ( boundp ( lpMachine->oszName ) ) {
    lpszName	= UNREADABLE_OBJECT_PREFIX 
      "invalid machinename"
      UNREADABLE_OBJECT_SUFFIX;
  } else {
    lpszName	= UNREADABLE_OBJECT_PREFIX 
      "unbound machinename"
      UNREADABLE_OBJECT_SUFFIX;
  }
  if ( lpnName ) {
    *lpnName	= strlen ( lpszName );
  }
  RETURN ( lpszName );
} /* mfnMachNameOf */

/* ----------------------------------------------------------------------- */
static LPSTR		mfnPrintMach		( OBJID oObjId,
						  LPOBJID lpSHvector,
						  LPCLASSINFO lpClassInfo,
						  LPSTR lpszBuffer,
						  size_t nBuffer )
{
  char		szBuffer [ 256 ];
  int		i = 0;
  PROCEDURE	( mfnPrintMach );

  szBuffer [ 0 ]	= '\0';
  if ( stringp ( ((LPPLOBMACH)lpSHvector)->oszName ) ) {
    strncpy ( szBuffer, string_ptr ( ((LPPLOBMACH)lpSHvector)->oszName ),
	      sizeof ( szBuffer ) );
  }
  i			= strlen ( szBuffer );
  if ( ((LPPLOBMACH)lpSHvector)->oLoginP == allowmarker) {
    strcpy ( & szBuffer [ i ], " :allow" );
  } else if ( ((LPPLOBMACH)lpSHvector)->oLoginP == denymarker) {
    strcpy ( & szBuffer [ i ], " :deny" );
  }
  strncpy ( lpszBuffer, szBuffer, nBuffer );
  RETURN ( lpszBuffer );
} /* mfnPrintMach */

/* ----------------------------------------------------------------------- */
static COMPARETAG	mfnMachCompare		( LPVOID	poSelf,
						  SHTYPETAG	eTypeTagSelf,
						  OBJID		oCompare )
{
  LPPLOBMACH	pSelf, pCompare;
  int		i, n;
  COMPARETAG	eCompared = eshEqual;

  PROCEDURE	( mfnMachCompare );

  pSelf		= fnGetPlobMach ( * (LPOBJID) poSelf );

  pCompare	= fnGetPlobMach ( oCompare );
  if ( pCompare == NULL ) {
    RETURN ( eshNotEq );
  }

  for ( i = 0, n = length ( pSelf->onAddr ); i < n; i++ ) {
    eCompared	= gfnCompare ( & pSelf->onAddr [ i ],
			       typetagof ( pSelf->onAddr [ i ] ),
			       pCompare->onAddr [ i ] );
    switch ( eCompared ) {
    case eshEqual: case eshEql: case eshEq:
      break;
    default:
      RETURN ( eCompared );
    }
  }

  RETURN ( eCompared );
} /* mfnMachCompare */

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
static LPPLOBROOT	fnGetPlobRoot		( BOOL bIgnoreError )
{
  static CONST char	szDiscard []	=
    "Discard old PLOB-root object and create a new one.";

  LPPLOBROOT		lpPlobRoot;
  OBJID			oPlobRoot, oFirst, oRoot;

  PROCEDURE	( fnGetPlobRoot );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  oFirst	= SH_first_object ();
  oPlobRoot	= SH_read_key ( oFirst, eshSHvectorIdxFirstObjId );
  oRoot		= -1;
  lpPlobRoot	= (LPPLOBROOT) SH_key_to_address ( oPlobRoot );
  ASSERT ( lpPlobRoot );
  if ( ! typetagp ( lpPlobRoot->Header.oTypeTag ) ||
       OBJID2TYPETAG ( lpPlobRoot->Header.oTypeTag ) != eshRootTag ) {
    oRoot	= 0;
  } else if ( ! fixnump ( lpPlobRoot->oPlobVersion ) || 
	      lpPlobRoot->oSelf != oPlobRoot ) {
    if ( ! bIgnoreError ) {
      CERROR (( szDiscard, "Corrupted PLOB stable heap root object." ));
    }
    oRoot	= 0;
  } else if ( OBJID2FIXNUM ( lpPlobRoot->oPlobVersion ) / 100 !=
	      PlobVersion / 100 ) {
    if ( ! bIgnoreError ) {
      CERROR (( szDiscard, "Version conflict:"
	        " PLOB stable heap root object has version %d;"
	        " current PLOB run-time version is %d.",
	        OBJID2FIXNUM ( lpPlobRoot->oPlobVersion ), PlobVersion ));
    }
    oRoot	= lpPlobRoot->oRootUser;
  }
  if ( oRoot >= 0 ) {
    fnInvalidateAllCaches ();
    SH_write_key ( oFirst, eshSHvectorIdxFirstObjId, oFirst );
    /* Allocate a new PLOB root object and chain it in: */
    oPlobRoot			=
      fnCreateObject ( (SHTYPETAG) eshRootTag, 0, NULLTYPETAG, 0 );
    lpPlobRoot			=
      (LPPLOBROOT) SH_key_to_address ( oPlobRoot );
    /* Make sure we can access the PLOB root object: */
    ASSERT ( lpPlobRoot );
    SH_write_key ( oFirst, eshSHvectorIdxFirstObjId, oPlobRoot );
    AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    lpPlobRoot->oRootUser	= ( oRoot > 0 ) ? oRoot : oPlobRoot;
    AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    SH_stabilise ();
    lpPlobRoot			=
      (LPPLOBROOT) SH_key_to_address ( oPlobRoot );
  }
  RETURN ( lpPlobRoot );
} /* fnGetPlobRoot */

/* ----------------------------------------------------------------------- */
static LPPLOBUSER	fnGetPlobUser		( OBJID oUser )
{
  LPPLOBUSER	lpUser;

  PROCEDURE	( fnGetPlobUser );

  lpUser	= (LPPLOBUSER) SH_key_to_address ( oUser );
  ASSERT ( lpUser != NULL );
  if ( ! ASSERT_TYPE ( oUser, lpUser, eshUserTag ) ) {
    RETURN ( (LPPLOBUSER) NULL );
  }
  RETURN ( lpUser );
}/* fnGetPlobUser */

/* ----------------------------------------------------------------------- */
static LPPLOBMACH	fnGetPlobMach		( OBJID oMachine )
{
  LPPLOBMACH	lpMachine;

  PROCEDURE	( fnGetPlobMach );

  lpMachine	= (LPPLOBMACH) SH_key_to_address ( oMachine );
  ASSERT ( lpMachine != NULL );
  if ( ! ASSERT_TYPE ( oMachine, lpMachine, eshMachTag ) ) {
    RETURN ( (LPPLOBMACH) NULL );
  }
  RETURN ( lpMachine );
}/* fnGetPlobMach */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
int DLLEXPORT		fnGetNextTractId	( void )
{
  LPPLOBROOT	lpPlobRoot;
  int		nTractId;

  PROCEDURE	( fnGetNextTractId );

  lpPlobRoot			= fnGetPlobRoot ( FALSE );
  nTractId			= ObjId2Fixnum ( lpPlobRoot->oLastTractId );
  lpPlobRoot->oLastTractId	= Fixnum2ObjId ( nTractId + 1 );
  RETURN ( nTractId );
} /* fnGetNextTractId */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnGetRootFreeList	( void )
{
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnGetRootFreeList );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot		= fnGetPlobRoot ( FALSE );
  oFreeListCache	= lpPlobRoot->oFreeList;
  RETURN ( oFreeListCache );
} /* fnGetRootFreeList */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnSetRootFreeList	( OBJID oFreeList )
{
  LPPLOBROOT	lpPlobRoot;
  OBJID		oFreeListOld;

  PROCEDURE	( fnSetRootFreeList );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot		= fnGetPlobRoot ( FALSE );
  oFreeListOld		= lpPlobRoot->oFreeList;
  AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  lpPlobRoot->oFreeList	= oFreeList;
  oFreeListCache	= oFreeList;
  AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  RETURN ( oFreeListOld );
} /* fnSetRootFreeList */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnGetRootLockQueue	( void )
{
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnGetRootLockQueue );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot	= fnGetPlobRoot ( FALSE );
  if ( ! btreep ( lpPlobRoot->oLockQueue ) ) {
    AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    lpPlobRoot->oLockQueue	= make_btree ( eshEq );
    AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  }
  oRootLockQueueCache	= lpPlobRoot->oLockQueue;
  RETURN ( oRootLockQueueCache );
} /* fnGetRootLockQueue */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnGetRootLock		( void )
{
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnGetRootLock );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot		= fnGetPlobRoot ( FALSE );
  oRootLockCache	= lpPlobRoot->oLock;
  RETURN ( oRootLockCache );
} /* fnGetRootLock */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT		fnSetRootLock		( OBJID oLock )
{
  LPPLOBROOT	lpPlobRoot;
  OBJID		oLockOld;
  LPOBJID	lpSHvector;

  PROCEDURE	( fnSetRootLock );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot				= fnGetPlobRoot ( FALSE );
  oLockOld				= lpPlobRoot->oLock;
  lpSHvector				= ( immediatep ( oLock ) ) ?
    (LPOBJID) NULL : AtomicLock ( oLock, lpPlobRoot->oSelf );
  AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  lpPlobRoot->oLock			= oLock;
  lpPlobRoot->Header.oLockedBy		= oLock;
  oRootLockCache			= oLock;
  if ( lpSHvector )
    lpSHvector [ eshSHvectorIdxLockedBy ]	= lpPlobRoot->oSelf;
  AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  if ( lpSHvector )
    AtomicUnlock ( oLock, lpPlobRoot->oSelf );
  RETURN ( oLockOld );
} /* fnSetRootLock */

/* ----------------------------------------------------------------------- */
int DLLEXPORT		fnIncGCcounter		( void )
{
  LPPLOBROOT	lpPlobRoot;
  int		nGCcounter;

  PROCEDURE	( fnIncGCcounter );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpPlobRoot			= fnGetPlobRoot ( FALSE );
  AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  nGCcounter			= ObjId2Fixnum ( lpPlobRoot->oGCcounter );
  nGCcounter++;
  lpPlobRoot->oGCcounter	= Fixnum2ObjId ( nGCcounter );
  AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  nGCcounterCache		= nGCcounter;
  /* Invalidate the cached objid's (because their values changed by the
     performed garbage collection): */
  fnInvalidateAllCaches ();
  nGlobalTouched		= 0;

  RETURN ( nGCcounter );
} /* fnIncGCcounter */

/* ----------------------------------------------------------------------- */
int DLLEXPORT		fnGetGCcounter		( void )
{
  LPPLOBROOT	lpPlobRoot;
  int		nGCcounter;

  PROCEDURE	( fnGetGCcounter );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot		= fnGetPlobRoot ( FALSE );
  nGCcounter		= ObjId2Fixnum ( lpPlobRoot->oGCcounter );
  nGCcounterCache	= nGCcounter;

  RETURN ( nGCcounter );
}

/* -------------------------------------------------------------------------
| Utility functions:
 ------------------------------------------------------------------------- */
void DLLEXPORT	fnSetGlobalMinMaxObjId	( void )
{
  struct stableheap_configuration	Configuration;
  struct stableheap_statistics		Statistics;

  PROCEDURE	( fnSetGlobalMinMaxObjId );
  INITIALIZEPLOB;

  memset ( &Configuration, 0, sizeof ( Configuration ) );
  memset ( &Statistics, 0, sizeof ( Statistics ) );
  SH_configuration ( &Configuration );
  SH_statistics ( &Statistics );
  oGlobalMinObjId	= Configuration.maximum_key -
    ( Statistics.number_of_objects - 1 ) *
    Configuration.key_alignment;
  oGlobalMaxObjId	= Configuration.maximum_key;

  RETURN ( VOID );
} /* fnSetGlobalMinMaxObjId */

/* -------------------------------------------------------------------------
| Functions on administrating PLOB client users:
 ------------------------------------------------------------------------- */
/* Returns the object containing all known client users, i.e. a BTree: */
OBJID DLLEXPORT	fnUsers			( void )
{
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnUsers );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpPlobRoot	= fnGetPlobRoot ( FALSE );
  if ( ! btreep ( lpPlobRoot->oUsers ) ) {
    AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    lpPlobRoot->oUsers	= make_btree ( eshEqual );
    AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  }
  oUsersCache	= lpPlobRoot->oUsers;
  RETURN ( lpPlobRoot->oUsers );
} /* fnUsers */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnUserInsertByName	( LPCSTR	lpszUserName,
					  int		nUserID )
{
  OBJID		oUsers, oUser;
  LPPLOBUSER	lpUser;

  PROCEDURE	( fnUserInsertByName );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oUsers	= fnUsers ();
  makunbound ( oUser );
  if ( fnBTreeSearch ( NULLOBJID, oUsers, (LPCVOID) lpszUserName,
		       eshDynCStringPtrTag, (LPOBJID) NULL, &oUser ) ==
       btreeFound && boundp ( oUser ) ) {
    /* Update the user: */
    lpUser		= fnGetPlobUser ( oUser );
    ASSERT ( lpUser != NULL );
    AtomicLock ( oUser, oUser );
    lpUser->onID	= Fixnum2ObjId ( nUserID );
    AtomicUnlock ( oUser, oUser );
  } else {
    /* Insert the user: */
    oUser		=
      fnCreateObject ( (SHTYPETAG) eshUserTag, 0, NULLTYPETAG, 0 );
    lpUser		= fnGetPlobUser ( oUser );
    lpUser->oszName	= make_string ( lpszUserName );
    lpUser->onID	= Fixnum2ObjId ( nUserID );
    AtomicLock ( oUsers, oUsers );
    fnBTreeInsertByObjId ( NULLOBJID, oUsers, lpUser->oszName, oUser );
    AtomicUnlock ( oUsers, oUsers );
  }
  RETURN ( oUser );
} /* fnUserInsertByName */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnUserSearchByName	( LPCSTR	lpszUserName )
{
  OBJID		oUsers, oUser;

  PROCEDURE	( fnUserSearchByName );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oUsers	= fnUsers ();
  makunbound ( oUser );
  if ( fnBTreeSearch ( NULLOBJID, oUsers, (LPCVOID) lpszUserName,
		       eshDynCStringPtrTag, (LPOBJID) NULL, &oUser ) ==
       btreeFound && boundp ( oUser ) ) {
    RETURN ( oUser );
  }
  RETURN ( NULLOBJID );
} /* fnUserSearchByName */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnUserDeleteByName	( LPCSTR	lpszUserName )
{
  OBJID		oUsers, oUser;

  PROCEDURE	( fnUserDeleteByName );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oUsers	= fnUsers ();
  makunbound ( oUser );
  if ( fnBTreeSearch ( NULLOBJID, oUsers, (LPCVOID) lpszUserName,
		       eshDynCStringPtrTag, (LPOBJID) NULL, &oUser ) ==
       btreeFound && boundp ( oUser ) ) {
    AtomicLock ( oUsers, oUsers );
    fnBTreeDelete ( NULLOBJID, oUsers, (LPCVOID) lpszUserName,
		    eshDynCStringPtrTag );
    AtomicUnlock ( oUsers, oUsers );
  }
  RETURN ( oUser );
} /* fnUserDeleteByName */

/* -------------------------------------------------------------------------
| Functions on administrating PLOB client machines:
 ------------------------------------------------------------------------- */
/* Returns the object containing all known client machines, i.e. a BTree: */
OBJID DLLEXPORT	fnMachs			( void )
{
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnMachs );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot	= fnGetPlobRoot ( FALSE );
  if ( ! btreep ( lpPlobRoot->oMachines ) ) {
    AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    lpPlobRoot->oMachines	= make_btree ( eshEqual );
    AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  }
  oMachsCache	= lpPlobRoot->oMachines;
  RETURN ( lpPlobRoot->oMachines );
} /* fnMachs */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnCreateMachine		( int		nAddr [ 4 ],
					  OBJID		oLoginP )
{
  OBJID			oMachine;
  LPPLOBMACH		lpMachine;
  int			i, j, k, nWildcards;
  unsigned long		nA;
  char			szAddr [ 32 ], szHostname [ 256 ];
  struct hostent	* pHost = (struct hostent *) NULL;
  BOOL			bIsLocalAddr = TRUE; /* A flag for 0.0.0.0 */

  PROCEDURE	( fnCreateMachine );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  ASSERT ( nAddr != NULL );

  oMachine	= fnCreateObject ( (SHTYPETAG) eshMachTag, 0, NULLTYPETAG, 0 );
  lpMachine	= fnGetPlobMach ( oMachine );

  for ( i = 0; i < length ( lpMachine->onAddr ); i++ ) {
    lpMachine->onAddr [ i ]	= Fixnum2ObjId ( nAddr [ i ] );
    if ( nAddr [ i ] != 0 ) {
      bIsLocalAddr	= FALSE;
    }
  }

  for ( i = 0, j = 0, nWildcards = 0; i < length ( lpMachine->onAddr ); i++ ) {
    if ( i > 0 ) {
      szAddr [ j++ ]	= '.';
    }
    if ( nAddr [ i ] == nMatchAny ) {
      szAddr [ j++ ]	= '*';
      nWildcards++;
      /* Look ahead if the rest of the address consists of wildcards,
         too: */
      for ( k = i;
	    nAddr [ k ] == nMatchAny && k < length ( lpMachine->onAddr );
	    k++ );
      if ( k >= length ( lpMachine->onAddr ) ) {
	/* Rest of the name are only trailing wildcards; omit them: */
	break;
      }
    } else {
      sprintf ( & szAddr [ j ], "%d", nAddr [ i ] );
      j	+= strlen ( & szAddr [ j ] );
    }
  }
  szAddr [ j++ ]	= '\0';

  if ( bIsLocalAddr ) {
    lpMachine->oszName	= make_string ( szLocalName );
  } else if ( nWildcards == 0 ) {
    /* Locate the machine name: */
    nA			= inet_addr ( szAddr );
    pHost		=
      gethostbyaddr ( (char*) &nA, sizeof ( nA ), AF_INET );
    szHostname [ 0 ]	= '\0';
    if ( pHost != NULL && pHost->h_name != NULL ) {
      strncpy ( szHostname, pHost->h_name, sizeof ( szHostname ) );
      szHostname [ sizeof ( szHostname ) - 1 ]	= '\0';
      /* I can't believe it: On NT, pHost->h_name has a trailing ^M! */
      for ( i = strlen ( szHostname ) - 1;
	    i >= 0 && szHostname [ i ] < ' '; i-- ) {
	szHostname [ i ]	= '\0';
      }
    }
    if ( szHostname [ 0 ] != '\0' ) {
      lpMachine->oszName	= make_string ( szHostname );
    }
  }

  if ( ! boundp ( lpMachine->oszName ) ) {
    lpMachine->oszName	= make_string ( szAddr );
  }

  lpMachine->oLoginP	= oLoginP;

  RETURN ( oMachine );
} /* fnCreateMachine */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMachLoginP		( OBJID		oMachine,
					  OBJID		oLoginP )
{
  LPPLOBMACH	lpMachine;
  OBJID		oLoginPold;

  PROCEDURE	( fnMachLoginP );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpMachine	= fnGetPlobMach ( oMachine );
  oLoginPold	= lpMachine->oLoginP;

  switch ( oLoginP ) {
  case allowmarker: case denymarker: case unbound:
    lpMachine->oLoginP	= oLoginP;
  }

  RETURN ( oLoginPold );
} /* fnMachLoginP */

/* ----------------------------------------------------------------------- */
typedef enum {
    eaExact,		/* Search for an exact match */
    eaMatch,		/* Search for a first wildcard match */
    eaLogin		/* Search for a login match */
}	MATCHACTION;

typedef struct {
  int		nAddr [ length ( ((LPPLOBMACH)NULL)->onAddr ) ];
  MATCHACTION	eAction;
  /* `Score' of last match found; score is a measure on how good the
     match found matches the machine to search: */
  int		nScore;
  OBJID		oKey;
  OBJID		oData;
}	MACHSEARCHARG, * PMACHSEARCHARG;

/* ----------------------------------------------------------------------- */
static BOOL	fnEnumMachSearch	( LPVOID lpUserData,
					  OBJID oBTree,
					  OBJID oKey,
					  OBJID oData,
					  OBJID oBTreePage,
					  int nIndex )
{
  PMACHSEARCHARG	pMachSearch	= (PMACHSEARCHARG) lpUserData;
  LPPLOBMACH		lpData;
  int			nScore = -1, i, a, nExact = 0, nMatches = 0;

  PROCEDURE	( fnEnumMachSearch );
  ASSERT ( pMachSearch != NULL );

  lpData	= fnGetPlobMach ( oData );
  for ( i = 0; i < length ( lpData->onAddr ); i++ ) {
    a		= ObjId2Fixnum ( lpData->onAddr [ i ] );
    if ( a == nMatchAny && nScore < 0 ) {
      nScore	= i;
    }
    if ( a == pMachSearch->nAddr [ i ] || a == nMatchAny ) {
      nMatches++;
    }
    if ( a == pMachSearch->nAddr [ i ] ) {
      nExact++;
    }
  }

  switch ( pMachSearch->eAction ) {
  case eaExact:
    if ( nExact >= length ( pMachSearch->nAddr ) ) {
      pMachSearch->nScore	= nScore;
      pMachSearch->oKey		= oKey;
      pMachSearch->oData	= oData;
      RETURN ( FALSE );
    }
    break;
  case eaMatch:
    if ( nMatches >= length ( pMachSearch->nAddr ) ) {
      pMachSearch->nScore	= nScore;
      pMachSearch->oKey		= oKey;
      pMachSearch->oData	= oData;
      RETURN ( FALSE );
    }
    break;
  case eaLogin:
    if ( ( lpData->oLoginP == allowmarker ||
	   lpData->oLoginP == denymarker ) ) {
      if ( nExact >= length ( pMachSearch->nAddr ) ) {
	/* Found a direct machine entry: */
	pMachSearch->nScore	= nScore;
	pMachSearch->oKey	= oKey;
	pMachSearch->oData	= oData;
	RETURN ( FALSE );
      }
      if ( nMatches >= length ( pMachSearch->nAddr ) &&
	   pMachSearch->nScore < nScore ) {
	/* Found a better matching machine entry: */
	pMachSearch->nScore	= nScore;
	pMachSearch->oKey	= oKey;
	pMachSearch->oData	= oData;
      }
    }
    break;
  default:
    ERROR (( "Unexpected value: %d", pMachSearch->eAction ));
    RETURN ( FALSE );
  }

  RETURN ( TRUE );
} /* fnEnumMachSearch */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMachInsertByAddr	( int		nAddr [ 4 ],
					  OBJID		oLoginP )
{
  MACHSEARCHARG	MachSearchArg;
  OBJID		oMachines, oMachine;
  LPPLOBMACH	lpMachine;

  PROCEDURE	( fnMachInsertByAddr );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oMachines		= fnMachs ();
  memset ( &MachSearchArg, 0, sizeof ( MachSearchArg ) );
  memcpy ( MachSearchArg.nAddr, nAddr, sizeof ( MachSearchArg.nAddr ) );
  MachSearchArg.eAction	= eaExact;
  fnBTreeMapAll ( NULLOBJID, oMachines, fnEnumMachSearch, &MachSearchArg );
  oMachine	= MachSearchArg.oData;

  if ( ! boundp ( oMachine ) ) {
    /* Insert the machine: */
    oMachine	= fnCreateMachine ( nAddr, oLoginP );
    lpMachine	= fnGetPlobMach ( oMachine );
    AtomicLock ( oMachines, oMachines );
    fnBTreeInsertByObjId ( NULLOBJID, oMachines,
			   lpMachine->oszName, oMachine );
    AtomicUnlock ( oMachines, oMachines );
  }
  RETURN ( oMachine );
} /* fnMachInsertByAddr */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMachSearchByAddr	( int		nAddr [ 4 ] )
{
  MACHSEARCHARG	MachSearchArg;
  OBJID		oMachines, oMachine;

  PROCEDURE	( fnMachSearchByAddr );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oMachines		= fnMachs ();
  memset ( &MachSearchArg, 0, sizeof ( MachSearchArg ) );
  memcpy ( MachSearchArg.nAddr, nAddr, sizeof ( MachSearchArg.nAddr ) );
  MachSearchArg.eAction	= eaExact;
  fnBTreeMapAll ( NULLOBJID, oMachines, fnEnumMachSearch, &MachSearchArg );
  oMachine	= MachSearchArg.oData;
  RETURN ( oMachine );
} /* fnMachSearchByAddr */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnMachDeleteByAddr	( int		nAddr [ 4 ] )
{
  MACHSEARCHARG	MachSearchArg;
  OBJID		oMachines, oMachine;

  PROCEDURE	( fnMachDeleteByAddr );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  oMachines		= fnMachs ();
  memset ( &MachSearchArg, 0, sizeof ( MachSearchArg ) );
  memcpy ( MachSearchArg.nAddr, nAddr, sizeof ( MachSearchArg.nAddr ) );
  MachSearchArg.eAction	= eaExact;
  fnBTreeMapAll ( NULLOBJID, oMachines, fnEnumMachSearch, &MachSearchArg );
  if ( boundp ( MachSearchArg.oData ) ) {
    AtomicLock ( oMachines, oMachines );
    fnBTreeDeleteByObjId ( NULLOBJID, oMachines, MachSearchArg.oKey );
    AtomicUnlock ( oMachines, oMachines );
  }
  RETURN ( MachSearchArg.oData );
} /* fnMachDeleteByAddr */

/* -------------------------------------------------------------------------
| Functions on administrating PLOB client sessions:
 ------------------------------------------------------------------------- */
/* Returns the object containing all known client sessions,
   i.e. a BTree containing persistent heaps: */
OBJID DLLEXPORT	fnSessions		( void )
{
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnSessions );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  lpPlobRoot		= fnGetPlobRoot ( FALSE );
  if ( ! btreep ( lpPlobRoot->oSessions ) ) {
    AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    lpPlobRoot->oSessions	= make_btree ( eshEq );
    AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
  }
  oSessionsCache	= lpPlobRoot->oSessions;
  RETURN ( lpPlobRoot->oSessions );
} /* fnSessions */

/* ----------------------------------------------------------------------- */
BTREERESULT DLLEXPORT fnSessionInsert	( OBJID		oSession )
{
  OBJID		oSessions;
  BTREERESULT	nResult;

  PROCEDURE	( fnSessionInsert );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  oSessions	= fnSessions ();
  AtomicLock ( oSessions, oSessions );
  nResult	= fnBTreeInsertByObjId ( NULLOBJID, oSessions,
					 oSession, oSession );
  AtomicUnlock ( oSessions, oSessions );
  RETURN ( nResult );
} /* fnSessionInsert */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnSessionSearch		( OBJID		oSession )
{
  OBJID		oSessions, oData;

  PROCEDURE	( fnSessionSearch );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  oSessions	= fnSessions ();
  makunbound ( oData );
  BTreeSearchByObjId ( NULLOBJID, oSessions, oSession, NULL, &oData );
  RETURN ( oData );
} /* fnSessionSearch */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnSessionDelete		( OBJID		oSession )
{
  OBJID		oSessions, oData;

  PROCEDURE	( fnSessionDelete );

  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );
  oSessions	= fnSessions ();
  makunbound ( oData );
  BTreeSearchByObjId ( NULLOBJID, oSessions, oSession, NULL, &oData );
  if ( boundp ( oData ) ) {
    AtomicLock ( oSessions, oSessions );
    fnBTreeDeleteByObjId ( NULLOBJID, oSessions, oSession );
    AtomicUnlock ( oSessions, oSessions );
  }
  RETURN ( oData );
} /* fnSessionDelete */

/* ----------------------------------------------------------------------- */
OBJID DLLEXPORT	fnReadLispRoot		( void )
{
  LPPLOBROOT	lpPlobRoot;
  OBJID		oLispRoot	= unbound;

  PROCEDURE	( fnReadLispRoot );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpPlobRoot	= fnGetPlobRoot ( FALSE );
  oLispRoot	= ( lpPlobRoot->oRootUser == lpPlobRoot->oSelf ) ?
    NULLOBJID : lpPlobRoot->oRootUser;
  RETURN ( oLispRoot );
} /* fnReadLispRoot */

/* -------------------------------------------------------------------------
| Functions on administrating PLOB:
 ------------------------------------------------------------------------- */
BOOL DLLEXPORT		fnLoginP	( OBJID		oUser,
					  OBJID		oMachine,
					  LPSTR		lpszReason,
					  size_t	nReason )
{
  BOOL		bLoginP	= TRUE;
  MACHSEARCHARG	MachSearchArg;
  OBJID		oMachines;
  LPPLOBMACH	lpMachine, lpMatch;
  int		i;

  PROCEDURE	( fnLoginP );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  if ( lpszReason != NULL ) {
    *lpszReason	= '\0';
  }

  if ( fnAdminP ( oUser, oMachine ) == eaAdminTrue ) {
    if ( lpszReason != NULL ) {
      strncpy ( lpszReason, "Administrator login", nReason );
    }
    RETURN ( TRUE );
  }

  oMachines		= fnMachs ();
  lpMachine		= fnGetPlobMach ( oMachine );
  memset ( &MachSearchArg, 0, sizeof ( MachSearchArg ) );
  for ( i = 0; i < length ( MachSearchArg.nAddr ); i++ ) {
    MachSearchArg.nAddr [ i ]	= ObjId2Fixnum ( lpMachine->onAddr [ i ] );
  }
  MachSearchArg.eAction	= eaLogin;
  fnBTreeMapAll ( NULLOBJID, oMachines, fnEnumMachSearch, &MachSearchArg );
  if ( boundp ( MachSearchArg.oData ) ) {
    if ( lpszReason != NULL ) {
      fnPrintObject ( MachSearchArg.oData, lpszReason, nReason );
    }
    lpMatch	= fnGetPlobMach ( MachSearchArg.oData );
    bLoginP	= (BOOL) ( lpMatch->oLoginP == allowmarker );
  } else if ( lpszReason != NULL ) {
    strncpy ( lpszReason,  "No allow/deny found", nReason );
  }

  RETURN ( bLoginP );
} /* fnLoginP */

/* ----------------------------------------------------------------------- */
ADMINP DLLEXPORT	fnAdminP	( OBJID		oUser,
					  OBJID		oMachine )
{
  ADMINP	AdminP = eaNoAdmin;
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnAdminP );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpPlobRoot	= fnGetPlobRoot ( FALSE );

  if ( boundp ( lpPlobRoot->oAdminUser ) /* && */
       /* boundp ( lpPlobRoot->oAdminMach ) */ ) {
    AdminP	= ( gfnEqual ( lpPlobRoot->oAdminUser, oUser )
		    /* && */
		    /* gfnEqual ( lpPlobRoot->oAdminMach, oMachine ) */ ) ?
      eaAdminTrue : eaAdminFalse;
  }
  RETURN ( AdminP );
} /* fnAdminP */

/* ----------------------------------------------------------------------- */
void DLLEXPORT		fnAdminSet	( OBJID		oUser,
					  OBJID		oMachine )
{
  LPPLOBROOT	lpPlobRoot;

  PROCEDURE	( fnAdminSet );
  INITIALIZEPLOB;
  ASSERT ( StableHeap_is_open );

  lpPlobRoot			= fnGetPlobRoot ( FALSE );
  lpPlobRoot->oAdminUser	= oUser;
  lpPlobRoot->oAdminMach	= oMachine;

  RETURN ( VOID );
} /* fnAdminSet */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbReadRoot, "c-sh-read-root",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  SHORTOBJID	oShortRoot;
  char		szVersion [ 128 ], szUnderline [ 128 ];
  int		nServerVersion, nDatabaseVersion, n;
  char		szServerVersion [ 16 ], szDatabaseVersion [ 16 ];

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  nServerVersion	=
    fnServerGetVersion ( oShortObjIdHeap, esvServerCode );
  nDatabaseVersion	=
    fnServerGetVersion ( oShortObjIdHeap, esvDatabase );

  sprintf ( szVersion, "PLOB! daemon version %s"
	    " database version %s - %s",
	    GetVersionString ( nServerVersion, szServerVersion ),
	    GetVersionString ( nDatabaseVersion, szDatabaseVersion ),
	    fnGetCompileOpsysString () );
  n			= strlen ( szVersion );
  memset ( szUnderline, '=', n );
  szUnderline [ n ]	= '\0';

  INFO (( "%s\n"
	  "%s\n"
	  "%s\n"
	  "%s\n"
	  "Part of server code (C) University of St. Andrews"
	  " napier@dcs.st-and.ac.uk\n"
	  "PLOB! comes with ABSOLUTELY NO WARRANTY;"
	  " for details, look into the\n"
	  " user's guide `plob/ps/userg.ps'"
	  " provided within the distribution.",
	  szUnderline, szVersion, szUnderline,
	  fnGetCopyrightString () ));

  oShortRoot	= LONG2SHORTOBJID ( fnReadLispRoot () );

  UnstoreSession ();
  RETURN ( oShortRoot );
} EndFunction ( fnClientDbReadRoot );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnServerDbWriteRoot, "c-sh-write-root",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  LPPLOBROOT				lpPlobRoot;
  OBJID					oHeap, oSHroot;
  OBJID					oUser, oMachine;
  LPPLOBUSER				lpUser;
  LPPLOBMACH				lpMachine;
  SHORTOBJID				oShortObjIdHeapOld;
  int					nSessions, nObjects;
  struct stableheap_statistics		Statistics;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap		= ( oShortObjIdHeap != NULLOBJID ) ?
    Short2LongObjId ( oShortObjIdHeap ) : NULLOBJID;
  oUser		= fnHeapUser ( oHeap );
  oMachine	= fnHeapMachine ( oHeap );
  lpPlobRoot	= fnGetPlobRoot ( FALSE );
  if ( fnAdminP ( oUser, oMachine ) == eaAdminFalse ) {
    char	szAdminUser [ 80 ], szAdminMachine [ 80 ];
    char	szUser [ 80 ], szMachine [ 80 ];
    PrintObject ( lpPlobRoot->oAdminUser, szAdminUser );
    PrintObject ( lpPlobRoot->oAdminMach, szAdminMachine );
    PrintObject ( oUser, szUser );
    PrintObject ( oMachine, szMachine );
    ERROR (( "Only the administrator is allowed to set the root object.\n"
	     "       Administrator is %s on\n"
	     "       machine %s,\n"
	     "       user was %s on\n"
	     "       machine %s",
	     szAdminUser, szAdminMachine,
	     szUser, szMachine ));
    UnstoreSession ();
    RETURN ( NULLOBJID );
  }

  if ( oShortObjId != NULLOBJID ) {
    AtomicLock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    oSHroot			= Short2LongObjId ( oShortObjId );
    lpPlobRoot->oRootUser	= oSHroot;
    AtomicUnlock ( lpPlobRoot->oSelf, lpPlobRoot->oSelf );
    LOG (( "Set root object to %s.",
	   fnPrintObject ( oSHroot, (LPSTR) NULL, 0 ) ));
  } else {
    /* Setting the root object to NULLOBJID means to delete everything
       in the stable heap: */
    nSessions	= gfnCount ( fnSessions () );
    if ( nSessions > 1 ) {
      ERROR (( "Max. 1 client may be active for purging the root object.\n"
	       "       Found %d active clients.",
	       nSessions ));
      UnstoreSession ();
      RETURN ( NULLOBJID );
    }
    memset ( &Statistics, 0, sizeof ( Statistics ) );
    SH_statistics ( &Statistics );
    nObjects			= Statistics.number_of_objects;
    oSHroot			= SH_first_object ();
    SH_write_key ( oSHroot, eshSHvectorIdxFirstObjId,
		   ( oHeap != NULLOBJID ) ? oHeap : oSHroot );
    oGlobalSession		= NULLOBJID;
    SH_garbage_collect ();
    fnSetGlobalMinMaxObjId ();
    oShortObjIdHeapOld		= oShortObjIdHeap;
    if ( oHeap != NULLOBJID ) {
      /* Reload the single session objid: */
      oSHroot		= SH_first_object ();
      oHeap		= SH_read_key ( oSHroot, eshSHvectorIdxFirstObjId );
      SH_write_key ( oSHroot, eshSHvectorIdxFirstObjId, oSHroot );
      oGlobalSession	= oHeap;
      oUser		= fnHeapUser ( oHeap );
      lpUser		= fnGetPlobUser ( oUser );
      ASSERT ( lpUser != NULL );
      fnBTreeInsertByObjId ( NULLOBJID, fnUsers (), lpUser->oszName, oUser );
      oMachine		= fnHeapMachine ( oHeap );
      lpMachine		= fnGetPlobMach ( oMachine );
      ASSERT ( lpMachine != NULL );
      fnBTreeInsertByObjId ( NULLOBJID, fnMachs (),
			     lpMachine->oszName, oMachine );
      oShortObjIdHeap	= Long2ShortObjId ( oHeap );
    }
    fnGetPlobRoot ( TRUE );
    fnIncGCcounter ();
    SH_stabilise ();
    memset ( &Statistics, 0, sizeof ( Statistics ) );
    SH_statistics ( &Statistics );
    LOG (( "Purged root object.\n"
	   "       %d - %d = %d objects, heap objid changed from %d to %d.",
	   nObjects, nObjects - Statistics.number_of_objects,
	   Statistics.number_of_objects,
	   oShortObjIdHeapOld, oShortObjIdHeap ));
  }

  UnstoreSession ();
  RETURN ( oShortObjIdHeap );
} EndFunction ( fnServerDbWriteRoot );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbSessions, "c-sh-sessions",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  OBJID	oSessions;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oSessions	= fnSessions ();
  oSessions	= ( oSessions != NULLOBJID ) ?
    Long2ShortObjId ( oSessions ) : NULLOBJID;

  UnstoreSession ();
  RETURN ( oSessions );
} EndFunction ( fnClientDbSessions );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerGetVersion, "c-sh-get-version",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( GETVERSION, value_in, eWhat ) ) )
{
  FIXNUM	nVersion	= 0;
  LPPLOBROOT	lpPlobRoot	= (LPPLOBROOT) NULL;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }

  switch ( eWhat ) {
  case esvDatabase:
    if ( StableHeap_is_open ) {
      lpPlobRoot	= fnGetPlobRoot ( FALSE );
      if ( lpPlobRoot != NULL ) {
	nVersion	= ObjId2Fixnum ( lpPlobRoot->oPlobVersion );
      }
    }
    break;
  case esvServerCode:
    nVersion		= PlobVersion;
    break;
  default:
    break;
  }

  UnstoreSession ();
  RETURN ( nVersion );
} EndFunction ( fnServerGetVersion );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbCreateMachine, "c-sh-create-machine",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( VECTOR ( int, 4 ), vector_in, nAddr )
		  and
		  argument ( MACHLOGINP, value_in, eLoginP ) ) )
{
  OBJID		oMachine, oLoginP;
  SHORTOBJID	oShortMachine;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  switch ( eLoginP ) {
  case eshLoginAllow:
    oLoginP	= allowmarker;
    break;
  case eshLoginDeny:
    oLoginP	= denymarker;
    break;
  default:
    makunbound ( oLoginP );
    break;
  }
  oMachine	= fnCreateMachine ( nAddr, oLoginP );
  oShortMachine	= ( boundp ( oMachine ) ) ?
    Long2ShortObjId ( oMachine ) : NULLOBJID;

  UnstoreSession ();
  RETURN ( oShortMachine );
} EndFunction ( fnClientDbCreateMachine );

/* ----------------------------------------------------------------------- */
BeginFunction ( MACHLOGINP,
		fnClientMachineLoginP, "c-sh-machine-loginp",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortMachine )
		  and
		  argument ( MACHLOGINP, value_in, eLoginP ) ) )
{
  MACHLOGINP	eLoginPold = eshLoginGet;
  OBJID		oMachine;
  LPPLOBMACH	lpMachine;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshLoginGet );
    }
  }
  ASSERT ( StableHeap_is_open );

  oMachine	= Short2LongObjId ( oShortMachine );
  lpMachine	= fnGetPlobMach ( oMachine );

  switch ( lpMachine->oLoginP ) {
  case allowmarker:
    eLoginPold	= eshLoginAllow;
    break;
  case denymarker:
    eLoginPold	= eshLoginDeny;
    break;
  default:
    eLoginPold	= eshLoginIgnore;
    break;
  }

  switch ( eLoginP ) {
  case eshLoginIgnore:
    fnMachLoginP ( oMachine, unbound );
    break;
  case eshLoginAllow:
    fnMachLoginP ( oMachine, allowmarker );
    break;
  case eshLoginDeny:
    fnMachLoginP ( oMachine, denymarker );
    break;
  default:
    break;
  }

  UnstoreSession ();
  RETURN ( eLoginPold );
} EndFunction ( fnClientMachineLoginP );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientMachineAddr, "c-sh-machine-addr",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortMachine )
		   and
		   argument ( VECTOR ( int, 4 ), vector_out, pnAddr ) ) )
{
  OBJID		oMachine;
  LPPLOBMACH	lpMachine;
  int		i;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( FALSE );
    }
  }
  ASSERT ( StableHeap_is_open );

  oMachine	= Short2LongObjId ( oShortMachine );
  lpMachine	= fnGetPlobMach ( oMachine );

  for ( i = 0; i < length ( lpMachine->onAddr ); i++ ) {
    pnAddr [ i ]	= ObjId2Fixnum ( lpMachine->onAddr [ i ] );
  }

  UnstoreSession ();
  RETURN ( TRUE );
} EndFunction ( fnClientMachineAddr );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbMachineSearch, "c-sh-search-machine",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( VECTOR ( int, 4 ), vector_in, nAddr ) ) )
{
  OBJID		oHeap, oHeapUser, oHeapMachine;
  OBJID		oMachine;
  SHORTOBJID	oShortMachine;

  PROCEDURE	( fnClientDbMachineSearch );
  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  oHeapUser	= fnHeapUser ( oHeap );
  oHeapMachine	= fnHeapMachine ( oHeap );
  if ( fnAdminP ( oHeapUser, oHeapMachine ) != eaAdminFalse ) {
    oMachine	= fnMachSearchByAddr ( nAddr );
    oShortMachine	= ( boundp ( oMachine ) ) ?
      Long2ShortObjId ( oMachine ) : NULLOBJID;
  } else {
    ERROR (( "Only the administrator is allowed to search machines." ));
    UnstoreSession ();
    RETURN ( NULLOBJID );
  }

  UnstoreSession ();
  RETURN ( oShortMachine );
} EndFunction ( fnClientDbMachineSearch );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbMachineDelete, "c-sh-delete-machine",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( VECTOR ( int, 4 ), vector_in, nAddr ) ) )
{
  OBJID		oHeap, oHeapUser, oHeapMachine;
  OBJID		oMachine;
  SHORTOBJID	oShortMachine = NULLOBJID;

  PROCEDURE	( fnClientDbMachineDelete );
  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  oHeapUser	= fnHeapUser ( oHeap );
  oHeapMachine	= fnHeapMachine ( oHeap );
  if ( fnAdminP ( oHeapUser, oHeapMachine ) != eaAdminFalse ) {
    oMachine		= fnMachDeleteByAddr ( nAddr );
    oShortMachine	= ( boundp ( oMachine ) ) ?
      Long2ShortObjId ( oMachine ) : NULLOBJID;
  } else {
    ERROR (( "Only the administrator is allowed to delete machines." ));
    UnstoreSession ();
    RETURN ( NULLOBJID );
  }

  UnstoreSession ();
  RETURN ( oShortMachine );
} EndFunction ( fnClientDbMachineDelete );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbMachineInsert, "c-sh-insert-machine",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortMachine ) ) )
{
  OBJID		oHeap, oHeapUser, oHeapMachine;
  OBJID		oMachine = NULLOBJID, oMachines = NULLOBJID;
  SHORTOBJID	oReturn = NULLOBJID;
  LPPLOBMACH	lpMachine;
  int		i, nAddr [ 4 ];

  PROCEDURE	( fnClientDbMachineInsert );
  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }

  oHeap		= Short2LongObjId ( oShortObjIdHeap );
  oHeapUser	= fnHeapUser ( oHeap );
  oHeapMachine	= fnHeapMachine ( oHeap );
  if ( fnAdminP ( oHeapUser, oHeapMachine ) != eaAdminFalse ) {
    oMachine	= Short2LongObjId ( oShortMachine );
    lpMachine	= fnGetPlobMach ( oMachine );
    for ( i = 0; i < length ( nAddr ); i++ ) {
      nAddr [ i ]	= ObjId2Fixnum ( lpMachine->onAddr [ i ] );
    }
    if ( ! boundp ( fnMachSearchByAddr ( nAddr ) ) ) {
      oMachines	= fnMachs ();
      fnBTreeInsertByObjId ( NULLOBJID, oMachines,
			     lpMachine->oszName, oMachine );
      oReturn	= oShortMachine;
    }
  } else {
    ERROR (( "Only the administrator is allowed to insert machines." ));
    UnstoreSession ();
    RETURN ( NULLOBJID );
  }

  UnstoreSession ();
  RETURN ( oReturn );
} EndFunction ( fnClientDbMachineInsert );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbMachines, "c-sh-machines",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  OBJID		oHeap, oHeapUser, oHeapMachine;
  OBJID		oMachines = NULLOBJID;
  SHORTOBJID	oShortMachines;

  INITIALIZEPLOB;
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( NULLOBJID );
    }
  }
  ASSERT ( StableHeap_is_open );

  oHeap			= Short2LongObjId ( oShortObjIdHeap );
  oHeapUser		= fnHeapUser ( oHeap );
  oHeapMachine		= fnHeapMachine ( oHeap );
  if ( fnAdminP ( oHeapUser, oHeapMachine ) != eaAdminFalse ) {
    oMachines		= Machs ();
  } else {
    ERROR (( "Only the administrator is allowed to request the list\n"
	     "       of known machines." ));
    UnstoreSession ();
    RETURN ( NULLOBJID );
  }
  oShortMachines	= ( boundp ( oMachines ) ) ?
    Long2ShortObjId ( oMachines ) : NULLOBJID;

  UnstoreSession ();
  RETURN ( oShortMachines );
} EndFunction ( fnClientDbMachines );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
