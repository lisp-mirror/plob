/* -------------------------------------------------------------------------
| Module	cplob.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1996/09/23
| Description	PLOB client source code.
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
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobnumber.h"
#include	"cplobsequ.h"
#include	"cplobstruct.h"
#include	"cplobclos.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobroot.h"
#include	"cplobadmin.h"

#define		RPCNOTYPES
#define		RPC_CLNT	1
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening: */
#define	LOGGING	0x00	/* 0 (no), 1 (flush object), 2 (create object) */

/* ----------------------------------------------------------------------- */
/* Persistent object pre-allocation. The idea is not to allocate
   objects one-by-one on client requests done by the LISP layer to the
   server, but to allocate a whole bunch of objects in this layer from
   the server and to give it one-by-one to the LISP layer. */
enum {
  nMaxObjIdBuffer	= 128	/* objects */,
  nMaxExtraLengths	= 256	/* 0 ... 255 words */
};
typedef struct {
  /* Number of preallocated objects: */
  u_int		nShortObjIds;
  /* The short objids of all preallocated objects: */
  SHORTOBJID	ShortObjIds [ nMaxObjIdBuffer ];
}	OBJIDBUFFER, * POBJIDBUFFER;
typedef struct {
  /* Size of objids contained in PLOB object measured in bits: */
  int		nFixSizeObjId;
  /* Size of values contained in PLOB object in elements: */
  int		nFixValues;
  /* The type flags: */
  TYPEFLAGS	nTypeFlags;
  union {
    /* pObjIds is referenced for ( nTypeFlags & typeVarSizeMask ) == 0: */
    POBJIDBUFFER	pObjIds;
    /* pObjIds is referenced for ( nTypeFlags & typeVarSizeMask ) !=
       0; indexed by nExtraReferences resp. nExtraValues: */
    POBJIDBUFFER	* ppObjIds;
  }	Buffer;
}	CLNTTYPEINFO, * PCLNTTYPEINFO;

/* key is a SHTYPETAG, data is an CLNTTYPEINFO: */
static HASHTABLE	PreAllocated;

/* ----------------------------------------------------------------------- */
const char		szExpectedAtIndex []	=
"Expected object of class %s at\n"
"       slot location %d in "
UNREADABLE_OBJECT_PREFIX "object short-objid=%u" UNREADABLE_OBJECT_SUFFIX
"; received tag %d";

/* ----------------------------------------------------------------------- */
static const char	szIndexOverflow []	=
"Slot location %d is too big for "
UNREADABLE_OBJECT_PREFIX "object short-objid=%u" UNREADABLE_OBJECT_SUFFIX
", maximum is %d";

static const char	szExpectedClass []	=
"Expected object with class tag %d, not %d\n"
"       for "
UNREADABLE_OBJECT_PREFIX "object short-objid=%u" UNREADABLE_OBJECT_SUFFIX;

/* -------------------------------------------------------------------------
| Prototypes
 ------------------------------------------------------------------------- */
static void	fnInitializePreAllocated	( void );
static BOOL	__bInitializePreAllocated__	= TRUE;
#define		INITIALIZEPREALLOCATED \
((__bInitializePreAllocated__)?\
 (fnInitializePreAllocated(),TRUE):FALSE)

/* ----------------------------------------------------------------------- */
int DLLEXPORT		fnStartRemoteServer	( CONST_STRING	szURL,
						  GETACTION	eAction )
{
  struct timeval	Timeout;
  char		szServer [ MAX_FNAME ];
  char		szTransport [ MAX_FNAME ];
  char		szDirectory [ MAX_FNAME ];
  LPCSTR	pszServer;
  PCLIENT	pClient = (PCLIENT) NULL;
  int		i;
  int		nRpcPortCurr = -1, nRpcPortDb = -1;
  char		szServerName [ MAXNETNAMELEN + 1 ];
  fnServerGetPortByDirectory_args	arg;
  fnServerGetPortByDirectory_rets	clnt_res;

  PROCEDURE	( fnStartRemoteServer );

  strncpy ( szGlobalDirectory, szURL, sizeof ( szGlobalDirectory ) );
  szServer [ 0 ]	= '\0';
  szTransport [ 0 ]	= '\0';
  szDirectory [ 0 ]	= '\0';
  fnSplitURL ( szURL, szServer, szTransport, szDirectory );

  pClient	= fnClientPlobd ();

  if ( pClient != NULL ) {
    pszServer	= fnClientPlobdHost ();
    if ( strcmp ( szServer, pszServer ) != 0 ) {
      fnHeapCloseAll ();
      pClient	= fnClientDestroy ( pClient );
    }
  }

  if ( pClient != NULL ) {
    /* Send a NULLPROC request to server, to check if it is alive: */
    if ( ! fnClientPlobdFlush ( pClient ) ) {
      fnHeapCloseAll ();
      pClient	= fnClientDestroy ( pClient );
    }
  }

  if ( pClient != NULL ) {
    nRpcPortCurr	= (int) fnPlobdGetPort ();
  } else {
    fnInvalidateAllCaches ();
    nRpcPortCurr	= nMasterPort;
    fnPlobdSetPort ( nRpcPortCurr );
    pClient		= fnClientCreate ( szServer, szTransport );
  }

  ASSERT ( pClient != NULL );

  while ( ! fnClientPlobdFlush ( pClient ) ) {
    pClient	= fnClientDestroy ( pClient );
    pClient	= fnClientCreate ( szServer, szTransport );
    if ( pClient == NULL ) {
      RETURN ( nRpcPortDb );
    }
  }

  /* Get the database-specific Port number from server: */
  while ( nRpcPortDb < 0 ) {
    nRpcPortDb	=
      fnServerGetPortByDirectory ( szDirectory, (GETACTION)
				      ( ( (unsigned int) eAction &
					  ~ ( (unsigned int)
					      eGetPortPassive |
					      (unsigned int)
					      eGetPortActive ) ) |
					(unsigned int) eGetPortPassive ) );
    if ( nRpcPortDb < 0 ) {
      if ( ( (unsigned int) eAction &
	     (unsigned int) eCreateDatabase ) != 0 ) {
	ERROR (( "Could not create database %s", szURL ));
	RETURN ( nRpcPortDb );
      } else {
	CERROR (( "Try to create it.",
		  "Could not open database %s", szURL ));
	eAction	= (GETACTION)
	  ( (unsigned int) eAction | (unsigned int) eCreateDatabase );
      }
    }
  }

  if ( nRpcPortDb > 0 ) {
    /* If a server start is requested, do the start now: */
    if ( ( (unsigned int) eAction & (unsigned int) eStartServer ) != 0 ) {
      memset ( &Timeout, 0, sizeof ( Timeout ) );
      memset ( (char *) &arg, 0, sizeof ( arg ) );
      arg.szDirectory	= szDirectory;
      arg.eAction	= eAction;
      Timeout.tv_sec	= 4;
      nRpcPortDb	= -1;
      for ( i = 0; i < 20 && nRpcPortDb <= 0; i++ ) {
	if ( i == 1 ) {
	  INFO (( "Starting server for %s, please wait ...", szURL ));
	}
	if ( clnt_call ( pClient, NULLPROC,
			 (xdrproc_t) xdr_void, (caddr_t) NULL,
			 (xdrproc_t) xdr_void, (caddr_t) NULL,
			 Timeout ) == RPC_SUCCESS ) {
	  memset ( (char *) &clnt_res, 0, sizeof ( clnt_res ) );
	  if ( clnt_call ( pClient, fnRpc_fnServerGetPortByDirectory,
			   (xdrproc_t) xdr_fnServerGetPortByDirectory_args,
			   (caddr_t) &arg,
			   (xdrproc_t) xdr_fnServerGetPortByDirectory_rets,
			   (caddr_t) &clnt_res, Timeout ) == RPC_SUCCESS ) {
	    nRpcPortDb	= clnt_res.ReturnValue;
	    if ( clnt_res.nErrorLvl ) {
	      /* Server returned a user error message: */
	      char	szErrorMsg [ 1024 ];
	      nRpcPortDb	= -1;
	      strncpy ( szErrorMsg,
			( clnt_res.pszErrorMsg != NULL &&
			  clnt_res.pszErrorMsg [ 0 ] != '\0' ) ?
			(LPCSTR) clnt_res.pszErrorMsg : (LPCSTR) szEmpty,
			sizeof ( szErrorMsg ) );
	      szErrorMsg [ sizeof ( szErrorMsg ) - 1 ]	= '\0';
	      xdr_free ( (xdrproc_t) xdr_fnServerGetPortByDirectory_rets,
			 (char *) &clnt_res );
	      fnLISPerrorCallback ( clnt_res.nErrorLvl, __szProc__,
				    szErrorMsg );
	    } else {
	      xdr_free ( (xdrproc_t) xdr_fnServerGetPortByDirectory_rets,
			 (char *) &clnt_res );
	    }
	  }
	}
      }
    } else if ( ( (unsigned int) eAction &
		  (unsigned int) eGetPortActive ) != 0 ) {
      /* Check if there's a server running on the requested rpc
         Port number: */
      if ( nRpcPortCurr != nRpcPortDb ) {
	/* Switch RPC Port number: */
	fnInvalidateAllCaches ();
	pClient		= fnClientDestroy ( pClient );
	nRpcPortCurr	= nRpcPortDb;
	fnPlobdSetPort ( nRpcPortDb );
	pClient		= fnClientPlobdCreate ( szServer, szTransport );
      }
      if ( pClient == NULL || ! fnClientPlobdFlush ( pClient ) ) {
	nRpcPortDb	= -1;
      }
    }
  }

  if ( nRpcPortDb > 0 && nRpcPortCurr != nRpcPortDb ) {
    /* Switch RPC Port number: */
    fnInvalidateAllCaches ();
    pClient		= fnClientDestroy ( pClient );
    nRpcPortCurr	= nRpcPortDb;
    fnPlobdSetPort ( nRpcPortDb );
    pClient		= fnClientCreate ( szServer, szTransport );
  }

  RETURN ( nRpcPortDb );
} /* fnStartRemoteServer */

/* ----------------------------------------------------------------------- */
SHORTOBJID		fnOpen			( CONST_STRING	szURL,
						  GETACTION	eAction,
						  CONST_STRING
						  szDescription )
{
  SHORTOBJID	oHeap = NULLOBJID;

  PROCEDURE	( fnOpen );

  if ( fnStartRemoteServer ( szURL, eAction ) >= 0  ) {
    PCLIENT	pClient = (PCLIENT) NULL;
    int		nAuth = -1;
    AUTH	* pAuth = (AUTH *) NULL;
    char	szHost [ MAX_FNAME ], szDirectory [ MAX_FNAME ];
    pClient	= fnClientPlobd ();
    if ( pClient != NULL ) {
      fnSplitURL ( szURL, szHost, (LPSTR) NULL, szDirectory );
      pAuth	= fnCreateAuth ( szHost, &nAuth );
      if ( pAuth != NULL ) {
	auth_destroy ( pClient->cl_auth );
	pClient->cl_auth	= pAuth;
      }
      oHeap	= fnServerDbOpen ( szDirectory, fnGetUser (), szDescription,
				   (FIXNUM) NULL,
				   &oGlobalMinObjId, &oGlobalMaxObjId );
      if ( nAuth != AUTH_NONE ) {
	pAuth			= authnone_create ();
	auth_destroy ( pClient->cl_auth );
	pClient->cl_auth	= pAuth;
      }
    }
  }

  RETURN ( oHeap );
} /* fnOpen */

/* ----------------------------------------------------------------------- */
PCLIENT		fnClientDestroy		( PCLIENT	pClient )
{
  PROCEDURE	( fnClientDestroy );

  oGlobalMinObjId	= NULLOBJID;
  oGlobalMaxObjId	= NULLOBJID - 1;
  pClient		= fnClientPlobdDestroy ( pClient );

  RETURN ( pClient );
} /* fnClientDestroy */

/* ----------------------------------------------------------------------- */
void		fnInitializePlobModule	( void )
{
  PROCEDURE	( fnInitializePlobModule );

#if WIN32
  {
    WORD	wVersionRequested;
    WSADATA	wsaData;
    wVersionRequested	= MAKEWORD ( 2, 0 );
    WSAStartup ( wVersionRequested, &wsaData );
  }
#endif

  szGlobalDirectory [ 0 ]	= '\0';
  HashCreate ( &PreAllocated, 32, sizeof ( CLNTTYPEINFO ) );

  RETURN ( VOID );
} /* fnInitializePlobModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitializePlobModule	( void )
{
  PROCEDURE	( fnDeinitializePlobModule );

  fnInvalidatePlobCache ();
  fnHashDestroy ( &PreAllocated );

  RETURN ( VOID );
} /* fnDeinitializePlobModule */

/* ----------------------------------------------------------------------- */
static SHORTOBJID	fnCreateObjIdFromBuffer
			  ( POBJIDBUFFER	* ppObjIdBuffer,
			    SHORTOBJID		oShortObjIdHeap,
			    SHTYPETAG		nTypeTag,
			    FIXNUM		nExtraReferences,
			    SHTYPETAG		eTypeTagValues,
			    FIXNUM		nExtraValues )
{
  static BOOL	bFirstTime	= TRUE;
  SHORTOBJID	oShortObjId	= NULLOBJID;
  
  PROCEDURE	( fnCreateObjIdFromBuffer );

  if ( ppObjIdBuffer != NULL ) {
    if ( *ppObjIdBuffer == NULL ) {
      *ppObjIdBuffer	= (POBJIDBUFFER)
	Malloc ( sizeof ( *(*ppObjIdBuffer) ) );
      ASSERT ( *ppObjIdBuffer != NULL );
      memset ( *ppObjIdBuffer, 0, sizeof ( *(*ppObjIdBuffer) ) );
    }
    /* Check if there are still objects left in the pre-allocate
       buffer; if not, allocate a new bunch of objects: */
    if ( (*ppObjIdBuffer)->nShortObjIds <= 0 ) {
      (*ppObjIdBuffer)->nShortObjIds	=
	fnServerDbCreateObjects ( oShortObjIdHeap, nTypeTag,
				  nExtraReferences,
				  eTypeTagValues, nExtraValues,
				  length ( (*ppObjIdBuffer)->ShortObjIds ),
				  (u_int *) (*ppObjIdBuffer)->ShortObjIds );
    }
    if ( (*ppObjIdBuffer)->nShortObjIds > 0 ) {
      /* Take one object from the pre-allocate buffer: */
      oShortObjId	=
	(*ppObjIdBuffer)->ShortObjIds [ --((*ppObjIdBuffer)->nShortObjIds) ];
      (*ppObjIdBuffer)->ShortObjIds [ (*ppObjIdBuffer)->nShortObjIds ] =
	NULLOBJID;
      ASSERT ( oShortObjId != NULLOBJID );
    }
  }
  RETURN ( oShortObjId );
}

/* ----------------------------------------------------------------------- */
static void		fnDestroyObjIdToBuffer
			  ( POBJIDBUFFER	pObjIdBuffer,
			    SHORTOBJID		oShortObjIdHeap,
			    SHORTOBJID		oShortObjId )
{
  PROCEDURE	( fnDestroyObjIdToBuffer );
  if ( pObjIdBuffer != NULL &&
       pObjIdBuffer->nShortObjIds < length ( pObjIdBuffer->ShortObjIds ) ) {
    /* Put the object back into the PreAllocate table: */
    pObjIdBuffer->ShortObjIds [ pObjIdBuffer->nShortObjIds++ ]	=
      oShortObjId;
  } else {
    fnServerObjectDestroy ( oShortObjIdHeap, oShortObjId );
  }
  RETURN ( VOID );
} /* fnDestroyObjIdToBuffer */

/* ----------------------------------------------------------------------- */
static void		fnFreeObjIdBuffer	( POBJIDBUFFER	pObjIdBuffer,
						  BOOL		bFreeObjIds )
{
  u_int		i, n;

  PROCEDURE	( fnFreeObjIdBuffer );

  if ( pObjIdBuffer != NULL ) {
    ASSERT ( pObjIdBuffer->nShortObjIds <=
	     length ( pObjIdBuffer->ShortObjIds ) );
    if ( bFreeObjIds ) {
      /* Connection to server exists; free the objects on the server, too: */
      for ( i = 0, n = pObjIdBuffer->nShortObjIds; i < n; i++ ) {
	ASSERT ( pObjIdBuffer->ShortObjIds [ i ] != NULLOBJID );
	fnServerObjectDestroy ( NULLOBJID, pObjIdBuffer->ShortObjIds [ i ] );
	pObjIdBuffer->nShortObjIds--;
	pObjIdBuffer->ShortObjIds [ i ]	= NULLOBJID;
      }
    } else {
      /* No connection to server exists; simply forget the objects: */
      pObjIdBuffer->nShortObjIds	= 0;
      memset ( pObjIdBuffer->ShortObjIds, 0,
	       sizeof ( pObjIdBuffer->ShortObjIds ) );
    }
    ASSERT ( pObjIdBuffer->nShortObjIds == 0 );
  }
  RETURN ( VOID );
} /* fnFreeObjIdBuffer */

/* ----------------------------------------------------------------------- */
void			fnInvalidatePlobCache		( void )
{
  PCLIENT	pClient	= (PCLIENT) NULL;
  BOOL		bMapped;
  PCLNTTYPEINFO	pClntTypeInfo;
  u_int		i;

  PROCEDURE	( fnInvalidatePlobCache );

  pClient	= fnClientPlobd ();
  for ( bMapped = fnHashFirst ( &PreAllocated, (LPHASHKEY) NULL,
				(LPVOID *) &pClntTypeInfo, (size_t *) NULL );
	bMapped;
	bMapped = fnHashNext ( &PreAllocated, (LPHASHKEY) NULL,
			       (LPVOID *) &pClntTypeInfo, (size_t *) NULL ) ) {
    if ( ( pClntTypeInfo->nTypeFlags & typeVarSizeMask ) == 0 ) {
      if ( pClntTypeInfo->Buffer.pObjIds != NULL ) {
	fnFreeObjIdBuffer ( pClntTypeInfo->Buffer.pObjIds,
			    (BOOL) ( pClient != NULL ) );
	Free ( pClntTypeInfo->Buffer.pObjIds );
	pClntTypeInfo->Buffer.pObjIds	= (POBJIDBUFFER) NULL;
      }
    } else if ( pClntTypeInfo->Buffer.ppObjIds != NULL ) {
      for ( i = 0; i < nMaxExtraLengths; i++ ) {
 	if ( pClntTypeInfo->Buffer.ppObjIds [ i ] != NULL ) {
	  fnFreeObjIdBuffer ( pClntTypeInfo->Buffer.ppObjIds [ i ],
			      (BOOL) ( pClient != NULL ) );
	  Free ( pClntTypeInfo->Buffer.ppObjIds [ i ] );
	  pClntTypeInfo->Buffer.ppObjIds [ i ]	= (POBJIDBUFFER) NULL;
	}
      }
      Free ( pClntTypeInfo->Buffer.ppObjIds );
      pClntTypeInfo->Buffer.ppObjIds	= (POBJIDBUFFER *) NULL;
    }
  }
  RETURN ( VOID );
} /* fnInvalidatePlobCache */

/* ----------------------------------------------------------------------- */
void			fnInvalidateAllCaches		( void )
{
  PROCEDURE	( fnInvalidateAllCaches );

  /* Caches of cplob: */
  oTypeTagCache			= NULLOBJID;
  nTypeTagCache			= (SHTYPETAG) NULLTYPETAG;

  fnInvalidatePlobCache   ();
  fnInvalidateStructCache ();
  fnInvalidateCLOSCache   ();

  RETURN ( VOID );
} /* fnInvalidateAllCaches */

/* ----------------------------------------------------------------------- */
PCLIENT		fnClientCreate	( LPCSTR	pszHost,
				  LPCSTR	pszTransport )
{
  char		szHost [ MAX_FNAME ];
  char		szTransport [ MAX_FNAME ];
  char		szContinue [ 80 ];
  PCLIENT	pClient	= (PCLIENT) NULL;

  PROCEDURE	( fnClientCreate );

  ASSERT ( pszHost != NULL );
  ASSERT ( pszTransport != NULL );

  strncpy ( szHost, pszHost, sizeof ( szHost ) );
  strncpy ( szTransport, pszTransport, sizeof ( szTransport ) );
  szContinue [ 0 ]	= '\0';
  while ( pClient == NULL ) {
    pClient	= fnClientPlobdCreate ( szHost, szTransport );
    if ( pClient == NULL ) {
      char	szError [ 256 ];
      int	l;
      LPCSTR	pszMaster	= ( fnPlobdGetPort () == nMasterPort ) ?
	"master " : szEmpty;
      if ( ! szContinue [ 0 ] ) {
	sprintf ( szContinue, "Retry to connect to host %s.", szHost );
      }
      strncpy ( szError, clnt_spcreateerror ( szHost ),
		sizeof ( szError ) );
      szError [ sizeof ( szError ) - 1 ]	= '\0';
      for ( l = strlen ( szError ) - 1; l >= 0 && szError [ l ] <= ' '; l-- ) {
	szError [ l ]	= '\0';
      }
      CERROR (( szContinue,
		"Connect to %sserver on host %s failed:\n"
		"       %s\n"
		"       Check if a PLOB! %sserver process"
		" is running on the host.",
		pszMaster, szHost, szError, pszMaster ));
    }
  }
  return pClient;
} /* fnClientCreate */

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnClientDbClose, "c-sh-close",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( BOOL, value_in, bWithGarbageCollection ) ) )
{
  struct timeval	Timeout;
  PCLIENT	pClient	= (PCLIENT) NULL;
  FIXNUM	nHeapsLeft;

  INITIALIZEPLOB;

  pClient	= fnClientPlobd ();
  if ( pClient != NULL ) {
    nHeapsLeft	= fnCacheDestroy ( oShortObjIdHeap );
    if ( nHeapsLeft == 0 ) {
      fnInvalidateAllCaches ();
    }
    /* Send a NULLPROC request to server, to check if it is alive: */
    memset ( &Timeout, 0, sizeof ( Timeout ) );
    Timeout.tv_sec	= nDefaultTimeout;
    if ( clnt_call ( pClient, NULLPROC, xdr_void, (caddr_t) NULL,
		     xdr_void, (caddr_t) NULL,
		     Timeout ) != RPC_SUCCESS ) {
      pClient	= fnClientDestroy ( pClient );
    }
    if ( pClient != NULL ) {
      fnServerDbClose ( oShortObjIdHeap, bWithGarbageCollection );
      fnClientPlobdFlush ( pClient );
      if ( nHeapsLeft == 0  ) {
	pClient	= fnClientDestroy ( pClient );
      }
    }
  }
  RETURN ( VOID );
} EndFunction ( fnClientDbClose );

/* ----------------------------------------------------------------------- */
static void	fnInitializePreAllocated	( void )
{
  static BOOL	bFirstTime	= TRUE;

  BOOL		bMapped;
  SHTYPETAG	nTypeTag;
  char		szTypeName [ 128 ];
  FIXNUM	nObjIdSize, nValues;
  TYPEFLAGS	nTypeFlags;
  CLNTTYPEINFO	ClntTypeInfo;

  PROCEDURE	( fnInitializePreAllocated );

  __bInitializePreAllocated__	= FALSE;
  if ( ! bFirstTime ) {
    RETURN ( VOID );
  }
  bFirstTime	= FALSE;
  memset ( &ClntTypeInfo, 0, sizeof ( ClntTypeInfo ) );
  for ( bMapped = fnLISPmapClassInfoFirst ( &nTypeTag, szTypeName,
					    sizeof ( szTypeName ),
					    &nObjIdSize,
					    &nValues, &nTypeFlags );
	bMapped;
	bMapped = fnLISPmapClassInfoNext ( &nTypeTag, szTypeName,
					   sizeof ( szTypeName ),
					   &nObjIdSize,
					   &nValues, &nTypeFlags ) ) {
    if ( nTypeTag != eshStructureTag /* Structures and CLOS instances */ &&
	 nTypeTag != eshInstanceTag  /* have their own pre-allocation */ &&
	 ( ! ( nTypeFlags & ( typeImmediateP | typeNotAllocatableP | 
			      typeTransientP | typeNotYetImplemented |
			      typeNotCachableP ) ) ) &&
	 HashGet ( &PreAllocated, nTypeTag ) == NULL ) {
      ClntTypeInfo.nFixSizeObjId	= nObjIdSize;
      ClntTypeInfo.nFixValues		= nValues;
      ClntTypeInfo.nTypeFlags		= nTypeFlags;
      HashInsert ( &PreAllocated, nTypeTag, &ClntTypeInfo );
    }
  }
  RETURN ( VOID );
} /* fnInitializePreAllocated */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
	        fnClientDbCreateObject, "c-sh-create-object",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTag )
		  and
		  argument ( FIXNUM, value_in, nExtraReferences )
		  and
		  argument ( SHTYPETAG, value_in, eTypeTagValues )
		  and
		  argument ( FIXNUM, value_in, nExtraValues ) ) )
{
  SHORTOBJID	oShortObjId = NULLOBJID;
  PCLNTTYPEINFO	pClntTypeInfo = (PCLNTTYPEINFO) NULL;
  SHLOCK	nLockStore;
  FIXNUM	nExtraValuesInWords;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    INITIALIZEPREALLOCATED;
    pClntTypeInfo	= (PCLNTTYPEINFO) HashGet ( &PreAllocated, nTypeTag );
    if ( pClntTypeInfo != NULL ) {
      switch ( pClntTypeInfo->nTypeFlags & typeVarSizeMask ) {
      case typeVarSizeObjIdP:
	/* The object is fixed-sized in its value slots; make sure
	   that no extra value slots have been requested: */
	ASSERT ( nExtraValues == 0 );
	if ( nExtraReferences < nMaxExtraLengths ) {
	  if ( pClntTypeInfo->Buffer.ppObjIds == NULL ) {
	    pClntTypeInfo->Buffer.ppObjIds	= (POBJIDBUFFER *)
	      Malloc ( sizeof ( *(pClntTypeInfo->Buffer.ppObjIds) ) *
		       nMaxExtraLengths );
	    memset ( pClntTypeInfo->Buffer.ppObjIds, 0,
		     sizeof ( *(pClntTypeInfo->Buffer.ppObjIds) ) *
		     nMaxExtraLengths );
	  }
	  oShortObjId	= fnCreateObjIdFromBuffer
	    ( & pClntTypeInfo->Buffer.ppObjIds [ nExtraReferences ],
	      oShortObjIdHeap, nTypeTag, nExtraReferences,
	      eTypeTagValues, nExtraValues );
	}
	break;
      case typeVarSizeValueP:
	/* The object is fixed-sized in its reference slots; make sure
	   that no extra reference slots have been requested: */
	ASSERT ( nExtraReferences == 0 );
	nExtraValuesInWords	=
	  fnTypeTagSizeValue ( 1, &eTypeTagValues, &nExtraValues );
	if ( nExtraValuesInWords < nMaxExtraLengths ) {
	  if ( pClntTypeInfo->Buffer.ppObjIds == NULL ) {
	    pClntTypeInfo->Buffer.ppObjIds	= (POBJIDBUFFER *)
	      Malloc ( sizeof ( *(pClntTypeInfo->Buffer.ppObjIds) ) *
		       nMaxExtraLengths );
	    memset ( pClntTypeInfo->Buffer.ppObjIds, 0,
		     sizeof ( *(pClntTypeInfo->Buffer.ppObjIds) ) *
		     nMaxExtraLengths );
	  }
	  oShortObjId	= fnCreateObjIdFromBuffer
	    ( & pClntTypeInfo->Buffer.ppObjIds [ nExtraValuesInWords ],
	      oShortObjIdHeap, nTypeTag, nExtraReferences, eTypeTagValues,
	      nExtraValues );
	}
	break;
      case 0:
	/* The object is fixed-sized; make sure that no extra
           reference and value slots have been requested: */
	ASSERT ( nExtraReferences == 0 );
	ASSERT ( nExtraValues == 0 );
	oShortObjId	= fnCreateObjIdFromBuffer
	  ( &pClntTypeInfo->Buffer.pObjIds, oShortObjIdHeap, nTypeTag,
	    nExtraReferences, eTypeTagValues, nExtraValues );
	break;
      default:
	break;
      }
    }
  }
  if ( oShortObjId == NULLOBJID ) {
    oShortObjId	= fnServerDbCreateObject ( oShortObjIdHeap, nTypeTag,
					   nExtraReferences,
					   eTypeTagValues, nExtraValues );
  }

  if ( bGlobalDoCaching && pClntTypeInfo != NULL &&
       oShortObjId != NULLOBJID ) {
    nLockStore	= fnCacheLockP ( oShortObjIdHeap, NULLOBJID,
				 eshLockStoreWrite );
    if ( (int) nLockStore > 0 ) {
      /* We have a store write lock. This implies that no lock on vector
	 level will be necessary for modifying the object. So, create
	 the object into the cache and mark it as write locked: */
#if (LOGGING+0) & 0x02
      char	szObject [ 256 ], szHeap [ 256 ];
      fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjId,
				  eshShortObjIdTag,
				  szObject, sizeof ( szObject ) );
      fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjIdHeap,
				  eshShortObjIdTag,
				  szHeap, sizeof ( szHeap ) );
      INFO (( "Created %s\n"
	      "       into %s\n"
	      "       with store lock level 0x%X",
	      szObject, szHeap, nLockStore ));
#endif /* #if (LOGGING+0) & 0x01 */
      fnCacheCreateObject ( oShortObjIdHeap, oShortObjId, nTypeTag,
			    AlignBitsToWords ( pClntTypeInfo->nFixSizeObjId ) +
			    nExtraReferences,
			    eTypeTagValues, nExtraValues,
			    nLockStore );
    }
  }
  RETURN ( oShortObjId );
} EndFunction ( fnClientDbCreateObject );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnClientObjectDestroy, "c-sh-destroy-object",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  static BOOL	bFirstTime	= TRUE;
  SHTYPETAG	nTypeTag;
  PCLNTTYPEINFO	pClntTypeInfo;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    INITIALIZEPREALLOCATED;
    nTypeTag	= typetagof ( SHORT2LONGOBJID ( oShortObjIdHeap ),
			      SHORT2LONGOBJID ( oShortObjId ) );
    pClntTypeInfo	= (PCLNTTYPEINFO) HashGet ( &PreAllocated, nTypeTag );
    if ( pClntTypeInfo != NULL &&
	 ( pClntTypeInfo->nTypeFlags & typeRecycleP ) != 0 &&
	 ( pClntTypeInfo->nTypeFlags & typeVarSizeMask ) == 0 ) {
      /* Put the object back into the PreAllocate table: */
      fnDestroyObjIdToBuffer ( pClntTypeInfo->Buffer.pObjIds,
			       oShortObjIdHeap, oShortObjId );
    } else {
      fnServerObjectDestroy ( oShortObjIdHeap, oShortObjId );
    }
    fnCacheDelete ( oShortObjIdHeap, oShortObjId );
  } else {
    fnServerObjectDestroy ( oShortObjIdHeap, oShortObjId );
  }
  RETURN ( VOID );
} EndFunction ( fnClientObjectDestroy );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientObjectObjIdSize, "c-sh-objid-size",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{ 
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      RETURN ( pHeapObjectCache->pObjectCache->nSlots );
    }
  }
  RETURN ( fnServerObjectObjIdSize ( oShortObjIdHeap, oShortObjId ) );
} EndFunction ( fnClientObjectObjIdSize );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHORTOBJID,
		fnClientDbOpen, "c-sh-short-open",
		( argument ( CONST_STRING, vector_in, szURL )
		  and
		  argument ( CONST_STRING, vector_in, szDescription )
		  and
		  argument ( FIXNUM, value_in, nMinAddrInK ) ) )
{
  SHORTOBJID	oHeap;

  INITIALIZEPLOB;

  /* 1998/07/17 HK: Debug: */
  /*
  INFO (( "&nGlobalFlagWord 0x%X", &nGlobalFlagWord ));
  INFO (( "nGlobalFlagWord    %d", nGlobalFlagWord ));
  */

  oHeap	= fnOpen ( szURL, (GETACTION)
		   ( (unsigned int) eGetPortActive |
		     (unsigned int) eStartServer ),
		   szDescription );
  if ( oHeap != NULLOBJID ) {
    fnCacheCreate ( oHeap );
  }
  RETURN ( oHeap );
} EndFunction ( fnClientDbOpen );

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnClientObjectFlush, "c-sh-flush-object",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( BOOL, value_in, bRemoveFromCache ) ) )
{
  BOOL	bFlushed;

  INITIALIZEPLOB;

#if (LOGGING+0) & 0x01
  INFO (( "Flush request for heap %d, short-objid %d",
	  oShortObjIdHeap, oShortObjId ));
#endif /* #if (LOGGING+0) & 0x01 */

  bFlushed	= ( fnCacheFlush  ( oShortObjIdHeap, oShortObjId ) > 0 );
  if ( bRemoveFromCache ) {
    fnCacheDelete ( oShortObjIdHeap, oShortObjId );
  }

  RETURN ( bFlushed );
} EndFunction ( fnClientObjectFlush );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectReadAtIndex, "c-sh-read-index",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( INTEGER, value_out, pnValue )
		  and
		  argument ( SHTYPETAG, value_out, pnTypeTag ) ) )

{
  OBJID			oRead;
  PHEAPOBJECTCACHE	pHeapObjectCache;
  SHTYPETAG		nTypeTag;
  SHLOCK		nLockOld;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      if ( nExpectingTypeTag != NULLTYPETAG &&
	   nExpectingTypeTag != pHeapObjectCache->pObjectCache->nTypeTag ) {
	ERROR (( szExpectedClass, nExpectingTypeTag,
		 pHeapObjectCache->pObjectCache->nTypeTag, oShortObjId ));
	RETURN ( eshGeneralError );
      }
      if ( nIndex < 0 || nIndex >= pHeapObjectCache->pObjectCache->nSlots ) {
	ERROR (( szIndexOverflow, nIndex, oShortObjId,
		 pHeapObjectCache->pObjectCache->nSlots ));
	RETURN ( eshGeneralError );
      }
      ASSERT ( pHeapObjectCache->pObjectCache->poSlots != NULL );
      oRead	= pHeapObjectCache->pObjectCache->poSlots [ nIndex ];
      ASSERT ( pHeapObjectCache->pObjectCache->pnTypeTags != NULL );
      nTypeTag	= pHeapObjectCache->pObjectCache->pnTypeTags [ nIndex ];
      if ( pnValue != NULL ) {
	*pnValue	= ( immediatep ( nTypeTag ) ) ?
	  fnObjId2Immediate ( oRead, nTypeTag ) :
	  LONG2SHORTOBJID ( oRead );
      }
      if ( pnTypeTag != NULL ) {
	if ( nTypeTag == eshObjIdTag ) {
	  /* Re-load the `real' type: */
	  nTypeTag	=
	    typetagof ( SHORT2LONGOBJID ( oShortObjIdHeap ), oRead );
	  pHeapObjectCache->pObjectCache->pnTypeTags [ nIndex ]	= nTypeTag;
	}
	*pnTypeTag	= nTypeTag;
      }
      nLockOld	= pHeapObjectCache->nVectorLockOld;
      pHeapObjectCache->nSlotsRead++;
      fnCacheCompletedP ( oShortObjIdHeap, oShortObjId );
      RETURN ( nLockOld );
    }
  }
  RETURN ( fnServerObjectReadAtIndex ( oShortObjIdHeap, oShortObjId,
				       oExpectingClass, nExpectingTypeTag,
				       nIndex, pnValue, pnTypeTag ) );
} EndFunction ( fnClientObjectReadAtIndex );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectReadAtIndices, "c-sh-read-indices",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( int, nObjIds ),
			     vector_out, pObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_out, pnTypeTags ) ) )
{
  OBJID			oRead;
  PHEAPOBJECTCACHE	pHeapObjectCache;
  SHTYPETAG		nTypeTag;
  int			nSlots, nSlotsClipped, s, nTotalRead = 0;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      if ( nExpectingTypeTag != NULLTYPETAG &&
	   nExpectingTypeTag != pHeapObjectCache->pObjectCache->nTypeTag ) {
	ERROR (( szExpectedClass, nExpectingTypeTag,
		 pHeapObjectCache->pObjectCache->nTypeTag, oShortObjId ));
	RETURN ( eshGeneralError );
      }
      nSlots		= pHeapObjectCache->pObjectCache->nSlots;
      nSlotsClipped	= MIN ( nSlots - nIndex, nObjIds );
      if ( nSlotsClipped > 0 ) {
	if ( nIndex < 0 || nIndex >= nSlots ) {
	  ERROR (( szIndexOverflow, nIndex, oShortObjId, nSlots ));
	  RETURN ( eshGeneralError );
	}
	nTotalRead	= nSlotsClipped;
	ASSERT ( pHeapObjectCache->pObjectCache->poSlots != NULL );
	ASSERT ( pHeapObjectCache->pObjectCache->pnTypeTags != NULL );
	for ( s = 0; s < nSlotsClipped; s++ ) {
	  oRead	= pHeapObjectCache->pObjectCache->poSlots [ nIndex + s ];
	  nTypeTag = pHeapObjectCache->pObjectCache->pnTypeTags [ nIndex + s ];
	  if ( pObjIds != NULL ) {
	    pObjIds [ s ]		= ( immediatep ( nTypeTag ) ) ?
	      fnObjId2Immediate ( oRead, nTypeTag ) :
	      LONG2SHORTOBJID ( oRead );
	  }
	  if ( pnTypeTags != NULL ) {
	    if ( nTypeTag == eshObjIdTag ) {
	      /* Re-load the `real' type: */
	      nTypeTag	=
		typetagof ( SHORT2LONGOBJID ( oShortObjIdHeap ), oRead );
	      pHeapObjectCache->pObjectCache->pnTypeTags [ nIndex ] = nTypeTag;
	    }
	    pnTypeTags [ s ]	= nTypeTag;
	  }
	}
	if ( nTotalRead > 0 ) {
	  pHeapObjectCache->nSlotsRead	+= nTotalRead;
	  fnCacheCompletedP ( oShortObjIdHeap, oShortObjId );
	}
      }
      RETURN ( nTotalRead );
    }
  }
  RETURN ( fnServerObjectReadAtIndices ( oShortObjIdHeap, oShortObjId,
					 oExpectingClass, nExpectingTypeTag,
					 nIndex,
					 nObjIds, pObjIds, pnTypeTags ) );
} EndFunction ( fnClientObjectReadAtIndices );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectReadObjId, "c-sh-read-objid",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( INTEGER, value_out, pnObjId ) ) )
{
  SHTYPETAG	nTypeTag;
  SHLOCK	nLockOld;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    nTypeTag	= eshObjIdTag;
    nLockOld	=
      fnClientObjectReadAtIndex ( oShortObjIdHeap, oShortObjId,
				  oExpectingClass, nExpectingTypeTag,
				  nIndex, pnObjId, &nTypeTag );
    if ( immediatep ( nTypeTag ) ) {
      ERROR (( szExpectedAtIndex, "objid", nIndex, oShortObjId,
	       nTypeTag ));
      RETURN ( eshGeneralError );
    }
    RETURN ( nLockOld );
  }
  RETURN ( fnServerObjectReadObjId ( oShortObjIdHeap, oShortObjId,
				     oExpectingClass, nExpectingTypeTag,
				     nIndex, pnObjId ) );
} EndFunction ( fnClientObjectReadObjId );

/* ----------------------------------------------------------------------- */
static FIXNUM	fnReadValuesInBlocks	( SHORTOBJID	oShortObjIdHeap,
					  SHORTOBJID	oShortObjId,
					  SHORTOBJID	oExpectingClass,
					  SHTYPETAG	nExpectingTypeTag,
					  FIXNUM	nIndex,
					  SHTYPETAG	nElementTypeTag,
					  FIXNUM	nSizeInElements,
					  void		*pBuffer )
{
  LPCLASSINFO	pClassInfo;
  int		nBitsPerElement, nTransferBlockSizeInElements;
  int		b, i, nToRead, nRead;
  int		nTotalRead	= 0;
  SHORTOBJID	oExp		= oExpectingClass;
  SHTYPETAG	nExp		= nExpectingTypeTag;

  PROCEDURE	( fnReadValuesInBlocks );

  pClassInfo		= (LPCLASSINFO) FindClassInfo ( nElementTypeTag );
  ASSERT ( pClassInfo != NULL );
  nBitsPerElement	= pClassInfo->nFixSizeValue;
  ASSERT ( nBitsPerElement > 0 );

  nTransferBlockSizeInElements	= (int)
    ( ( (unsigned long) nTransferBlockSizeInWords *
	(unsigned long) nSizeOfPostoreWord *
	(unsigned long) nBitsPerByte ) /
      (unsigned long) nBitsPerElement );

  /* Do a block-oriented transfer since the RPC layer barfs when
     transmitting very big memory areas (actually, I think this is a
     `feature' of the xdr layer): */
  for ( b = 0, i = 0; i < nSizeInElements;
	b += AlignBitsToWords ( nRead * nBitsPerElement ),
	  i += nRead, nIndex += nRead ) {
    nToRead	=
      MIN ( nTransferBlockSizeInElements, nSizeInElements - nIndex );
    if ( nToRead <= 0 ) {
      break;
    }
    nRead	=
      fnServerObjectReadValues ( oShortObjIdHeap, oShortObjId,
				 oExp, nExp, nIndex, 
				 nElementTypeTag, nToRead, NULL, NULL,
				 (void *) & ( (psint *) pBuffer ) [ b ] );
    if ( nRead < 0 ) {
      /* Error detected, e.g. a lock conflict: */
      RETURN ( nRead );
    }
    if ( nRead != nToRead ) {
      char	szObject [ 512 ];
      fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjId,
				  nExpectingTypeTag,
				  szObject, sizeof ( szObject ) );
      ERROR (( "Expected to read %d elements, read %d elements in block %d\n"
	       "       of %s",
	       nToRead, nRead, i / nTransferBlockSizeInElements,
	       szObject ));
      RETURN ( eshGeneralError );
    }
    nTotalRead	+= nRead;
    oExp	= NULLOBJID;
    nExp	= (SHTYPETAG) NULLTYPETAG;
  }
  RETURN ( nTotalRead );
} /* fnReadValuesInBlocks */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectReadValues, "c-sh-read-values",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( SHTYPETAG, value_in, nElementTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInElements )
		  and
		  argument ( VECTOR ( void,
				      fnTypeTagSizeValue(1,&nElementTypeTag,
							 &nSizeInElements ) ),
			     vector_out, pBuffer ) ) )
{
  LPCLASSINFO	pClassInfo;
  int			nAlignment, nBitsPerElement;
  PHEAPOBJECTCACHE	pHeapObjectCache;
  int			nValues, nValuesClipped, nTotalRead = 0;

  INITIALIZEPLOB;

  pClassInfo		= (LPCLASSINFO) FindClassInfo ( nElementTypeTag );
  ASSERT ( pClassInfo != NULL );
  nBitsPerElement	= pClassInfo->nFixSizeValue;
  ASSERT ( nBitsPerElement > 0 );
  nAlignment		=
    ( nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerElement;
  if ( nAlignment > 0 && nIndex % nAlignment != 0 ) {
    char	szObject [ 256 ];
    fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjId,
				nExpectingTypeTag,
				szObject, sizeof ( szObject ) );
    ERROR (( szInvalidAlignment, nIndex, szObject, nAlignment ));
    RETURN ( eshGeneralError );
  }

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      /* Finding the object in the cache implies at least a read lock,
         so no further checking for a read lock is done here: */
      if ( nExpectingTypeTag != NULLTYPETAG &&
	   nExpectingTypeTag != pHeapObjectCache->pObjectCache->nTypeTag ) {
	ERROR (( szExpectedClass, nExpectingTypeTag,
		 pHeapObjectCache->pObjectCache->nTypeTag ));
	RETURN ( eshGeneralError );
      }
      nValues		= pHeapObjectCache->pObjectCache->nValues;
      nValuesClipped	= MIN ( nValues - nIndex, nSizeInElements );
      if ( nValuesClipped > 0 ) {
	if ( nIndex < 0 || nIndex >= nValues ) {
	  ERROR (( szIndexOverflow, nIndex, oShortObjId,
		   pHeapObjectCache->pObjectCache->nSlots ));
	  RETURN ( eshGeneralError );
	}
	nTotalRead	= nValuesClipped;
	memcpy ( pBuffer,
		 & ( (psint *) pHeapObjectCache->pObjectCache->pValues )
		 [ AlignBitsToWords ( nIndex * nBitsPerElement ) ],
		 AlignBitsToWords ( nValuesClipped * nBitsPerElement ) *
		 sizeof ( psint ) );
      }
      if ( nTotalRead > 0 ) {
	pHeapObjectCache->nValuesRead	+= nTotalRead;
	fnCacheCompletedP ( oShortObjIdHeap, oShortObjId );
      }
      RETURN ( nTotalRead );
    }
  }
  RETURN ( fnReadValuesInBlocks ( oShortObjIdHeap, oShortObjId,
				  oExpectingClass, nExpectingTypeTag,
				  nIndex, nElementTypeTag,
				  nSizeInElements, pBuffer ) );
} EndFunction ( fnClientObjectReadValues );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
	        fnClientDbStabilise, "c-sh-stabilise",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap ) ) )
{
  PCLIENT	pClient	= (PCLIENT) NULL;

  INITIALIZEPLOB;

  pClient	= fnClientPlobd ();
  if ( pClient != NULL ) {
    fnCacheFlush ( NULLOBJID, NULLOBJID );
    fnServerDbStabilise ( oShortObjIdHeap );
    fnClientPlobdFlush ( pClient );
  }
} EndFunction ( fnClientDbStabilise );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHTYPETAG,
	        fnClientObjectTypeTag, "c-sh-type-tag",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      RETURN ( pHeapObjectCache->pObjectCache->nTypeTag );
    }
  }
  RETURN ( fnServerObjectTypeTag ( oShortObjIdHeap, oShortObjId ) );
} EndFunction ( fnClientObjectTypeTag );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
	        fnClientObjectValueSize, "c-sh-value-size",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId ) ) )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      RETURN ( pHeapObjectCache->pObjectCache->nValues );
    }
  }
  RETURN ( fnServerObjectValueSize ( oShortObjIdHeap, oShortObjId ) );
} EndFunction ( fnClientObjectValueSize );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectWriteAtIndex, "c-sh-write-index",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( FIXNUM, value_in, nValue )
		  and
		  argument ( SHTYPETAG, value_in, nTypeTagValue ) ) )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;
  FIXNUM		nValueOld, nValueNew;
  SHLOCK		nLockCache, nLockOld;

  INITIALIZEPLOB;

  /* Check if the heap was opened from the client: */
  if ( fnCacheGetHeap ( oShortObjIdHeap ) == NULL ) {
    RETURN ( eshGeneralError );
  }

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      if ( nExpectingTypeTag != NULLTYPETAG &&
	   nExpectingTypeTag != pHeapObjectCache->pObjectCache->nTypeTag ) {
	ERROR (( szExpectedClass, nExpectingTypeTag,
		 pHeapObjectCache->pObjectCache->nTypeTag ));
	RETURN ( eshGeneralError );
      }
      if ( nIndex < 0 || nIndex >= pHeapObjectCache->pObjectCache->nSlots ) {
	ERROR (( szIndexOverflow, nIndex, oShortObjId,
		 pHeapObjectCache->pObjectCache->nSlots ));
	RETURN ( eshGeneralError );
      }
      nValueOld		= pHeapObjectCache->pObjectCache->poSlots [ nIndex ];
      nLockCache	= fnCacheLockP ( oShortObjIdHeap, oShortObjId,
					 eshLockVectorWrite );
      nLockOld		= ( (int) nLockCache < 0 ) ?
	/* Cache-only locking failed, ask server to write: */
	fnServerObjectWriteAtIndex ( oShortObjIdHeap, oShortObjId,
				     oExpectingClass, nExpectingTypeTag,
				     nIndex, nValue, nTypeTagValue ) :
	nLockCache;
      if ( (int) nLockOld >= 0 ) {
	nValueNew	= fnImmediate2ObjId ( nValue, &nTypeTagValue );
	if ( nValueOld != nValueNew ) {
	  pHeapObjectCache->pObjectCache->poSlots [ nIndex ]	=
	    (OBJID) nValueNew;
	  if ( (int) nLockCache >= 0 ) {
	    pHeapObjectCache->pObjectCache->nChanges++;
	  }
	}
	pHeapObjectCache->pObjectCache->pnTypeTags [ nIndex ]	=
	  nTypeTagValue;
      }
      pHeapObjectCache->nSlotsWritten++;
      fnCacheCompletedP ( oShortObjIdHeap, oShortObjId );
      RETURN ( nLockOld );
    }
  }
  RETURN ( fnServerObjectWriteAtIndex ( oShortObjIdHeap, oShortObjId,
					oExpectingClass, nExpectingTypeTag,
					nIndex, nValue, nTypeTagValue ) );
} EndFunction ( fnClientObjectWriteAtIndex );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectWriteAtIndices, "c-sh-write-indices",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( int, nObjIds ), value_in, pObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_in, pnTypeTags ) ) )
{
  PHEAPOBJECTCACHE	pHeapObjectCache;
  int			nSlots, nSlotsClipped, s, nTotalWritten = 0;
  LPINT			pChanges = (LPINT) NULL;
  FIXNUM		nValueOld, nValueNew;
  SHTYPETAG		nTypeTagValue;

  INITIALIZEPLOB;

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      if ( nExpectingTypeTag != NULLTYPETAG &&
	   nExpectingTypeTag != pHeapObjectCache->pObjectCache->nTypeTag ) {
	ERROR (( szExpectedClass, nExpectingTypeTag,
		 pHeapObjectCache->pObjectCache->nTypeTag, oShortObjId ));
	RETURN ( eshGeneralError );
      }
      nSlots		= pHeapObjectCache->pObjectCache->nSlots;
      nSlotsClipped	= MIN ( nSlots - nIndex, nObjIds );
      if ( nSlotsClipped > 0 ) {
	if ( nIndex < 0 || nIndex >= nSlots ) {
	  ERROR (( szIndexOverflow, nIndex, oShortObjId, nSlots ));
	  RETURN ( eshGeneralError );
	}
	if ( pHeapObjectCache->pObjectCache->oHeap == oShortObjIdHeap ) {
	  pChanges	= &pHeapObjectCache->pObjectCache->nChanges;
	  nTotalWritten	= nSlotsClipped;
	} else {
	  pChanges	= (LPINT) NULL;
	  nTotalWritten	= 
	    fnServerObjectWriteAtIndices ( oShortObjIdHeap, oShortObjId,
					   oExpectingClass, nExpectingTypeTag,
					   nIndex,
					   nObjIds, pObjIds, pnTypeTags );
	  if ( nTotalWritten >= 0 ) {
	    /* The write placed implicit a vector lock onto oShortObjId: */
	    ASSERT ( ! boundp ( pHeapObjectCache->pObjectCache->oHeap ) );
	    pHeapObjectCache->pObjectCache->oHeap	= oShortObjIdHeap;
	    pHeapObjectCache->nVectorLockNow	= (SHLOCK)
	      ( (unsigned int) pHeapObjectCache->nVectorLockNow |
		(unsigned int) eshLockVectorWrite );
	    pHeapObjectCache->nVectorLockOld	= (SHLOCK)
	      ( (unsigned int) pHeapObjectCache->nVectorLockOld |
		(unsigned int) eshLockVectorWrite );
	  }
	}
	if ( nTotalWritten > 0 ) {
	  for ( s = 0; s < nSlotsClipped; s++ ) {
	    if ( pnTypeTags [ s ] != eshIgnoreSlotTag ) {
	      nValueOld		=
		pHeapObjectCache->pObjectCache->poSlots [ nIndex + s ];
	      nTypeTagValue	= (SHTYPETAG) pnTypeTags [ s ];
	      if ( nTypeTagValue == eshShortObjIdTag ) {
		nValueNew	= SHORT2LONGOBJID ( pObjIds [ s ] );
		nTypeTagValue	= eshObjIdTag;
	      } else if ( ! immediatep ( nTypeTagValue ) ) {
		char	szElement [ 256 ], szCached [ 256 ], szTypeTag [ 256 ];
		fnClientObjectPrettyPrint
		  ( LONG2SHORTOBJID ( pHeapObjectCache->pObjectCache->oHeap ),
		    pObjIds [ s ], eshShortObjIdTag,
		    szElement, sizeof ( szElement ) );
		fnClientObjectPrettyPrint
		  ( LONG2SHORTOBJID ( pHeapObjectCache->pObjectCache->oHeap ),
		    oShortObjId, pHeapObjectCache->pObjectCache->nTypeTag,
		    szCached, sizeof ( szCached ) );
		fnPrintImmediateObject ( nTypeTagValue, eshBuiltInTag,
					 szTypeTag, sizeof ( szTypeTag ) );
		CERROR (( "Change it to a short objid.",
			  "Encountered invalid long\n"
			  "       objid %d (%s),\n"
			  "       type %s,\n"
			  "       in cached object %s,\n"
			  "       at slot location %d",
			  pObjIds [ s ], szElement, szTypeTag, szCached,
			  nIndex + s ));
		nValueNew	= SHORT2LONGOBJID ( pObjIds [ s ] );
		nTypeTagValue	= eshObjIdTag;
	      } else {
		nValueNew	=
		  fnImmediate2ObjId ( pObjIds [ s ], &nTypeTagValue );
	      }
	      if ( nValueOld != nValueNew ) {
		pHeapObjectCache->pObjectCache->poSlots [ nIndex + s ]	=
		  nValueNew;
		if ( pChanges != NULL  ) {
		  (*pChanges)++;
		}
	      }
	      pHeapObjectCache->pObjectCache->pnTypeTags [ nIndex + s ]	=
		nTypeTagValue;
	    }
	  }
	  pHeapObjectCache->nSlotsWritten	+= nTotalWritten;
	  fnCacheCompletedP ( oShortObjIdHeap, oShortObjId );
	}
      }
      RETURN ( nTotalWritten );
    }
  }
  nTotalWritten	=
    fnServerObjectWriteAtIndices ( oShortObjIdHeap, oShortObjId,
				   oExpectingClass, nExpectingTypeTag,
				   nIndex, nObjIds, pObjIds, pnTypeTags );
  RETURN ( nTotalWritten );
} EndFunction ( fnClientObjectWriteAtIndices );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
	        fnClientObjectWriteObjId, "c-sh-write-objid",
	        ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdWrite ) ) )
{
  INITIALIZEPLOB;
  RETURN ( fnClientObjectWriteAtIndex ( oShortObjIdHeap, oShortObjId,
					oExpectingClass, nExpectingTypeTag,
					nIndex,
					oShortObjIdWrite, eshShortObjIdTag ) );
} EndFunction ( fnClientObjectWriteObjId );

/* ----------------------------------------------------------------------- */
static FIXNUM	fnWriteValuesInBlocks	( SHORTOBJID	oShortObjIdHeap,
					  SHORTOBJID	oShortObjId,
					  SHORTOBJID	oExpectingClass,
					  SHTYPETAG	nExpectingTypeTag,
					  FIXNUM	nIndex,
					  SHTYPETAG	nElementTypeTag,
					  FIXNUM	nSizeInElements,
					  void		*pBuffer )
{
  LPCLASSINFO	pClassInfo;
  int		nBitsPerElement, nTransferBlockSizeInElements;
  int		b, i, nToWrite, nWritten;
  int		nTotalWritten	= 0;
  SHORTOBJID	oExp		= oExpectingClass;
  SHTYPETAG	nExp		= nExpectingTypeTag;

  PROCEDURE	( fnWriteValuesInBlocks );

  pClassInfo		= (LPCLASSINFO) FindClassInfo ( nElementTypeTag );
  ASSERT ( pClassInfo != NULL );
  nBitsPerElement	= pClassInfo->nFixSizeValue;
  ASSERT ( nBitsPerElement > 0 );

  nTransferBlockSizeInElements	= (int)
    ( ( (unsigned long) nTransferBlockSizeInWords *
	(unsigned long) nSizeOfPostoreWord *
	(unsigned long) nBitsPerByte ) /
      (unsigned long) nBitsPerElement );

  /* Do a block-oriented transfer since the RPC layer barfs when
     transmitting very big memory areas (actually, I think this is a
     `feature' of the xdr layer): */
  for ( b = 0, i = 0; i < nSizeInElements;
	b += AlignBitsToWords ( nWritten * nBitsPerElement ),
	  i += nWritten, nIndex += nWritten ) {
    nToWrite	=
      MIN ( nTransferBlockSizeInElements, nSizeInElements - nIndex );
    if ( nToWrite <= 0 ) {
      break;
    }
    nWritten	=
      fnServerObjectWriteValues ( oShortObjIdHeap, oShortObjId,
				  oExp, nExp, nIndex,
				  nElementTypeTag, nToWrite,
				  (void *) & ( (int *) pBuffer ) [ b ] );
    if ( nWritten < 0 ) {
      /* Error detected, e.g. a lock conflict: */
      RETURN ( nWritten );
    }
    if ( nWritten != nToWrite ) {
      char	szObject [ 512 ];
      fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjId,
				  nExpectingTypeTag,
				  szObject, sizeof ( szObject ) );
      ERROR (( "Expected to write %d elements, wrote %d elements in block %d\n"
	       "       of %s",
	       nToWrite, nWritten, i / nTransferBlockSizeInElements,
	       szObject ));
      RETURN ( eshGeneralError );
    }
    nTotalWritten	+= nWritten;
    oExp		= NULLOBJID;
    nExp		= (SHTYPETAG) NULLTYPETAG;
  }
  RETURN ( nTotalWritten );
} /* fnWriteValuesInBlocks */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectWriteValues, "c-sh-write-values",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nIndex )
		  and
		  argument ( SHTYPETAG, value_in, nElementTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInElements )
		  and
		  argument ( VECTOR ( void,
				      fnTypeTagSizeValue(1,&nElementTypeTag,
							 &nSizeInElements ) ),
			     vector_in, pBuffer ) ) )
{
  LPCLASSINFO		pClassInfo;
  int			nAlignment, nBitsPerElement;
  PHEAPOBJECTCACHE	pHeapObjectCache;
  int			nValues, nValuesClipped, nTotalWritten = 0;
  int			nIndexInWords = 0, nSizeInBytes = 0;

  INITIALIZEPLOB;

  pClassInfo		= (LPCLASSINFO) FindClassInfo ( nElementTypeTag );
  ASSERT ( pClassInfo != NULL );
  nBitsPerElement	= pClassInfo->nFixSizeValue;
  ASSERT ( nBitsPerElement > 0 );
  nAlignment		=
    ( nSizeOfPostoreWord * nBitsPerByte ) / nBitsPerElement;
  if ( nAlignment > 0 && nIndex % nAlignment != 0 ) {
    char	szObject [ 256 ];
    fnClientObjectPrettyPrint ( oShortObjIdHeap, oShortObjId,
				nExpectingTypeTag,
				szObject, sizeof ( szObject ) );
    ERROR (( szInvalidAlignment, nIndex, szObject, nAlignment ));
    RETURN ( eshGeneralError );
  }

  if ( bGlobalDoCaching ) {
    pHeapObjectCache	= fnCacheGetObject ( oShortObjIdHeap, oShortObjId );
    if ( pHeapObjectCache != NULL ) {
      if ( nExpectingTypeTag != NULLTYPETAG &&
	   nExpectingTypeTag != pHeapObjectCache->pObjectCache->nTypeTag ) {
	ERROR (( szExpectedClass, nExpectingTypeTag,
		 pHeapObjectCache->pObjectCache->nTypeTag ));
	RETURN ( eshGeneralError );
      }
      nValues		= pHeapObjectCache->pObjectCache->nValues;
      nValuesClipped	= MIN ( nValues - nIndex, nSizeInElements );
      if ( nValuesClipped > 0 ) {
	if ( nIndex < 0 || nIndex >= nValues ) {
	  ERROR (( szIndexOverflow, nIndex, oShortObjId,
		   pHeapObjectCache->pObjectCache->nSlots ));
	  RETURN ( eshGeneralError );
	}
	nIndexInWords	= AlignBitsToWords ( nIndex * nBitsPerElement );
	if ( pHeapObjectCache->pObjectCache->oHeap == oShortObjIdHeap ) {
	  nTotalWritten	= nValuesClipped;
	  nSizeInBytes	=
	    AlignBitsToWords ( nTotalWritten * nBitsPerElement ) *
	    sizeof ( psint );
	  if ( memcmp ( & ( (psint *) pHeapObjectCache->pObjectCache->pValues )
			[ nIndexInWords ], pBuffer, nSizeInBytes ) != 0 ) {
	    pHeapObjectCache->pObjectCache->nChanges++;
	    memcpy ( & ( (psint *) pHeapObjectCache->pObjectCache->pValues )
		     [ nIndexInWords ], pBuffer, nSizeInBytes );
	  }
	} else {
	  nTotalWritten	= 
	    fnWriteValuesInBlocks ( oShortObjIdHeap, oShortObjId,
				    oExpectingClass, nExpectingTypeTag,
				    nIndex, nElementTypeTag,
				    nValuesClipped, pBuffer );
	  if ( nTotalWritten >= 0 ) {
	    /* The write placed implicit a vector lock onto oShortObjId: */
	    nSizeInBytes	=
	      AlignBitsToWords ( nTotalWritten * nBitsPerElement ) *
	      sizeof ( psint );
	    ASSERT ( ! boundp ( pHeapObjectCache->pObjectCache->oHeap ) );
	    pHeapObjectCache->pObjectCache->oHeap	= oShortObjIdHeap;
	    pHeapObjectCache->nVectorLockNow	= (SHLOCK)
	      ( (unsigned int) pHeapObjectCache->nVectorLockNow |
		(unsigned int) eshLockVectorWrite );
	    pHeapObjectCache->nVectorLockOld	= (SHLOCK)
	      ( (unsigned int) pHeapObjectCache->nVectorLockOld |
		(unsigned int) eshLockVectorWrite );
	    memcpy ( & ( (psint *) pHeapObjectCache->pObjectCache->pValues )
		     [ nIndexInWords ], pBuffer, nSizeInBytes );
	  }
	}
      }
      if ( nTotalWritten > 0 ) {
	pHeapObjectCache->nValuesWritten	+= nTotalWritten;
	fnCacheCompletedP ( oShortObjIdHeap, oShortObjId );
      }
      RETURN ( nTotalWritten );
    }
  }
  RETURN ( fnWriteValuesInBlocks ( oShortObjIdHeap, oShortObjId,
				   oExpectingClass, nExpectingTypeTag,
				   nIndex,
				   nElementTypeTag, nSizeInElements,
				   pBuffer ) );
} EndFunction ( fnClientObjectWriteValues );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
