/* -------------------------------------------------------------------------
| Module	splobclos.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		9.3.94
| Description	
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
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
#include	"splobstruct.h"
#include	"splobclos.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
const char		szInstance []			= "instance";

OBJID			oGlobalClassDescr		= NULLOBJID;
OBJID			oGlobalClassSlotDescr		= NULLOBJID;
OBJID			oGlobalClassDirSlotDescr	= NULLOBJID;
OBJID			oGlobalClassEffSlotDescr	= NULLOBJID;
OBJID			oGlobalMethodDescr		= NULLOBJID;

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */
#define			LocateClassDescr( oSelf )	\
((!boundp(oGlobalClassDescr)||				\
  !boundp(oGlobalClassDirSlotDescr)||			\
  !boundp(oGlobalClassEffSlotDescr)||			\
  !boundp(oGlobalMethodDescr))?				\
 (fnLocateClassDescr(oSelf),TRUE):			\
 FALSE)
static OBJID		fnLocateDescrVector	( OBJID oClassDescr,
						  int nIndex );

static LPOBJID		fnInstanceVector	( OBJID oSelf );

static LPSTR	mfnPrintInstance( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitializeCLOSModule		( void )
{
  PROCEDURE	( fnInitializeCLOSModule );

  RegisterMethod ( eshInstanceTag, gfnInitializeInstance,
		   mfnInitStandard );
  RegisterMethod ( eshInstanceTag, gfnPrintObjectDetails,
		   mfnPrintInstance );
  RETURN ( VOID );
} /* fnInitializeCLOSModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitializeCLOSModule	( void )
{
  PROCEDURE	( fnDeinitializeCLOSModule );

  RETURN ( VOID );
} /* fnDeinitializeCLOSModule */

/* -------------------------------------------------------------------------
| Object print methods
 ------------------------------------------------------------------------- */

static LPOBJID		fnInstanceVector	( OBJID oSelf )
{
  LPOBJID	lpInstance, lpI;
  OBJID		oI;

  PROCEDURE	( fnInstanceVector );

  lpInstance	= (LPOBJID) NULL;
  if ( ObjId_is_valid ( oSelf ) ) {
    lpI	= SH_key_to_address ( oSelf );
    if ( OBJID2TYPETAG ( lpI [ eshSHvectorIdxTypeTag ] ) == eshInstanceTag ) {
      oI	= lpI [ Cooked2RawIndex ( eshInstIdxDataVector ) ];
      if ( vectorp ( oI ) ) {
	lpInstance	= SH_key_to_address ( oI );
	lpInstance	+= eshVectorObjIdSize;
      }
    }
  }
  RETURN ( lpInstance );
} /* fnInstanceVector */

/* ----------------------------------------------------------------------- */
static OBJID		fnLocateDescrVector	( OBJID oClassDescr,
						  int nIndex )
{
  OBJID		oI;
  LPOBJID	lpI;

  PROCEDURE	( fnLocateDescrVector );

  /* Look for the class slot description vector; if found,
     then look for the class slot description object: */
  lpI	= fnInstanceVector ( oClassDescr );
  oI	= ( lpI != NULL ) ? lpI [ nIndex ] : NULLOBJID;
  
  if ( vectorp ( oI ) ) {
    oI	= vector_svref ( oI, 0 );
    if ( instancep ( oI ) ) {
      lpI	= SH_key_to_address ( oI );
      oI	= lpI [ Cooked2RawIndex ( eshInstIdxClassWrapper ) ];
      if ( instancep ( oI ) ) {
	lpI	= SH_key_to_address ( oI );
	if ( lpI [ Cooked2RawIndex ( eshInstIdxClassWrapper ) ] ==
	     oClassDescr ) {
	  RETURN ( oI );
	}
      }
    }
  }

  RETURN ( NULLOBJID );
} /* fnLocateDescrVector */

/* ----------------------------------------------------------------------- */
void			fnLocateClassDescr		( OBJID oSelf )
{
  int		i;
  OBJID		oI;
  LPOBJID	lpI;

  PROCEDURE	( fnLocateClassDescr );

  if ( ! instancep ( oSelf ) )
    RETURN ( VOID );

  /* Search for the class description object: Dereference the
     class-wrapper field until it points onto itself; this
     object is the class-description: */
  if ( ! boundp ( oGlobalClassDescr ) ) {
    for ( i = 0, oI = oSelf, lpI = SH_key_to_address ( oSelf ); i < 4; i++ ) {
      if ( lpI [ Cooked2RawIndex ( eshInstIdxClassWrapper ) ] == oI ) {
	/* Found the class description: */
	oGlobalClassDescr	= oI;
	break;
      }
      oI	= lpI [ Cooked2RawIndex ( eshInstIdxClassWrapper ) ];
      if ( OBJID2TYPETAG ( lpI [ eshSHvectorIdxTypeTag ] ) ==
	   eshInstanceTag && ObjId_is_valid ( oI ) ) {
	lpI	= SH_key_to_address ( oI );
      } else {
	break;
      }
    }
  }

  if ( ! boundp ( oGlobalClassDirSlotDescr ) ) {
    oGlobalClassDirSlotDescr	=
      fnLocateDescrVector ( oGlobalClassDescr,
			    Cooked2RawIndex ( eshClassDescrIdxDirSlots ) );
  }
  if ( ! boundp ( oGlobalClassEffSlotDescr ) ) {
    oGlobalClassEffSlotDescr	=
      fnLocateDescrVector ( oGlobalClassDescr,
			    Cooked2RawIndex ( eshClassDescrIdxEffSlots ) );
  }
  if ( ! boundp ( oGlobalMethodDescr ) ) {
    oGlobalMethodDescr	=
      fnLocateDescrVector ( oGlobalClassDescr,
			    Cooked2RawIndex ( eshClassDescrIdxDirMethods ) );
  }
  RETURN ( VOID );
} /* fnLocateClassDescr */

/* ----------------------------------------------------------------------- */
static LPSTR	mfnPrintInstance( OBJID oObjId, LPOBJID lpSHvector,
				  LPCLASSINFO lpClassInfo,
				  LPSTR lpszBuffer, size_t nBuffer )
{
  /* Multi threading restriction: */
  static int	nEntered	= 0;

  int		i, l, n, nLocation, nSlotName;
  OBJID		oName, oDesc, oIndex;
  OBJID		oSlots, oSlot, oSlotName, oSlotValue;
  LPSTR		lpsName;
  LPOBJID	lpDesc, lpInstance, lpSlot, lpSlotInstanceVector;
  char		szBuffer [ 1024 ];
  LPSTR		lpszSlotName;

  PROCEDURE	( mfnPrintInstance );

  if ( nEntered > 0 ) {
    RETURN ( lpszBuffer );
  }
  nEntered++;

  LocateClassDescr ( oObjId );
  memset ( szBuffer, 0, sizeof ( szBuffer ) );
  oDesc		= lpSHvector [ Cooked2RawIndex ( eshInstIdxClassWrapper ) ];
  lpDesc	= fnInstanceVector ( oDesc );
  oName		= ( lpDesc != NULL ) ?
    lpDesc [ Cooked2RawIndex ( eshClassDescrIdxName ) ] : NULLOBJID;
  lpsName	= gfnNameOf ( oName, &n );
  if ( lpsName ) {
    strncpy ( szBuffer, lpsName, MIN ( sizeof ( szBuffer ) - 1, n ) );
    fnBarifyString ( szBuffer, sizeof ( szBuffer ) - 1 );
  }

  lpInstance	= fnInstanceVector ( oObjId );
  if ( lpInstance == NULL ) {
    strncpy ( lpszBuffer, szBuffer, nBuffer );
    nEntered--;
    RETURN ( lpszBuffer );
  }

  i		= 0;
  if ( boundp ( oGlobalClassDescr ) && oDesc == oGlobalClassDescr ) {
    /* oSelf is a class description object; add the class name,
       version number and number of slots: */
    fnAddName ( lpInstance [ Cooked2RawIndex ( eshClassDescrIdxName ) ],
	        szBuffer, sizeof ( szBuffer ), &i, TRUE );
    if ( fixnump ( lpInstance [ Cooked2RawIndex
			        ( eshClassDescrIdxVersion ) ] ) &&
	 i < sizeof ( szBuffer ) - 10 ) {
      if ( i > 0 )
	szBuffer [ i++ ]	= ' ';
      sprintf ( & szBuffer [ i ], szFormatVersNum,
	        OBJID2FIXNUM ( lpInstance
			       [ Cooked2RawIndex
				 ( eshClassDescrIdxVersion ) ] ) / 100,
	        OBJID2FIXNUM ( lpInstance
			       [ Cooked2RawIndex
				 ( eshClassDescrIdxVersion ) ] ) % 100 );
      i	+= strlen ( & szBuffer [ i ] );
    }
    if ( i < sizeof ( szBuffer ) - 22 &&
	 fixnump ( lpInstance [ Cooked2RawIndex
			        ( eshClassDescrIdxPNSlots ) ] ) &&
	 fixnump ( lpInstance [ Cooked2RawIndex
			        ( eshClassDescrIdxNSlots ) ] ) ) {
      if ( i > 0 )
	szBuffer [ i++ ]	= ' ';
      sprintf ( & szBuffer [ i ], szFormatSlots,
	        OBJID2FIXNUM ( lpInstance
			       [ Cooked2RawIndex
				 ( eshClassDescrIdxPNSlots ) ] ),
	        OBJID2FIXNUM ( lpInstance
			       [ Cooked2RawIndex
				 ( eshClassDescrIdxNSlots ) ] ) );
     }
  } else if ( ( boundp ( oGlobalClassSlotDescr ) &&
	        oDesc == oGlobalClassSlotDescr ) ||
	      ( boundp ( oGlobalClassDirSlotDescr ) &&
	        oDesc == oGlobalClassDirSlotDescr ) ||
	      ( boundp ( oGlobalClassEffSlotDescr ) &&
	        oDesc == oGlobalClassEffSlotDescr ) ) {
    /* oSelf is a slot description object; add the slot name
       and slot extent: */
    if ( fnAddName ( lpInstance [ Cooked2RawIndex
				  ( eshClassSlotDescrIdxName ) ],
		     szBuffer, sizeof ( szBuffer ), &i, TRUE ) &&
	 i < sizeof ( szBuffer ) )
      szBuffer [ i++ ]	= ' ';
    if ( i < sizeof ( szBuffer ) - 8 ) {
      strncpy ( & szBuffer [ i ], "extent=", nBuffer - i );
      i	+= strlen ( & szBuffer [ i ] );
      fnAddName ( lpInstance [ Cooked2RawIndex
			       ( eshClassSlotDescrIdxExtent ) ],
		  szBuffer, sizeof ( szBuffer ), &i, FALSE );
    }
  } else if ( boundp ( oGlobalMethodDescr ) &&
	      oDesc == oGlobalMethodDescr ) {
    /* oSelf is a method description object: */
    fnAddName ( lpInstance [ Cooked2RawIndex ( eshMethodIdxName ) ],
	        szBuffer, sizeof ( szBuffer ), &i, TRUE );
    fnAddName ( lpInstance [ Cooked2RawIndex ( eshMethodIdxQualifiers ) ],
	        szBuffer, sizeof ( szBuffer ), &i, TRUE );
  } else if ( lpDesc != NULL ) {
    if ( instancep ( lpDesc [ Cooked2RawIndex
			        ( eshClassDescrIdxNextGen ) ] ) ) {
      /* oSelf is a 'normal' instance; add the version number iff
	 the class description is not the most actual class description: */
      i	= strlen ( szBuffer );
      if ( i < sizeof ( szBuffer ) - 8 ) {
	if ( i > 0 ) {
	  szBuffer [ i++ ]	= ' ';
	}
	sprintf ( & szBuffer [ i ], szFormatVersNum,
		  OBJID2FIXNUM ( lpDesc
				 [ Cooked2RawIndex
				     ( eshClassDescrIdxVersion ) ] ) / 100,
		  OBJID2FIXNUM ( lpDesc
				 [ Cooked2RawIndex
				     ( eshClassDescrIdxVersion ) ] ) % 100 );
      }
    }
    /* Now scan through the effective slots and add each slot for
       which an index has been defined. */
    oSlots	= lpDesc [ Cooked2RawIndex ( eshClassDescrIdxEffSlots ) ];
    if ( vectorp ( oSlots ) ) {
      find_symbol ( &oGlobalSymNil,
		    find_package ( &oGlobalPkgCommonLisp, szCOMMONLISP ),
		    szNIL );
      l		= strlen ( szBuffer );
      n		= vector_length ( oSlots );
      for ( i = 0; i < n; i++, l < nBuffer ) {
	oSlot		= vector_svref ( oSlots, i );
	lpSlotInstanceVector	= fnInstanceVector ( oSlot );
	oIndex		= NULLOBJID;
	oSlotName	= NULLOBJID;
	if ( lpSlotInstanceVector != NULL ) {
	  lpSlot	= SH_key_to_address ( oSlot );
	  oDesc		=
	    lpSlot [ Cooked2RawIndex ( eshInstIdxClassWrapper ) ];
	  if ( ! boundp ( oGlobalClassEffSlotDescr ) ||
	       oDesc != oGlobalClassEffSlotDescr ) {
	    lpSlotInstanceVector	= (LPOBJID) NULL;
	  }
	}
	if ( lpSlotInstanceVector != NULL ) {
	  /* Check if the slot has an index: */
	  oIndex	=
	    lpSlotInstanceVector [ Cooked2RawIndex
				     ( eshClassSlotDescrIdxIndex ) ];
	  if ( boundp ( oIndex ) && oGlobalSymNil != NULLOBJID &&
	       oIndex != oGlobalSymNil && 
	       fixnump ( lpSlotInstanceVector
			 [ Cooked2RawIndex
			   ( eshClassEffSlotDescrIdxLocation ) ] ) ) {
	    /* Yes, it has; add the slot name and value: */
	    fnAddName ( lpSlotInstanceVector
			[ Cooked2RawIndex ( eshClassSlotDescrIdxName ) ],
			szBuffer, sizeof ( szBuffer ), &l, TRUE );
	    nLocation		=
		OBJID2FIXNUM ( lpSlotInstanceVector
			         [ Cooked2RawIndex
				     ( eshClassEffSlotDescrIdxLocation ) ] );
	    oSlotValue	= lpInstance [ Cooked2RawIndex ( nLocation ) ];
	    szBuffer [ l++ ]	= '=';
	    fnPrintObject ( oSlotValue, & szBuffer [ l ], nBuffer - l );
	    l	+= strlen ( & szBuffer [ l ] );
	  }
	}
      }
    }
  }

  strncpy ( lpszBuffer, szBuffer, nBuffer );
  if ( nBuffer > 0 )
    lpszBuffer [ nBuffer - 1 ]	= '\0';
  nEntered--;
  RETURN ( lpszBuffer );
} /* mfnPrintInstance */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnServerDbCreateInstances, "c-sh-create-instances",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in,
			     oShortObjIdClassDescr )
		  and
		  argument ( FIXNUM, value_in, nObjIds )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_out, pObjIds )
		  and
		  argument ( u_int, value_out, pnSlots )
		  and
		  argument ( VECTOR ( u_int, nObjIds ),
			     vector_out, pDatas ) ) )
{
  int		nCreated = 0;
  OBJID		oInstance, oData, oClassDescr, oClassDescrData, oDependent;
  OBJID		oSlots;
  int		nSlots;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( 0 );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( 0 );
    }
  }
  ASSERT ( StableHeap_is_open );

  oClassDescr	= Short2LongObjId ( oShortObjIdClassDescr );
  if ( ! instancep ( oClassDescr ) ) {
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oClassDescr, (LPSTR) NULL, 0 ),
	     szInstance, szInvalidTypeTag ));
    UnstoreSession ();
    RETURN ( 0 );
  }

  LocateClassDescr ( oClassDescr );
  if ( instance_wrapper ( oClassDescr ) != oGlobalClassDescr ) {
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oClassDescr, (LPSTR) NULL, 0 ),
	     szInstance, szNotSelfRef ));
    UnstoreSession ();
    RETURN ( 0 );
  }

  oClassDescrData	= instance_data ( oClassDescr );
  oSlots		=
    vector_svref ( oClassDescrData, eshClassDescrIdxPNSlots );
  if ( ! fixnump ( oSlots ) ) {
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oClassDescr, (LPSTR) NULL, 0 ),
	     szInstance, szNSlotsNoFixnum ));
    UnstoreSession ();
    RETURN ( 0 );
  }
  nSlots		= OBJID2FIXNUM ( oSlots );
  if ( pnSlots != NULL ) {
    *pnSlots	= nSlots;
  }
  oDependent		=
    vector_svref ( oClassDescrData, eshClassDescrIdxDependent );

  for ( /* nCreated = 0 */; nCreated < nObjIds; nCreated++ ) {
    oInstance				= make_instance ();
    ASSERT ( boundp ( oInstance ) );
    oData				= make_vector ( nSlots );
    makdependent ( oData, flagDependentReadWrite );
    instance_wrapper ( oInstance )	= oClassDescr;
    instance_data ( oInstance )		= oData;
    if ( boundp ( oDependent ) ) {
      fnMakeDependentFromSymbol ( oInstance, oDependent );
    }
    if ( pObjIds != NULL ) {
      pObjIds [ nCreated ]	= LONG2SHORTOBJID ( oInstance );
    }
    if ( pDatas != NULL ) {
      pDatas [ nCreated ]	= LONG2SHORTOBJID ( oData );
    }
  }

  UnstoreSession ();
  RETURN ( nCreated );
} EndFunction ( fnServerDbCreateInstances );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerInstanceWriteWrapper,
		"c-sh-write-instance-wrapper",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdClassDescr ) ) )
{
  OBJID		oClassDescr;
  SHLOCK	nLockOld;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oClassDescr	= Short2LongObjId ( oShortObjIdClassDescr );

  /* Only class descriptions are allowed as instance wrapper type: */
  if ( ! instancep ( oClassDescr ) ) {
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oClassDescr, (LPSTR) NULL, 0 ) ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }
  LocateClassDescr ( oClassDescr );

  /* Only class descriptions are allowed as instance wrapper type: */
  if ( instance_wrapper ( oClassDescr ) != oGlobalClassDescr ) {
    ERROR (( szFormatErrorDescr,
	     fnPrintObject ( oClassDescr, (LPSTR) NULL, 0 ) ));
    UnstoreSession ();
    RETURN ( eshGeneralError );
  }

  nLockOld	=
    fnServerObjectWriteAtIndex ( oShortObjIdHeap, oShortObjIdInstance,
				 (OBJID) NULLOBJID, (SHTYPETAG) eshInstanceTag,
				 eshInstIdxClassWrapper, oShortObjIdClassDescr,
				 eshShortObjIdTag );

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerInstanceWriteWrapper );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnServerInstanceWriteData,
		"c-sh-write-instance-data",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjIdData ) ) )
{
  OBJID		oData;
  SHLOCK	nLockOld;

  INITIALIZEPLOB;
  if ( SuspendedP ) {
    RETURN ( eshGeneralError );
  }
  if ( StoreSession ( SHORT2LONGOBJID ( oShortObjIdHeap ) ) ) {
    if ( CATCHERROR ) {
      UNSTORESESSION ();
      RETURN ( eshGeneralError );
    }
  }
  ASSERT ( StableHeap_is_open );

  oData		= Short2LongObjId ( oShortObjIdData );
  /* Only vectors are allowed as instance data type: */
  ASSERT ( vectorp ( oData ) );

  nLockOld	=
    fnServerObjectWriteAtIndex ( oShortObjIdHeap, oShortObjIdInstance,
				 (OBJID) NULLOBJID, (SHTYPETAG) eshInstanceTag,
				 eshInstIdxDataVector, oShortObjIdData,
				 eshShortObjIdTag );
  if ( nLockOld >= 0 ) {
    makdependent ( oData, flagDependentReadWrite );
  }

  UnstoreSession ();
  RETURN ( nLockOld );
} EndFunction ( fnServerInstanceWriteData );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
