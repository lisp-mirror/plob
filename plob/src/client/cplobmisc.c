/* -------------------------------------------------------------------------
| Module	cplobmisc.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB client functions
|
| Copyright	PLOB! Copyright 1994--2006 Heiko Kirschke.
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
| $Id$
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
#include	"hash.h"
#include	"postore.h"
#include	"cplob.h"
#include	"cplobff.h"
#include	"cplobintern.h"
#include	"cplobnumber.h"
#include	"cplobmisc.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Global variables
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Error handling and LISP callbacks
 ------------------------------------------------------------------------- */
static int	fnErrorHandler			( ERRORLEVEL	eLevel,
						  LPCSTR	lpszContinue,
						  LPCSTR	lpszErrorMsg );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitializeMiscModule		( void )
{
  PROCEDURE	( fnInitializeMiscModule );

  fnGlobalSetErrorHandler ( fnErrorHandler );

  /* 1996/10/22 HK: Debug: */
#if 0
  bGlobalDoCaching	= FALSE;
#endif

  RETURN ( VOID );
} /* fnInitializeMiscModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitializeMiscModule	( void )
{
  PROCEDURE	( fnDeinitializeMiscModule );

  fflush ( stdin );
  fnGlobalSetErrorHandler ( (LPFNERROR) NULL );

  RETURN ( VOID );
} /* fnDeinitializeMiscModule */

/* ----------------------------------------------------------------------- */
static int	fnErrorHandler			( ERRORLEVEL	eLevel,
						  LPCSTR	lpszContinue,
						  LPCSTR	lpszErrorMsg )
{
  PROCEDURE	( fnErrorHandler );

  fnLISPerrorCallback ( eLevel, lpszContinue, lpszErrorMsg );

  RETURN ( 0 );
} /* fnErrorHandler */

/* ----------------------------------------------------------------------- */
void		fnPLOBerrorCallback		( LPCSTR lpszErrorMsg )
{
  PROCEDURE	( fnPLOBerrorCallback );

  _ERROR ( "libpostore.a", NULL, -1, ( lpszErrorMsg ) );

  RETURN ( VOID );
} /* fnPLOBerrorCallback */

/* ----------------------------------------------------------------------- */
void		fnPLOBsaveCallback		( void )
{
  PROCEDURE	( fnPLOBsaveCallback );

  /* 1996/10/02 HK: The callbacks are never called from the client:
  if ( nGlobalCallCallbacks )
    fnLISPsaveCallback ();
  */

  RETURN ( VOID );
} /* fnPLOBsaveCallback */

/* ----------------------------------------------------------------------- */
void		fnPLOBrestoreCallback		( void )
{
  PROCEDURE	( fnPLOBrestoreCallback );

  /* 1996/10/02 HK: The callbacks are never called from the client:
  if ( nGlobalCallCallbacks ) {
    fnLISPrestoreCallback ();
  }
  */

  RETURN ( VOID );
} /* fnPLOBrestoreCallback */

/* ----------------------------------------------------------------------- */
void		fnPLOBstabiliseCallback		( void )
{
  PROCEDURE	( fnPLOBstabiliseCallback );

  /* 1996/10/02 HK: The callbacks are never called from the client:
  fnLISPstabiliseCallback ();
  */

  RETURN ( VOID );
} /* fnPLOBstabiliseCallback */

/* ----------------------------------------------------------------------- */
BOOL		fnPLOBsuspendCallback		( OBJID oLockBy,
						  OBJID oToLock,
						  LPCSTR lpszReason )
{
  PROCEDURE	( fnPLOBsuspendCallback );

  /* 1996/10/02 HK: The callbacks are never called from the client: */
  /*
  RETURN ( fnLISPsuspendCallback ( LONG2SHORTOBJID ( oLockBy ),
				   LONG2SHORTOBJID ( oToLock ),
				   lpszReason ) );
  */
  RETURN ( FALSE );
} /* fnPLOBsuspendCallback */

/* ----------------------------------------------------------------------- */
BOOL		fnPLOBwakeupCallback		( OBJID oLockBy,
						  OBJID oToLock,
						  LPCSTR lpszReason )
{
  PROCEDURE	( fnPLOBwakeupCallback );

  /* 1996/10/02 HK: The callbacks are never called from the client: */
  /*
  RETURN ( fnLISPwakeupCallback ( LONG2SHORTOBJID ( oLockBy ),
				  LONG2SHORTOBJID ( oToLock ),
				  lpszReason ) );
  */
  RETURN ( FALSE );
} /* fnPLOBwakeupCallback */

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectPeekSlots, "c-sh-peek-object-slots",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( FIXNUM, value_in, hPeek )
		  and
		  argument ( FIXNUM, value_in, nWords )
		  and
		  argument ( VECTOR ( u_int, nWords ),
			     vector_out, pBuffer ) ) )
{
  INITIALIZEPLOB;

  RETURN ( fnServerObjectPeekSlots ( oShortObjIdHeap, hPeek,
				     nWords, pBuffer ) );
} EndFunction ( fnClientObjectPeekSlots );

/* ----------------------------------------------------------------------- */
BeginFunction ( SHLOCK,
		fnClientObjectPeekValues, "c-sh-peek-object-values",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( FIXNUM, value_in, hPeek )
		   and
		   argument ( FIXNUM, value_in, nObjects )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_out, pnElementTypeTags )
		   and
		   argument ( VECTOR ( u_int, nObjects ),
			      vector_out, pnSizesInElements )
		   and
		   argument ( VECTOR ( void,
				       fnTypeTagSizeValue
				       ( nObjects, pnElementTypeTags,
					 pnSizesInElements ) ),
			      vector_out, pBuffer ) ) );
{
  

  INITIALIZEPLOB;

  RETURN ( fnServerObjectPeekValues ( oShortObjIdHeap, hPeek,
				      nObjects,
				      pnElementTypeTags, pnSizesInElements,
				      pnElementTypeTags, pnSizesInElements,
				      pBuffer ) );
} EndFunction ( fnClientObjectPeekValues );

/* ----------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnClientObjectPoke, "c-sh-poke-object",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( FIXNUM, value_in, nSlots )
		  and
		  argument ( VECTOR ( u_int, nSlots ), vector_in, pSlots )
		  and
		  argument ( SHTYPETAG, value_in, nElementTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInElements )
		  and
		  argument ( VECTOR ( void,
				      fnTypeTagSizeValue(1,&nElementTypeTag,
							 &nSizeInElements ) ),
			     vector_in, pValues ) ) )
{
  INITIALIZEPLOB;

  fnServerObjectPoke ( oShortObjIdHeap, oShortObjId,
		       nSlots, pSlots,
		       nElementTypeTag, nSizeInElements, pValues );
  RETURN ( VOID );
} EndFunction ( fnClientObjectPoke );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
