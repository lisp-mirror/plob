/* ----------------------------------------------------------------------------
| Module	plobff.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/04/14 Created
| Description	Foreign function interface to LISP
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
 --------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<string.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobff.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* #define LOGGING to show on stderr some messages what's happening:
   0 (no), 1 (module initialisation), 2 (register c callables) */
#if 0
#define	LOGGING	(0x01|0x02)
static FILE *	pStreamDebug;
#else
#define	LOGGING	0x00
#endif

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitCommonFfModule	( void )
{
  PROCEDURE	( fnInitCommonFfModule );

#if (LOGGING+0)
  pStreamDebug = fopen ( "/tmp/plobff.log", szStreamAppend );

#if (LOGGING+0) & 0x02
  /* 2001-02-13 HK: Debug: */
  if ( pStreamDebug != NULL ) {
    fprintf ( pStreamDebug, "%s:%s(%d): "
	      "Initialised module.\n",
	      __szFile__, __szProc__, __LINE__ );
    fflush ( pStreamDebug );
  }
#endif
#endif

  RETURN ( VOID );
} /* fnInitCommonFfModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitCommonFfModule	( void )
{
  PROCEDURE	( fnDeinitCommonFfModule );

#if (LOGGING+0)
  /* 2001-02-13 HK: Debug: */
  if ( pStreamDebug != NULL && pStreamDebug != stdout ) {
    fclose ( pStreamDebug );
    pStreamDebug	= NULL;
  }
#endif

  RETURN ( VOID );
} /* fnDeinitCommonFfModule */

/* -------------------------------------------------------------------------
| Registering of foreign-callable functions
| This function is called from Allegro LISP when a LISP function
| callable from C should be registered to the C layer:
 ------------------------------------------------------------------------- */
BeginFunction ( voidResult,
		fnRegisterCcallable, "register-c-callable-to-c",
		( argument ( CONST_STRING, vector_in, lpszFunctionName )
		  and
		  argument ( POINTER, value_in, lpfnFunctionCode ) ) )
{
  INITIALIZEPLOB;

#if (LOGGING+0) & 0x02
  /* 2001-02-13 HK: Debug: */
  if ( pStreamDebug != NULL ) {
    fprintf ( pStreamDebug, "%s:%s(%d): ",
	      __szFile__, __szProc__, __LINE__ );
    fflush ( pStreamDebug );
    fprintf ( pStreamDebug,
	      "Registering function '%s', address 0x%lX\n",
	      lpszFunctionName, lpfnFunctionCode );
    fflush ( pStreamDebug );
  }
#endif
  fnRegisterFunction ( __szFile__, __szProc__, __LINE__,
		       (LPFNVOID) lpfnFunctionCode, lpszFunctionName );

  RETURN ( VOID );
} EndFunction ( fnRegisterCcallable );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
