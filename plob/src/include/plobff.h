/* ---------------------------------------------------------------------------
| Module	plobff.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/04/14 Created
| Description	Foreign function interface to LISP
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
 -------------------------------------------------------------------------- */

#if defined(LISP)
;;;; -------------------------------------------------------------------------
;;;; For further comments look into file cplobff.h
;;;; -------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#if !defined(RPC)	/* client: */
DefineFunction ( voidResult,
		 fnRegisterCcallable, "register-c-callable-to-c",
		 ( argument ( CONST_STRING, vector_in, lpszFunctionName )
		   and
		   argument ( POINTER, value_in, lpfnFunctionCode ) ) );
#endif

#if defined(LISP) 

DefineFunction ( FIXNUM,
		 fnGetErrorMessage, "sh-get-error-message",
		 ( argument ( STRING ( nBuffer ), vector_out, pszBuffer )
		   and
		   argument ( FIXNUM, value_in, nBuffer ) ) );
DefineFunction ( FIXNUM,
		 fnGetErrorContinue, "sh-get-error-continue",
		 ( argument ( STRING ( nBuffer ), vector_out, pszBuffer )
		   and
		   argument ( FIXNUM, value_in, nBuffer ) ) );
DefineFunction ( FIXNUM,
		 fnGetErrorLevel, "sh-get-error-level",
		 ( voidArgument ) );

#endif /* LISP */

#if defined(ALLEGRO)

/* -------------------------------------------------------------------------
| Foreign-callable functions for Allegro Common LISP
 ------------------------------------------------------------------------- */
DefineCallable ( voidResult,
		 fnLISPerrorCallback, "sh-error-callback",
		 ( voidArgument ) );

#elif !defined (RPC)

/* -------------------------------------------------------------------------
| Foreign-callable functions for non-Allegro Common LISP
 ------------------------------------------------------------------------- */
DefineCallable ( voidResult,
		 fnLISPerrorCallback, "sh-error-callback",
		 ( argument ( ERRLVL, value_in, eLevel )
		   and
		   argument ( CONST_STRING, vector_in, pszContinue )
		   and
		   argument ( CONST_STRING, vector_in, pszErrorMsg ) ) );

#endif /* ALLEGRO */

#if defined(C2C)
void		fnInitCommonFfModule	( void );
void		fnInitializeFfModule	( void );
void		fnDeinitializeFfModule	( void );
void		fnDeinitCommonFfModule	( void );

void		fnLISPserrorCallback	( ERRLVL	eLevel,
					  LPCSTR	pszProcedure,
					  LPCSTR	pszErrorMsg );

#endif	/* C2C */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
