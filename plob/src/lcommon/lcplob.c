/* -------------------------------------------------------------------------
| Module	lcplob.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/07/03
| Description	PLOB local source code.
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
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"lcplob.h"
#include	"lcplobintern.h"
#include	"lcplobmisc.h"
#include	"lcplobtype.h"
#include	"lcplobnumber.h"
#include	"lcplobsequ.h"
#include	"lcplobstruct.h"
#include	"lcplobclos.h"
#include	"lcploblock.h"
#include	"lcplobheap.h"
#include	"lcplobbtree.h"
#include	"lcplobroot.h"
#include	"lcplobadmin.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
#if WIN32
BOOL WINAPI DllMain	( HANDLE	hModule,
			  ULONG		dwReason,
			  LPVOID	lpReserved )
{
  static int	nThreads	= 0;
  BOOL		bDone		= TRUE;

  PROCEDURE	( DllMain );

  switch ( dwReason ) {
  case DLL_PROCESS_ATTACH:
    INITIALIZEPLOB;
    break;
  case DLL_THREAD_ATTACH:
    nThreads++;
    break;
  case DLL_THREAD_DETACH:
    nThreads--;
    if ( nThreads < 0 ) {
      fnDeinitializePlob ();
      fnDeinitializeCommon ();
    }
    break;
  case DLL_PROCESS_DETACH:
    fnDeinitializePlob ();
    fnDeinitializeCommon ();
    break;
  default:
    break;
  }

  RETURN ( bDone );
}
#endif

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectReadChars, "c-sh-read-chars",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInCharacters )
		  and
		  argument ( STRING ( nSizeInCharacters ),
			     vector_out, pszBuffer ) ) )
{
  FIXNUM	nRead	= 0;

  INITIALIZEPLOB;

  if ( nSizeInCharacters > 0 ) {
    FIXNUM	nWordsRead	=
      fnClientObjectReadValues ( oShortObjIdHeap, oShortObjId,
				 oExpectingClass, nExpectingTypeTag,
				 0, eshCharacterTag, nSizeInCharacters,
				 (void *) pszBuffer, 0, 0 );
    if ( nWordsRead >= 0 ) {
      nRead		= nWordsRead * nSizeOfPostoreWord;
      nRead		= MIN ( nRead, nSizeInCharacters );
      /* 2000-05-19 HK: Needed since LWL does not like not-null
         terminated ASCII strings: */
      pszBuffer [ nSizeInCharacters ]	= '\0';
    } else {
      nRead		= nWordsRead;
      pszBuffer [ 0 ]	= '\0';
    }
  }

  RETURN ( nRead );
} EndFunction ( fnClientObjectReadChars );

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnClientObjectWriteChars, "c-sh-write-chars",
		( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		  and
		  argument ( SHORTOBJID, value_in, oShortObjId )
		  and
		  argument ( SHORTOBJID, value_in, oExpectingClass )
		  and
		  argument ( SHTYPETAG, value_in, nExpectingTypeTag )
		  and
		  argument ( FIXNUM, value_in, nSizeInCharacters )
		  and
		  argument ( CONST_STRING, vector_in, pszBuffer ) ) )
{
  FIXNUM	nWritten	= 0;

  INITIALIZEPLOB;

  if ( nSizeInCharacters > 0 ) {
    FIXNUM	nWordsWritten	=
      fnClientObjectWriteValues ( oShortObjIdHeap, oShortObjId,
				  oExpectingClass, nExpectingTypeTag,
				  0, eshCharacterTag, nSizeInCharacters,
				  (void *) pszBuffer, 0, 0 );
    if ( nWordsWritten >= 0 ) {
      nWritten	= nWordsWritten * nSizeOfPostoreWord;
      nWritten	= MIN ( nWritten, nSizeInCharacters );
    } else {
      nWritten	= nWordsWritten;
    }
  }

  RETURN ( nWritten );
} EndFunction ( fnClientObjectWriteChars );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
