/* -------------------------------------------------------------------------
| Module	plobroot.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1996/09/23
| Description	PLOB source code common for server and client.
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
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobintern.h"
#include	"plobmisc.h"
#include	"plobtype.h"
#include	"plobroot.h"
#include	"plobbtree.h"
#include	"plobstruct.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void			fnInitCommonRootModule		( void )
{
  static CLASSINFO	ClassInfoTable []	= {
    { (SHTYPETAG) eshRootTag,
        SZPLOB,
        "root",
        (sizeof(PLOBROOT)-sizeof(PLOBHEADER))*nBitsPerByte,
        0,
        (LPCSTR) NULL,
        typeNotCachableP },
    { (SHTYPETAG) eshUserTag,
        SZPLOB,
        "user",
        (sizeof(PLOBUSER)-sizeof(PLOBHEADER))*nBitsPerByte,
        0,
        (LPCSTR) NULL,
        typeNotCachableP },
    { (SHTYPETAG) eshMachTag,
        SZPLOB,
        "machine",
        (sizeof(PLOBMACH)-sizeof(PLOBHEADER))*nBitsPerByte,
        0,
        (LPCSTR) NULL,
        typeNotCachableP },
  };

  int		i;

  PROCEDURE	( fnInitCommonRootModule );

  /* Register classes: */
  for ( i = 0; i < length ( ClassInfoTable ); i++ ) {
    RegisterPlobClass ( & ClassInfoTable [ i ] );
  }

  RETURN ( VOID );
} /* fnInitCommonRootModule */

/* ----------------------------------------------------------------------- */
void			fnDeinitCommonRootModule	( void )
{
  PROCEDURE	( fnDeinitCommonRootModule );

  RETURN ( VOID );
} /* fnDeinitCommonRootModule */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetVersionString ( int	nVersion,
					     LPSTR	pszVersion,
					     size_t	nSizeOfVersion )
{
  char	szBuffer [ 16 ];

  PROCEDURE	( fnGetVersionString );
  INITIALIZEPLOB;
  ASSERT ( pszVersion != NULL );

  sprintf ( szBuffer, "%d.%02.2d", nVersion / 100,  nVersion % 100 );
  strncpy ( pszVersion, szBuffer, nSizeOfVersion );

  RETURN ( pszVersion );
} /* fnGetVersionString */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetVersionDateString	( void )
{
  static char	szDate [ 32 ];

  PROCEDURE	( fnGetVersionDateString );
  INITIALIZEPLOB;

  if ( szDate [ 0 ] == '\0' ) {
    sprintf ( szDate, "%s %d, %d",
	      STRINGVERSIONMONTH, PLOBVERSIONDAY, PLOBVERSIONYEAR );
  }

  RETURN ( szDate );
} /* fnGetVersionDateString */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetCopyrightString	( void )
{
  static char		szCopyright [ 128 ]	= "";
  time_t		Time;

  PROCEDURE	( fnGetCopyrightString );
  INITIALIZEPLOB;

  if ( szCopyright [ 0 ] == '\0' ) {
    time ( &Time );
    strftime ( szCopyright, sizeof ( szCopyright ),
	       "(C) 1994-%Y " STRINGAUTHOR " ", localtime ( &Time ) );
    strcat ( szCopyright, fnGetEmailString () );
  }

  RETURN ( szCopyright );
} /* fnGetCopyrightString */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetEmailString	( void )
{
  PROCEDURE	( fnGetEmailString );
  INITIALIZEPLOB;

  RETURN ( STRINGEMAIL );
} /* fnGetEmailString */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetCompileDateString	( void )
{
  PROCEDURE	( fnGetCompileDateString );
  INITIALIZEPLOB;

  RETURN ( __DATE__ );
} /* fnGetCompileDateString */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetCompileTimeString	( void )
{
  PROCEDURE	( fnGetCompileTimeString );
  INITIALIZEPLOB;

  RETURN ( __TIME__ );
} /* fnGetCompileTimeString */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnGetCompileOpsysString	( void )
{
  PROCEDURE	( fnGetCompileOpsysString );
  INITIALIZEPLOB;

  RETURN ( OPSYS );
} /* fnGetCompileOpsysString */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
