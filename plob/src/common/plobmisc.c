/* -------------------------------------------------------------------------
| Module	plobmisc.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB source code common for server and client.
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

#include	<sys/types.h>
#if	!WIN32
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>
#include	<netdb.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobintern.h"
#include	"plobmisc.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */
DLLEXPORTVAR const char	szFormatURL []	= "%s://%s/%s";

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitCommonMiscModule		( void )
{
  PROCEDURE	( fnInitCommonMiscModule );

  RETURN ( VOID );
} /* fnInitCommonMiscModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitCommonMiscModule	( void )
{
  PROCEDURE	( fnDeinitCommonMiscModule );

  RETURN ( VOID );
} /* fnDeinitCommonMiscModule */

/* ----------------------------------------------------------------------- */
unsigned int DLLEXPORT	fnIdentity		( unsigned int wObject )
{
  PROCEDURE	( fnIdentity );
  INITIALIZEPLOB;
  RETURN ( wObject );
} /* fnIdentity */

/* ----------------------------------------------------------------------- */
BeginFunction ( FIXNUM,
		fnShiftLeftAndSet, "c-short-float-to-fixnum",
		( argument ( FIXNUM, value_in, wObject )
		  and
		  argument ( FIXNUM, value_in, nShiftLeft )
		  and
		  argument ( FIXNUM, value_in, wOrMask ) ) )
{
  INITIALIZEPLOB;

  RETURN ( ( ( nShiftLeft >= 0 ) ?
	     ( wObject << nShiftLeft ) : ( wObject >> -nShiftLeft ) ) |
	   wOrMask );
} EndFunction ( fnShiftLeftAndSet );

/* ----------------------------------------------------------------------- */
unsigned int DLLEXPORT	fnUnmaskAndAdd		( unsigned int	wObject,
						  unsigned int	wUnmask,
						  int		nAdd )
{
  PROCEDURE	( fnUnmaskAndAdd );
  INITIALIZEPLOB;
  /* Since wObject might be a pointer, care for right arithmetic: */
  RETURN ( ( nAdd < 0 ) ?
	   ( ( wObject & ~wUnmask ) - ((unsigned int)-nAdd) ) :
	   ( ( wObject & ~wUnmask ) + nAdd ) );
} /* fnUnmaskAndAdd */

/* ----------------------------------------------------------------------------
| Split an URL of the form
|	[<transport>:][//<host>][/directory]
| into its components. If a component is not found, the argument string
| is left unchanged.
 --------------------------------------------------------------------------- */
int DLLEXPORT	fnSplitURL			( LPCSTR lpszURL,
						  LPSTR lpszHost,
						  LPSTR lpszTransport,
						  LPSTR lpszDirectory )
{
  int		i, nComponents	= 0;
  LPCSTR	lpszFrom, lpszI;

  PROCEDURE	( fnSplitURL );
  INITIALIZEPLOB;

  ASSERT ( lpszURL != (LPCSTR) NULL );

  /* Scan the transport string: */
  lpszFrom	= lpszURL;
  for ( i = 0, lpszI = lpszURL;
	*lpszI && *lpszI != ':' && *lpszI != '/';
	i++, lpszI++ );
  if ( *lpszI == ':' ) {
    nComponents++;
    if ( lpszTransport ) {
      strncpy ( lpszTransport, lpszFrom, i );
      lpszTransport [ i ]	= '\0';
    }
    lpszFrom	= & lpszFrom [ i ];
    lpszFrom++;
  }

  /* Scan the host string: */
  if ( lpszFrom [ 0 ] == '/' && lpszFrom [ 1 ] == '/' ) {
    nComponents++;
    lpszFrom	+= 2;
    for ( i = 0, lpszI = lpszFrom; *lpszI && *lpszI != '/'; i++, lpszI++ );
    if ( lpszHost ) {
      strncpy ( lpszHost, lpszFrom, i );
      lpszHost [ i ]	= '\0';
    }
    lpszFrom	= & lpszFrom [ i ];
    if ( *lpszFrom == '/' ) {
      lpszFrom++;
    }
  }

  /* `Scan' the directory string: */
  if ( *lpszFrom ) {
    nComponents++;
    if ( lpszDirectory ) {
      strcpy ( lpszDirectory, lpszFrom );
    }
  }

  RETURN ( nComponents );
} /* fnSplitURL */

/* ----------------------------------------------------------------------- */
LPSTR DLLEXPORT	fnMergeURLs			( LPCSTR lpszURL1,
						  LPCSTR lpszURL2,
						  LPSTR lpszMergedURL,
						  int nMergedURL )
{
  char		szProtocol [ MAX_URL ], szHost [ MAX_URL ],
    szDirectory [ MAX_URL ];
  char		szMergedURL [ MAX_URL ];
  LPCSTR	ppszURLs [ 2 ];
  int		i;

  PROCEDURE	( fnMergeURLs );
  INITIALIZEPLOB;

  szProtocol [ 0 ]	= '\0';
  szHost [ 0 ]		= '\0';
  szDirectory [ 0 ]	= '\0';
  szMergedURL [ 0 ]	= '\0';

  ppszURLs [ 0 ]	= lpszURL1;
  ppszURLs [ 1 ]	= lpszURL2;

  for ( i = 0; i < length ( ppszURLs ) &&
	  ( szProtocol [ 0 ] == '\0' ||
	    szHost [ 0 ] == '\0' ||
	    szDirectory [ 0 ] == '\0' );
	i++ ) {
    char		szTmpProtocol [ MAX_URL ], szTmpHost [ MAX_URL ],
      szTmpDirectory [ MAX_URL ];
    szTmpProtocol [ 0 ]		= '\0';
    szTmpHost [ 0 ]		= '\0';
    szTmpDirectory [ 0 ]	= '\0';
    fnSplitURL ( ppszURLs [ i ], szTmpHost, szTmpProtocol, szTmpDirectory );
    if ( szProtocol [ 0 ] == '\0' && szTmpProtocol [ 0 ] != '\0' ) {
      strncpy ( szProtocol, szTmpProtocol, sizeof ( szProtocol ) );
    }
    if ( szHost [ 0 ] == '\0' && szTmpHost [ 0 ] != '\0' ) {
      strncpy ( szHost, szTmpHost, sizeof ( szHost ) );
    }
    if ( szDirectory [ 0 ] == '\0' && szTmpDirectory [ 0 ] != '\0' ) {
      strncpy ( szDirectory, szTmpDirectory, sizeof ( szDirectory ) );
    }
  }

  i	= 0;
  if ( szProtocol [ 0 ] != '\0' ) {
    strcpy ( & szMergedURL [ i ], szProtocol );
    i	+= strlen ( & szMergedURL [ i ] );
    szMergedURL [ i++ ]	= ':';
    szMergedURL [ i ]	= '\0';
  }

  if ( szHost [ 0 ] != '\0' ) {
    szMergedURL [ i++ ]	= '/';
    szMergedURL [ i++ ]	= '/';
    strcpy ( & szMergedURL [ i ], szHost );
    i	+= strlen ( & szMergedURL [ i ] );
  }

  if ( szDirectory [ 0 ] != '\0' ) {
    szMergedURL [ i++ ]	= '/';
    strcpy ( & szMergedURL [ i ], szDirectory );
    i	+= strlen ( & szMergedURL [ i ] );
  }

  if ( lpszMergedURL != NULL ) {
    strncpy ( lpszMergedURL, szMergedURL, nMergedURL );
  }

  RETURN ( lpszMergedURL );
} /* fnMergeURLs */

/* ----------------------------------------------------------------------- */
BeginFunction ( BOOL,
		fnGetHostAddr, "c-sh-get-host-addr",
		( argument ( CONST_STRING, vector_in, szHost )
		  and
		  argument ( VECTOR ( int, 4 ),
			     vector_out, pnAddr ) ) )
{
  static const char	szDashes []	=
    "----------------------------------------------------------------";
  static const char	szIgnore []	= "Ignore them.";
  static const char	szOutOfRange []	=
    "Number out of range:\n"
    "       %s\n"
    "    %*.*s^";
  static const char	szClip []	= "Clip number to 0 resp. 255.";

  BOOL			bDone = FALSE;
  int			nInternetAddr [ 4 ], nNumbers = 0, nRead [ 4 ], i, j;
  struct hostent	* pHost = (struct hostent *) NULL;
  struct in_addr	InAddr;

  INITIALIZEPLOB;

  memset ( nRead, 0, sizeof ( nRead ) );
  memset ( nInternetAddr, 0, sizeof ( nInternetAddr ) );
  nNumbers	= sscanf ( szHost, "%d%n%*c%d%n%*c%d%n%*c%d%n",
			   & nInternetAddr [ 0 ], & nRead [ 0 ],
			   & nInternetAddr [ 1 ], & nRead [ 1 ],
			   & nInternetAddr [ 2 ], & nRead [ 2 ],
			   & nInternetAddr [ 3 ], & nRead [ 3 ] );
  
  bDone	= (BOOL)
    ( ( nNumbers > 0 ) && ( nNumbers <= length ( nInternetAddr ) ) &&
      ( szHost [ nRead [ nNumbers - 1 ] ] == '\0' ) );

  if ( bDone ) {
    for ( i = 0, j = 0; i < nNumbers; j = nRead [ i++ ] + 1 ) {
      if ( nInternetAddr [ i ] < 0 || nInternetAddr [ i ] > 255 ) {
	CERROR (( szClip, szOutOfRange,
		  szHost, j + 3, j + 3, szDashes ));
	if ( nInternetAddr [ i ] < 0 ) {
	  nInternetAddr [ i ]	= 0;
	} else if ( nInternetAddr [ i ] > 255 ) {
	  nInternetAddr [ i ]	= 255;
	}
      }
    }
  }

  if ( ! bDone ) {
    /* Next try: Check if a wild card '*' is found in the host name: */
    bDone	= (BOOL) ( strchr ( szHost, '*' ) != NULL );
    if ( bDone ) {
      nNumbers	= 0;
      i		= 0;
      j		= 0;
      while ( TRUE ) {
	if ( szHost [ i ] == '.' || szHost [ i ] == '\0' ) {
	  if ( nNumbers >= length ( nInternetAddr ) ) {
	    CERROR (( "Ignore them",
		      "Found more than 4 components in wildcard name:\n"
		      "       %s\n"
		      "    %*.*s^",
		      szHost, j + 2, j + 2, szDashes ));
	    break;
	  }
	  if ( szHost [ j ] == '*' ) {
	    /* Last position is a '*' wildcard: */
	    nInternetAddr [ nNumbers++ ]	= nMatchAny;
	    if ( j + 1 != i ) {
	      CERROR (( szIgnore,
			"Found unexpected characters behind wildcard '*':\n"
			"       %s\n"
			"    %*.*s^",
			szHost, j + 4, j + 4, szDashes ));
	    }
	  } else {
	    nRead [ 0 ]	= 0;
	    nRead [ 1 ]	= sscanf ( & szHost [ j ], "%d%n",
				   & nInternetAddr [ nNumbers ],
				   & nRead [ 0 ] );
	    if ( nRead [ 1 ] != 1 ) {
	      CERROR (( "Use a match-any for the field.",
			"Expected number:\n"
			"       %s\n"
			"    %*.*s^",
			szHost, j + 3, j + 3, szDashes ));
	      nInternetAddr [ nNumbers ]	= nMatchAny;
	    } else if ( szHost [ j + nRead [ 0 ] ] != '.' &&
			szHost [ j + nRead [ 0 ] ] != '\0' ) {
	      CERROR (( szIgnore,
			"Found unexpected characters behind number:\n"
			"       %s\n"
			"    %*.*s^",
			szHost, j + nRead [ 0 ] + 3, j + nRead [ 0 ] + 3,
			szDashes ));
	    } else if ( nInternetAddr [ nNumbers ] < 0 ||
			nInternetAddr [ nNumbers ] > 255 ) {
	      CERROR (( szClip, szOutOfRange,
			szHost, j + 3, j + 3, szDashes ));
	      if ( nInternetAddr [ nNumbers ] < 0 ) {
		nInternetAddr [ nNumbers ]	= 0;
	      } else if ( nInternetAddr [ nNumbers ] > 255 ) {
		nInternetAddr [ nNumbers ]	= 255;
	      }
	    }
	    nNumbers++;
	  }
	  j	= i + 1;
	}
	if ( szHost [ i ] == '\0' ) {
	  break;
	} else {
	  i++;
	}
      }
    }
  }

  if ( ! bDone ) {
    /* Next try: Look if a host with the passed name can be located: */
    pHost	= gethostbyname ( szHost );
    bDone	= (BOOL) ( pHost != NULL && pHost->h_addr_list != NULL );
    if ( bDone ) {
      nNumbers			= 4;
      memcpy ( &InAddr.s_addr, *(pHost->h_addr_list),
	       sizeof ( InAddr.s_addr ) );
      nInternetAddr [ 0 ]	= ( InAddr.s_addr & 0xFF000000 ) >> 24;
      nInternetAddr [ 1 ]	= ( InAddr.s_addr & 0xFF0000 ) >> 16;
      nInternetAddr [ 2 ]	= ( InAddr.s_addr & 0xFF00 ) >> 8;
      nInternetAddr [ 3 ]	= ( InAddr.s_addr & 0xFF );
    } else {
      CERROR (( "Return with failure.", "Couldn't locate host %s", szHost ));
    }
  }

  if ( bDone && pnAddr != NULL ) {
    for ( i = nNumbers; i < 4; i++ ) {
      nInternetAddr [ i ]	= nMatchAny;
    }
    memcpy ( pnAddr, nInternetAddr, sizeof ( int [ 4 ] ) );
  }

  RETURN ( bDone );
} EndFunction ( fnGetHostAddr );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
