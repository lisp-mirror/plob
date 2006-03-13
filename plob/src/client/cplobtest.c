/* -------------------------------------------------------------------------
| Module	c-plob-test.c
| Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
|		mailto:Heiko.Kirschke@acm.org
| Date		11.11.93
| Description	PLOB test functions.
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

#if defined(BUILDDLL)
#undef BUILDDLL
#endif

#include	<ctype.h>
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

#if defined(NORPC)

#include	"splob.h"
#include	"splobintern.h"
#include	"splobmisc.h"
#include	"splobtype.h"
#include	"splobsequ.h"
#include	"sploblock.h"
#include	"splobheap.h"
#include	"splobbtree.h"
#include	"splobnumber.h"
#include	"splobroot.h"

#else

#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobsequ.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobnumber.h"
#include	"cplobroot.h"

#endif

#define		RPCNOTYPES
#include	"plobd.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
SHORTOBJID		oHeapDefault		= NULLOBJID;

/* ----------------------------------------------------------------------- */
static const char	szIgnore []		=
"Ignore error.";
static const char	szUnexpectedValue []	=
"Received unexpected return value.";

/* ----------------------------------------------------------------------- */
#if defined(NORPC)

RPCPORT	fnPlobdGetPort	( void )
{
  return (RPCPORT) 0;
} /* fnPlobdGetPort */

RPCPORT	fnPlobdSetPort	( RPCPort	lPortNew )
{
  return (RPCPORT) 0;
} /* fnPlobdSetPort */

RPCVERSION	fnPlobdGetVersion	( void )
{
  return (RPCVERSION) 0;
} /* fnPlobdGetVersion */

RPCVERSION	fnPlobdSetVersion	( RPCVERSION	lVersionNew )
{
  return (RPCVERSION) 0;
} /* fnPlobdSetVersion */
extern struct svc_req *	fnServerPlobdRequest	( void )
{
  return (struct svc_req *) NULL;
} /* fnServerPlobdRequest */

extern struct svc_req *	fnServerPlobdReply
	( void (*pfnCalled)(), void * pReturnValue )
{
  return (struct svc_req *) NULL;
} /* fnServerPlobdReply */

#endif

/* ----------------------------------------------------------------------- */
BOOL		fnTestCallback			( LPVOID lpUserData,
						  OBJID oBTree,
						  OBJID oKey,
						  OBJID oData,
						  OBJID oBTreePage,
						  int nIndex )
{
  fprintf ( stdout, "fnTestCallback: key = %s\n",
	    fnPrintObject ( oKey, NULL, 0 ) );
  fflush ( stdout );
  if ( lpUserData != NULL ) {
    fprintf ( stdout, "fnLISPmapCallback: Deleting key %s ...",
	      fnPrintObject ( oKey, NULL, 0 ) );
    fflush ( stdout );
    fnClientBtreeDelete ( (SHORTOBJID) lpUserData,
			  LONG2SHORTOBJID ( oBTree ),
			  ObjId2Fixnum ( oKey ), eshFixnumTag );
    fputs ( " done!\n", stdout );
    fflush ( stdout );
  }
  RETURN ( TRUE );
}

/* ----------------------------------------------------------------------- */
void fnTestString ()
{
  OBJID		oString, oLast;
  char		szAnswer [ 80 ], szData [ 80 ];
  LPSTR		lpszFound;
  BOOL		bContinue;

  bContinue	= TRUE;
  makunbound ( oString );
  makunbound ( oLast );
  srand ( time ( NULL ) );

  fputs ( "\nPersistent string testing.\n", stdout );

  while ( bContinue ) {
    fputs ( "\n(g)et [<objid>] (d)estroy [<objid>] (m)ake [<data>] (q)uit ",
	    stdout );
    fflush ( stdout );
    szAnswer [ 0 ]	= '\0';
    fgets ( szAnswer, sizeof ( szAnswer ), stdin );
    fputc ( '\n', stdout );
    fflush ( stdout );
    switch ( szAnswer [ 0 ] ) {

    case 'g': case 'G':
      if ( sscanf ( & szAnswer [ 1 ], "%d", &oString ) < 1 ) {
	oString	= oLast;
	fprintf ( stdout, "Reading last objid %d, object %s\n",
		  oString,
		  fnPrintObject ( SHORT2LONGOBJID ( oString ), NULL, 0 ) );
      }
      lpszFound	= string_ptr ( Short2LongObjId ( oString  ) );
      fprintf ( stdout, "Found string '%s'.\n",
	        ( lpszFound ) ? lpszFound : NULL );
      break;

    case 'd': case 'D':
      if ( sscanf ( & szAnswer [ 1 ], "%d", &oString ) < 1 ) {
	oString	= oLast;
	fprintf ( stdout, "Destroying last objid %d, object %s\n",
		  oString,
		  fnPrintObject ( SHORT2LONGOBJID ( oString ), NULL, 0 ) );
      }
      fnClientObjectDestroy ( NULLOBJID, oString );
      break;

    case 'm': case 'M':
      szData [ 0 ]	= '\0';
      if ( sscanf ( & szAnswer [ 1 ], "%s", szData ) < 1 ) {
	sprintf ( szData, "String %d", rand () % 1024 );
	fprintf ( stdout, "Making random string %s\n", szData );
      }
      oString	= make_string ( szData );
      oString	= Long2ShortObjId ( oString );
      if ( oString ) {
	fprintf ( stdout, "Created string %s\n",
		  fnPrintObject ( SHORT2LONGOBJID ( oString ), NULL, 0 ) );
	oLast	= oString;
      } else {
	fprintf ( stdout, "Create string failed." );
      }
      break;

    case '\0': case 'q': case 'Q':
      bContinue		= FALSE;
      break;
    }
  }

} /* fnTestString */

/* ----------------------------------------------------------------------- */
LPSTR fnPrintLockMask ( SHLOCK nLock )
{
  static char	szBuf [ 128 ];

  PROCEDURE ( fnPrintLockMask );

  szBuf [ 0 ]	= '\0';
  switch ( nLock ) {
  case eshLockDenied:
    strcpy ( szBuf, "#<lock denied>" );
    break;
  case eshUnknownLockMode:
    strcpy ( szBuf, "#<lock mode unknown>" );
    break;
  case eshGeneralError:
    strcpy ( szBuf, "#<lock general error>" );
    break;
  case eshUnlockFailed:
    strcpy ( szBuf, "#<unlock failed>" );
    break;
  case eshWriteFailed:
    strcpy ( szBuf, "#<write failed>" );
    break;
  case eshLockFailed:
    strcpy ( szBuf, "#<lock failed>" );
    break;
  default:
    if ( nLock & ( eshLockModeRead | eshLockModeWrite |
		   eshLockModeReadIntent | eshLockModeWriteIntent ) ) {
      if ( nLock & eshLockModeRead ) {
	strcpy ( szBuf, "read" );
      }
      if ( nLock & eshLockModeWrite ) {
	if ( szBuf [ 0 ] )
	  strcat ( szBuf, "|" );
	strcat ( szBuf, "write" );
      }
      if ( nLock & eshLockModeReadIntent ) {
	if ( szBuf [ 0 ] )
	  strcat ( szBuf, "|" );
	strcat ( szBuf, "read-intent" );
      }
      if ( nLock & eshLockModeWriteIntent ) {
	if ( szBuf [ 0 ] )
	  strcat ( szBuf, "|" );
	strcat ( szBuf, "write-intent" );
      }
    } else {
      strcpy ( szBuf, "<no lock>" );
    }
    break;
  }
  RETURN ( szBuf );
} /* fnPrintLockMask */

/* ----------------------------------------------------------------------- */
LPSTR	fnPrintVector	( oVector )
{
  static char	szBuffer [ 256 ];
  int		i, j, n;
  OBJID		oElement;

  n	= vector_length ( oVector );
  for ( i = 0, j = 0; i < n && j < sizeof ( szBuffer ); i++ ) {
    oElement	= vector_svref ( oVector, i );
    if ( boundp ( oElement ) ) {
      if ( j > 0 )
	szBuffer [ j++ ]	= ' ';
      fnPrintObject ( oElement, & szBuffer [ j ], sizeof ( szBuffer ) - j );
      j	+= strlen ( & szBuffer [ j ] );
    }
  }
  szBuffer [ j ]	= '\0';
  RETURN ( szBuffer );
} /* fnPrintVector */

/* ----------------------------------------------------------------------- */
void fnTestLocking ()
{
  enum {
    nTestVectorSize	= 10
  };
  static CONST char	szDelimiters []	= "\n\r ,";

  char		szTest [ 128 ], szAnswer [ 128 ];
  SHORTOBJID	oHeaps [ 3 ], oTest;
  OBJID		oUnbound;
  int		i, h, nIndex, nCurrentHeap	= 0;
  BOOL		bContinue, bGoOn;
  SHLOCK	nLockFirst, nLock, nLockOld;
  LPSTR		lpszLevel, lpszTok;

  PROCEDURE	( fnTestLocking );

  bContinue	= TRUE;
  makunbound ( oUnbound );

  oHeaps [ 0 ]	= oHeapDefault;

  for ( i = 1; i < length ( oHeaps ); i++ )
    oHeaps [ i ] = fnClientDbCreateObject ( NULLOBJID, eshHeapTag, 0,
					    NULLTYPETAG, 0 );
  oTest	= fnClientDbCreateObject ( NULLOBJID, eshVectorTag,
				   nTestVectorSize, NULLTYPETAG, 0 );
  for ( i = 0; i < 10; i++ )
    vector_svref ( Short2LongObjId ( oTest ), i )	= Fixnum2ObjId ( i );

  srand ( time ( NULL ) );

  fputs ( "\nLock testing.\n", stdout );
  fflush ( stdout );

  while ( bContinue ) {
    fprintf ( stdout,
	      "============================================================\n"
	      "\n"
	      "Actual heap %c = %s,\n"
	      "  test object = %s\n"
	      "  test vector = %s\n"
	      "\n"
	      "heap(a) heap(b) heap(c)\n"
	      "pu(r)ge root lock\n"
	      "\n"
	      "(s)tart- canc(e)l- en(d)-transaction heap[a] heap[b] heap[c]\n"
	      "\n"
	      "set test (v)ector [<value>]{,| <more values>}\n"
	      "\n"
	      "read-(o)nly read-(w)rite\n"
	      "\n"
	      "(i)nsert-lock [n]othing [(e)lement [<index>]]"
	      " [v]ector [s]tore\n"
	      "              read-[o]nly [r]ead"
	      " [w]rite [f]orce heap[a] heap[b] heap[c]\n"
	      "(l)ock [n]othing [(e)lement [<index>]]"
	      " [v]ector [s]tore\n"
	      "       [r]ead [w]rite [f]orce heap[a] heap[b] heap[c]\n"
	      "(u)nlock [n]othing [(e)lement [<index>]]"
	      " [v]ector [s]tore\n"
	      "         read-[o]nly [r]ead"
	      " [w]rite [f]orce heap[a] heap[b] heap[c]\n"
	      "u(n)lock-all [f]orce heap[a] heap[b] heap[c]\n"
	      "(g)et-lock [(e)lement [<index>]] [v]ector [s]tore"
	      " heap[a] heap[b] heap[c]\n"
	      "\n"
	      "(p)rint lock information [s]tore [v]ector [e]lement\n"
	      "\n"
	      "(q)uit ",
	      'a' + nCurrentHeap,
	      fnPrintObject ( Short2LongObjId ( oHeaps [ nCurrentHeap ] ),
			      NULL, 0 ),
	      fnPrintObject ( Short2LongObjId ( oTest ),
			      szTest, sizeof ( szTest ) ),
	      fnPrintVector ( Short2LongObjId ( oTest ) ) );

    fflush ( stdout );
    szAnswer [ 0 ]	= '\0';
    fgets ( szAnswer, sizeof ( szAnswer ), stdin );
    fputc ( '\n', stdout );
    fflush ( stdout );
    nLockFirst		= eshLockLevelElement | eshLockModeRead;
    lpszLevel		= "element";
    switch ( szAnswer [ 0 ] ) {

    case 'a': case 'A':
      nCurrentHeap	= 0;
      break;

    case 'b': case 'B':
      nCurrentHeap	= 1;
      break;

    case 'c': case 'C':
      nCurrentHeap	= 2;
      break;

    case 'r': case 'R':
      fnSetRootLock ( oUnbound );
      fnClientDbStabilise ( NULLOBJID );
      break;

    case 'o': case 'O':
      make_readonly ( Short2LongObjId ( oTest ) );
      break;

    case 'w': case 'W':
      make_readwrite ( Short2LongObjId ( oTest ) );
      break;

    case 's': case 'S':
      h			= nCurrentHeap;
      switch ( szAnswer [ 1 ] ) {
      case 'a': case 'A':
	h		= 0;
	break;
      case 'b': case 'B':
	h		= 1;
	break;
      case 'c': case 'C':
	h		= 2;
	break;
      }
      fprintf ( stdout,
	        "Starting transaction on heap %c %s\n",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ) );
      nLockOld	= fnClientTransactionBegin ( oHeaps [ h ], FALSE );
      fprintf ( stdout, "\nreturns %d\n", nLockOld );
      break;

    case 'e': case 'E':
      h			= nCurrentHeap;
      switch ( szAnswer [ 1 ] ) {
      case 'a': case 'A':
	h		= 0;
	break;
      case 'b': case 'B':
	h		= 1;
	break;
      case 'c': case 'C':
	h		= 2;
	break;
      }
      fprintf ( stdout,
	        "Cancel transaction on heap %c %s\n",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ) );
      nLockOld	= fnClientTransactionCancel ( oHeaps [ h ], FALSE );
      fprintf ( stdout, "\nreturns %d\n", nLockOld );
      break;

    case 'd': case 'D':
      h			= nCurrentHeap;
      switch ( szAnswer [ 1 ] ) {
      case 'a': case 'A':
	h		= 0;
	break;
      case 'b': case 'B':
	h		= 1;
	break;
      case 'c': case 'C':
	h		= 2;
	break;
      }
      fprintf ( stdout,
	        "End transaction on heap %c %s\n",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ) );
      nLockOld	= fnClientTransactionEnd ( oHeaps [ h ], FALSE );
      fprintf ( stdout, "\nreturns %d\n", nLockOld );
      break;

    case 'v': case 'V':
      h		= nCurrentHeap;
      for ( i = 1, bGoOn = TRUE; bGoOn && szAnswer [ i ]; i++ ) {
	switch ( szAnswer [ i ] ) {
	case 'a': case 'A':
	  h		= 0;
	  break;
	case 'b': case 'B':
	  h		= 1;
	  break;
	case 'c': case 'C':
	  h		= 2;
	  break;
	default:
	  bGoOn		= FALSE;
	  break;
	}
      }
      fprintf ( stdout,
	        "Notifying heap %c %s of object state change.\n",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ) );
      if ( ! gfnObjectStateChanged ( Short2LongObjId ( oHeaps [ h ] ),
				     Short2LongObjId ( oTest ) ) )
	break;
      for ( nIndex = 0; nIndex < nTestVectorSize; nIndex++ ) {
	makunbound ( vector_svref ( Short2LongObjId ( oTest ), nIndex ) );
      }
      lpszTok	= strtok ( & szAnswer [ i ], szDelimiters );
      nIndex	= 0;
      while ( lpszTok && nIndex < nTestVectorSize ) {
	i	= 0;
	if ( sscanf ( lpszTok, "%d", &i ) == 1 ) {
	  vector_svref ( Short2LongObjId ( oTest ), nIndex )	=
	    Fixnum2ObjId ( i );
	} else if ( ! lpszTok [ 1 ] ) {
	  vector_svref ( Short2LongObjId ( oTest ), nIndex )	=
	    Char2ObjId ( lpszTok [ 0 ] );
	} else {
	  vector_svref ( Short2LongObjId ( oTest ), nIndex )	=
	    make_string ( lpszTok );
	}
	lpszTok	= strtok ( NULL, szDelimiters );
	nIndex++;
      }
      break;

    case 'i': case 'I':
      h			= nCurrentHeap;
      nLock		= nLockFirst;
      nIndex		= 0;
      for ( i = 1; szAnswer [ i ]; i++ ) {
	switch ( szAnswer [ i ] ) {
	case 'n': case 'N':
	  nLock		&= ~eshLockModeMask;
	  break;
	case 'e': case 'E':
	  lpszLevel	= "element";
	  nLock		= ( nLock & ~eshLockLevelMask ) | eshLockLevelElement;
	  while ( isdigit ( szAnswer [ i + 1 ] ) ) {
	    nIndex	= 10 * nIndex + szAnswer [ ++i ] - '0';
	  }
	  break;
	case 'v': case 'V':
	  lpszLevel	= "vector";
	  nLock		= ( nLock & ~eshLockLevelMask ) | eshLockLevelVector;
	  break;
	case 's': case 'S':
	  lpszLevel	= "store";
	  nLock		= ( nLock & ~eshLockLevelMask ) | eshLockLevelStore;
	  break;
	case 'o': case 'O':
	  nLock		= ( nLock & ~eshLockModeMask ) | eshLockModeReadOnly;
	  break;
	case 'r': case 'R':
	  nLock		= ( nLock & ~eshLockModeMask ) | eshLockModeRead;
	  break;
	case 'w': case 'W':
	  nLock		= ( nLock & ~eshLockModeMask ) | eshLockModeWrite;
	  break;
	case 'f': case 'F':
	  nLock		|= eshLockForce;
	  break;
	case 'a': case 'A':
	  h		= 0;
	  break;
	case 'b': case 'B':
	  h		= 1;
	  break;
	case 'c': case 'C':
	  h		= 2;
	  break;
	}
      }
      fprintf ( stdout,
	        "Inserting lock with heap %c %s on level '%s',\n"
	        "mode '%s' into %s\n",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ),
	        lpszLevel,
	        fnPrintLockMask ( nLock ),
	        fnPrintObject ( Short2LongObjId ( oTest ), NULL, 0 ) );
      nLockOld	= fnClientTransactionLockInsert ( oHeaps [ h ], oHeaps [ h ],
						  nLock, oTest,
						  NULLTYPETAG, nIndex );
      fprintf ( stdout, "\nreturns %s\n",
	        fnPrintLockMask ( nLockOld ) );
      break;

    case 'u': case 'U':
      nLockFirst	|= eshUnlock;
    case 'l': case 'L':
      h			= nCurrentHeap;
      nLock		= nLockFirst;
      nIndex		= 0;
      for ( i = 1; szAnswer [ i ]; i++ ) {
	switch ( szAnswer [ i ] ) {
	case 'n': case 'N':
	  nLock		&= ~eshLockModeMask;
	  break;
	case 'e': case 'E':
	  lpszLevel	= "element";
	  nLock		= ( nLock & ~eshLockLevelMask ) | eshLockLevelElement;
	  while ( isdigit ( szAnswer [ i + 1 ] ) ) {
	    nIndex	= 10 * nIndex + szAnswer [ ++i ] - '0';
	  }
	  break;
	case 'v': case 'V':
	  lpszLevel	= "vector";
	  nLock		= ( nLock & ~eshLockLevelMask ) | eshLockLevelVector;
	  break;
	case 's': case 'S':
	  lpszLevel	= "store";
	  nLock		= ( nLock & ~eshLockLevelMask ) | eshLockLevelStore;
	  break;
	case 'o': case 'O':
	  nLock		= ( nLock & ~eshLockModeMask ) | eshLockModeReadOnly;
	  break;
	case 'r': case 'R':
	  nLock		= ( nLock & ~eshLockModeMask ) | eshLockModeRead;
	  break;
	case 'w': case 'W':
	  nLock		= ( nLock & ~eshLockModeMask ) | eshLockModeWrite;
	  break;
	case 'f': case 'F':
	  nLock		|= eshLockForce;
	  break;
	case 'a': case 'A':
	  h		= 0;
	  break;
	case 'b': case 'B':
	  h		= 1;
	  break;
	case 'c': case 'C':
	  h		= 2;
	  break;
	}
      }
      fprintf ( stdout,
	        "%s lock with heap %c %s on level '%s',\n"
	        "mode '%s' %s %s\n",
	        ( nLock & eshUnlock ) ? "Deleting" : "Setting",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ),
	        lpszLevel,
	        fnPrintLockMask ( nLock ),
	        ( nLock & eshUnlock ) ? "from" : "to",
	        fnPrintObject ( Short2LongObjId ( oTest ), NULL, 0 ) );
      nLockOld	= fnClientTransactionLockSet ( oHeaps [ h ], oHeaps [ h ],
				      nLock, oTest,
				      NULLTYPETAG, nIndex );
      fprintf ( stdout, "\nreturns %s\n",
	        fnPrintLockMask ( nLockOld ) );
      break;

    case 'g': case 'G':
      h			= nCurrentHeap;
      nLock		= nLockFirst & eshLockLevelMask;
      nIndex		= 0;
      for ( i = 1; szAnswer [ i ]; i++ ) {
	switch ( szAnswer [ i ] ) {
	case 'e': case 'E':
	  lpszLevel	= "element";
	  nLock		= eshLockLevelElement;
	  while ( isdigit ( szAnswer [ i + 1 ] ) ) {
	    nIndex	= 10 * nIndex + szAnswer [ ++i ] - '0';
	  }
	  break;
	case 'v': case 'V':
	  lpszLevel	= "vector";
	  nLock		= eshLockLevelVector;
	  break;
	case 's': case 'S':
	  lpszLevel	= "store";
	  nLock		= eshLockLevelStore;
	  break;
	case 'a': case 'A':
	  h	= 0;
	  break;
	case 'b': case 'B':
	  h	= 1;
	  break;
	case 'c': case 'C':
	  h	= 2;
	  break;
	}
      }
      fprintf ( stdout,
	        "Getting lock with heap %c %s on level '%s',\n"
	        "from %s\n",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ),
	        lpszLevel,
	        fnPrintObject ( Short2LongObjId ( oTest ), NULL, 0 ) );
      nLockOld	= fnClientTransactionLockGet ( oHeaps [ h ], oHeaps [ h ],
				      nLock, oTest,
				      NULLTYPETAG, nIndex );
      fprintf ( stdout, "\nreturns %s\n",
	        fnPrintLockMask ( nLockOld ) );
      break;

    case 'n': case 'N':
      h		= nCurrentHeap;
      for ( i = 1, bGoOn = TRUE; szAnswer [ i ] && bGoOn; i++ ) {
	switch ( szAnswer [ i ] ) {
	case 'a': case 'A':
	  h	= 0;
	  break;
	case 'b': case 'B':
	  h	= 1;
	  break;
	case 'c': case 'C':
	  h	= 2;
	  break;
        default:
	  bGoOn	= FALSE;
	  break;
	}
      }
      fprintf ( stdout,
	        "Unlocking all with heap %c %s from %s\n",
	        'a' + h,
	        fnPrintObject ( Short2LongObjId ( oHeaps [ h ] ),
			        szTest, sizeof ( szTest ) ),
	        fnPrintObject ( Short2LongObjId ( oTest ), NULL, 0 ) );
      nLockOld	= fnUnlockAll ( Short2LongObjId ( oHeaps [ h ] ),
			        Short2LongObjId ( oTest ) );
      fprintf ( stdout, "\nreturns %d\n", nLockOld );
      break;

    case 'p': case 'P':
      switch ( szAnswer [ 1 ] ) {
      case 'e': case 'E':
	fnPrintLock ( Short2LongObjId ( oTest ), eshLockLevelElement, stdout );
	break;
      case 's': case 'S':
	fnPrintLock ( Short2LongObjId ( oTest ), eshLockLevelStore, stdout );
	break;
      default:
	fnPrintLock ( Short2LongObjId ( oTest ), eshLockLevelVector, stdout );
	break;
      }
      break;

    case '\0': case 'q': case 'Q':
      bContinue		= FALSE;
      break;
    }
  }

  fnClientObjectDestroy ( NULLOBJID, oTest );
  for ( i = 1; i < length ( oHeaps ); i++ ) {
#if 0
    fnClientTransactionCancel ( oHeaps [ i ], TRUE );
#endif
    fnClientObjectDestroy ( NULLOBJID, oHeaps [ i ] );
  }
} /* fnTestLocking */

/* ----------------------------------------------------------------------- */
COMPARETAG fnString2CompareTag ( const char * pszFrom,
				 COMPARETAG eDefault,
				 BOOL bSwap )
{
  COMPARETAG eResult	= eDefault;

  if ( strcmp ( pszFrom, "<" ) == 0 ) {
    eResult	= ( bSwap ) ? eshGreater : eshLess;
  } else if ( strcmp ( pszFrom, "<=" ) == 0 ) {
    eResult	= ( bSwap ) ? eshGreaterEqual : eshLessEqual;
  } else if ( strcmp ( pszFrom, "==" ) == 0 ) {
    eResult	= eshEqual;
  } else if ( strcmp ( pszFrom, ">=" ) == 0 ) {
    eResult	= ( bSwap ) ? eshLessEqual : eshGreaterEqual;
  } else if ( strcmp ( pszFrom, ">" ) == 0 ) {
    eResult	= ( bSwap ) ? eshLess : eshGreater;
  }

  return eResult;
}
/* ----------------------------------------------------------------------- */
COMPARETAG fnCompareTagNegate ( COMPARETAG eFrom )
{
  switch ( eFrom ) {
  case eshEqual:
    return eshNotEqual;
  case eshEql:
    return eshNotEql;
  case eshEq:
    return eshNotEq;
  case eshNotEqual:
    return eshEqual;
  case eshNotEql:
    return eshEql;
  case eshNotEq:
    return eshEq;
  case eshLessEqual:
    return eshGreater;
  case eshLess:
    return eshGreaterEqual;
  case eshGreater:
    return eshLessEqual;
  case eshGreaterEqual:
    return eshLess;
  }
}

/* ----------------------------------------------------------------------- */
void fnTestBTree ()
{
  SHORTOBJID		oHeap, oBTree, oM, oMapper, oKey, oData;
  OBJID			oMin = TypeTag2ObjId ( eshMinTag );
  OBJID			oMax = TypeTag2ObjId ( eshMaxTag );
  char			szAnswer [ 80 ], szData [ 80 ], szFound [ 128 ];
  BOOL			bContinue, bDelete, bDescending = FALSE;
  FIXNUM		nKey, nKeyTo, nData;
  SHTYPETAG		nTypeTagData, nTypeTag, nTypeTagTo;
  int			i, nLast, nPrinted, nMapped;
  COMPARETAG		eCompareLower = eshGreaterEqual,
    eCompareUpper = eshLessEqual;
  SEEK			eOrigin;

  oHeap		= oHeapDefault;
  oBTree	= fnClientDbCreateObject ( NULLOBJID, eshBTreeTag, 0,
					   NULLTYPETAG, 0 );
  fnClientBtreePageSize ( oHeap, oBTree, 32 );
  oMapper	= fnClientDbCreateObject ( NULLOBJID, eshBTreeMapperTag,
					   0, NULLTYPETAG, 0 );
  fnClientTransactionBegin ( oHeap, FALSE );
  fnBTreeMapSearch ( SHORT2LONGOBJID ( oMapper ),
		     SHORT2LONGOBJID ( oHeap ),
		     SHORT2LONGOBJID ( oBTree ),
		     &oMin, eshMinTag, eshGreaterEqual,
		     &oMax, eshMaxTag, eshLessEqual,
		     FALSE );
  fnClientTransactionEnd ( oHeap, FALSE );

  bContinue	= TRUE;
  nLast		= 1;
  srand ( time ( NULL ) );

  fputs ( "\nPersistent BTree testing.\n", stdout );
  fflush ( stdout );

  while ( bContinue ) {
    fputs ( "\n(g)et [<key>] (c)lear (d)elete [<key>]"
	    " (i)nsert [<key> [<data>]]\n"
	    "m(a)ss-insert [<from-key> [<to-key> [<increment>]]]\n"
	    "(m[d])ap [<from-key> [<to-key>]]"
	    " (f)irst-next [<from-key> [<to-key>]]\n"
	    "c(o)mpare [<lower criterion> [<upper criterion>]]"
	    " d(e)scending [t|f]\n"
	    "page (s)size [<number>] seek (v) [<increment> [<origin>]]\n"
	    "(p)rint (q)uit ", stdout );
    fflush ( stdout );
    szAnswer [ 0 ]	= '\0';
    fgets ( szAnswer, sizeof ( szAnswer ), stdin );
    fputc ( '\n', stdout );
    fflush ( stdout );
    fnClientTransactionBegin ( oHeap, FALSE );
    switch ( szAnswer [ 0 ] ) {

    case 'a': case 'A':
      nKey	= 0;
      nKeyTo	= 1024;
      nMapped	= 1;
      sscanf ( & szAnswer [ 1 ], "%d%d%d", &nKey, &nKeyTo, &nMapped );
      fprintf ( stdout, "Inserting from key %d to key %d incr %d"
		" into btree %s\n",
		nKey, nKeyTo, nMapped,
		fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
      for ( i = nKey; i < nKeyTo; i += nMapped ) {
	sprintf ( szData, "data %d", i );
	oData = LONG2SHORTOBJID ( make_string ( szData ) );
	switch ( fnClientBtreeInsert ( oHeap, oBTree, i, eshFixnumTag,
				       oData, eshShortObjIdTag ) ) {
	case btreeInserted:
	case btreeUpdated:
	  break;
	default:
	  CERROR (( szIgnore, szUnexpectedValue ));
	}
      }
      break;

    case 'c': case 'C':
      switch ( fnClientBtreeClear ( oHeap, oBTree ) ) {
      case btreeNotFound:
	fprintf ( stdout, "Cleared empty btree %s.\n",
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      case btreeDeleted:
	fprintf ( stdout, "Cleared non-empty btree %s.\n",
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      default:
	CERROR (( szIgnore, szUnexpectedValue ));
      }
      break;

    case 'd': case 'D':
      nKey		= 0;
      if ( sscanf ( & szAnswer [ 1 ], "%d", &nKey ) < 1 ) {
	nKey	= nLast;
	fprintf ( stdout, "Deleting last key %d from btree %s\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
      }
      switch ( fnClientBtreeDelete ( oHeap, oBTree, nKey, eshFixnumTag ) ) {
      case btreeNotFound:
	fprintf ( stdout, "Key %d not found in btree %s.\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      case btreeDeleted:
	fprintf ( stdout, "Key %d found and deleted in btree %s.\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      default:
	CERROR (( szIgnore, szUnexpectedValue ));
      }
      break;

    case 'e': case 'E':
      szData [ 0 ]	= '\0';
      if ( sscanf ( & szAnswer [ 1 ], "%s", szData ) == 1 ) {
	switch ( szData [ 0 ] ) {
	case 'f': case 'F':
	  bDescending	= FALSE;
	  break;
	case 't': case 'T':
	  bDescending	= TRUE;
	  break;
	}
      }
      fprintf ( stdout, "%s order.\n",
		( bDescending ) ? "Descending" : "Ascending" );
      break;

    case 'f': case 'F':
      nKey		= 0;
      nTypeTag		= eshMinTag;
      nKeyTo		= 0;
      nTypeTagTo	= eshMaxTag;
      szData [ 0 ]	= '\0';
      if ( szAnswer [ 1 ] == 'd' || szAnswer [ 1 ] == 'D' ) {
	bDelete	= TRUE;
	i	= 2;
      } else {
	bDelete	= FALSE;
	i	= 1;
      }
      switch ( sscanf ( & szAnswer [ i ], "%d%d", &nKey, &nKeyTo ) ) {
      case -1:
      case 0:
	fprintf ( stdout, "Mapping all keys of btree %s\n",
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      case 1:
	fprintf ( stdout, "Mapping key %s %d into btree %s\n",
		  fnCompareTag2String ( eCompareLower, FALSE ), nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	nTypeTag	= eshFixnumTag;
	break;
      default:
	fprintf ( stdout, "Mapping %d %s key %s %d into btree %s\n",
		  nKey,
		  fnCompareTag2String ( eCompareLower, TRUE ),
		  fnCompareTag2String ( eCompareUpper, FALSE ), nKeyTo,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	nTypeTag	= eshFixnumTag;
	nTypeTagTo	= eshFixnumTag;
      }
      nPrinted	= 0;
      for ( nMapped = fnBTreeMapFirst ( &oM,
					SHORT2LONGOBJID ( oHeap ),
					SHORT2LONGOBJID ( oBTree ),
					&nKey, nTypeTag, eCompareLower,
					&nKeyTo, nTypeTagTo, eCompareUpper,
					bDescending, 1, &oKey, &oData );
	    nMapped == 1;
	    nMapped = fnBTreeMapNext ( oM, 1, &oKey, &oData ) ) {
	nPrinted++;
	fprintf ( stdout, "key = %s\n",
		  fnPrintObject ( oKey, NULL, 0 ) );
	if ( bDelete ) {
	  fnClientBtreeDelete ( oHeap, oBTree,
			    ObjId2Fixnum ( oKey ), eshFixnumTag );
	}
      }
      fprintf ( stdout, "Map function called %d times.\n", nPrinted );
      break;

    case 'g': case 'G':
      nKey		= 0;
      if ( sscanf ( & szAnswer [ 1 ], "%d", &nKey ) < 1 ) {
	nKey	= nLast;
	fprintf ( stdout, "Reading last key %d from btree %s\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
      }
      switch ( fnClientBtreeSearch ( oHeap, oBTree, nKey, eshFixnumTag,
				     &nKeyTo, &nTypeTagTo,
				     &nData, &nTypeTagData ) ) {
      case btreeNotFound:
	fprintf ( stdout, "Key %d not found in btree %s.\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      case btreeFound:
	fprintf ( stdout, "For key %d, btree %s found data\n%s.\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ),
		  fnPrintObject ( SHORT2LONGOBJID ( (OBJID) nData ), 
				  szFound, sizeof ( szFound ) ) );
	break;
      default:
	CERROR (( szIgnore, szUnexpectedValue ));
      }
      break;

    case 'i': case 'I':
      nKey		= 0;
      szData [ 0 ]	= '\0';
      if ( sscanf ( & szAnswer [ 1 ], "%d%s", &nKey, szData ) < 1 ) {
	nLast	= rand () % 1024;
	nKey	= nLast;
	fprintf ( stdout, "Inserting random key %d into btree %s\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
      }
      if ( ! szData [ 0 ] ) {
	sprintf ( szData, "data %d", nKey );
      }
      oData = LONG2SHORTOBJID ( make_string ( szData ) );
      switch ( fnClientBtreeInsert ( oHeap, oBTree, nKey, eshFixnumTag,
				     oData, eshShortObjIdTag ) ) {
      case btreeInserted:
	fprintf ( stdout, "Inserted key %d into btree %s.\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      case btreeUpdated:
	fprintf ( stdout, "Updated key %d in btree %s.\n",
		  nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      default:
	CERROR (( szIgnore, szUnexpectedValue ));
      }
      break;

    case 'm': case 'M':
      nKey		= 0;
      nTypeTag		= eshMinTag;
      nKeyTo		= 0;
      nTypeTagTo	= eshMaxTag;
      szData [ 0 ]	= '\0';
      if ( szAnswer [ 1 ] == 'd' || szAnswer [ 1 ] == 'D' ) {
	bDelete	= TRUE;
	i	= 2;
      } else {
	bDelete	= FALSE;
	i	= 1;
      }
      switch ( sscanf ( & szAnswer [ i ], "%d%d", &nKey, &nKeyTo ) ) {
      case -1:
      case 0:
	fprintf ( stdout, "Mapping all keys of btree %s\n",
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	break;
      case 1:
	fprintf ( stdout, "Mapping key %s %d into btree %s\n",
		  fnCompareTag2String ( eCompareLower, FALSE ), nKey,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	nTypeTag	= eshFixnumTag;
	break;
      default:
	fprintf ( stdout, "Mapping %d %s key %s %d into btree %s\n",
		  nKey, fnCompareTag2String ( eCompareLower, TRUE ),
		  fnCompareTag2String ( eCompareUpper, FALSE ), nKeyTo,
		  fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
	nTypeTag	= eshFixnumTag;
	nTypeTagTo	= eshFixnumTag;
      }
      nPrinted	= fnBTreeMap ( SHORT2LONGOBJID ( oHeap ),
			       SHORT2LONGOBJID ( oBTree ),
			       &nKey, nTypeTag, eCompareLower,
			       &nKeyTo, nTypeTagTo, eCompareUpper,
			       bDescending, fnTestCallback,
			       (LPVOID) ( ( bDelete ) ? (int) oHeap : 0 ) );
      fprintf ( stdout, "Map function called %d times.\n", nPrinted );
      break;

    case 'o': case 'O':
      szData [ 0 ]	= '\0';
      szFound [ 0 ]	= '\0';
      sscanf ( & szAnswer [ 1 ], "%s%s", szData, szFound );
      eCompareLower = fnString2CompareTag ( szData, eCompareLower, TRUE );
      eCompareUpper = fnString2CompareTag ( szFound, eCompareUpper, FALSE );
      fprintf ( stdout, "Criterion: lower %s key %s upper\n",
		fnCompareTag2String ( eCompareLower, TRUE ),
		fnCompareTag2String ( eCompareUpper, FALSE ) );
      break;

    case 'p': case 'P':
      fprintf ( stdout, "Dump of btree %s:\n",
		fnPrintObject ( SHORT2LONGOBJID ( oBTree ), NULL, 0 ) );
      nPrinted	= fnClientBtreePrint ( NULLOBJID, oBTree, eshStdOut );
      fprintf ( stdout, "Printed %d items.\n", nPrinted );
      break;

    case 's': case 'S':
      nKey		= -1;
      if ( sscanf ( & szAnswer [ 1 ], "%d", &nKey ) == 1 ) {
	fnClientBtreePageSize ( oHeap, oBTree, nKey );
      }
      fprintf ( stdout, "BTree page size now %d.\n",
		fnClientBtreePageSize ( oHeap, oBTree, -1 ) );
      break;

    case 'v': case 'V':
      nKey	= 1;
      szFound [ 0 ]	= '\0';
      sscanf ( & szAnswer [ 1 ], "%d%s", &nKey, szFound );
      makunbound ( oKey );
      makunbound ( oData );
      switch ( szFound [ 0 ] ) {
      case 's': case 'S':
	eOrigin	= seekSet;
	break;
      case 'e': case 'E':
	eOrigin	= seekEnd;
	break;
      default:
	eOrigin	= seekCur;
	break;
      }
      nMapped	= fnBTreeMapSeek ( SHORT2LONGOBJID ( oMapper ),
				   nKey, eOrigin, &oKey, &oData );
      if ( nMapped >= 0 ) {
	char	szMapper [ 128 ];
	PrintObject ( SHORT2LONGOBJID ( oMapper ), szMapper );
	fprintf ( stdout, "fnBTreeMapSeek on %s\n"
		  "by %d %s to key %s, data %s.\n",
		  szMapper, nMapped, szFound,
		  fnPrintObject ( oKey, NULL, 0 ),
		  PrintObject ( oData, szData ) );
      } else {
	fprintf ( stdout, "fnBTreeMapSeek returned error %d.\n",
		  nMapped );
      }
      break;

    case '\0': case 'q': case 'Q':
      bContinue		= FALSE;
      break;
    }
    fnClientTransactionEnd ( oHeap, FALSE );
  }
}

/* ----------------------------------------------------------------------- */
void fnTestList ( OBJID oHeap, int nElements )
{
  SHORTOBJID	oCons, oCar, oLast;
  int		nStartTime, nStopTime, i;

  PROCEDURE	( fnTestList );

  fprintf ( stdout, "Making list with %d elements ...\n", nElements );
  nStartTime	= time ( NULL );
  makunbound ( oLast );
  oCar	= make_string ( "list test" );
  oCar	= Long2ShortObjId ( oCar );
  fnClientTransactionBegin ( oHeap, FALSE );
  for ( i = 0; i < nElements; i++ ) {
    oCons	= fnClientDbCreateObject ( oHeap, eshConsTag, 0,
					   NULLTYPETAG, 0 );
    fnClientTransactionLockSet ( oHeap, oHeap, eshLockVectorWrite,
				 oCons, NULLTYPETAG, -1 );
    fnClientObjectWriteAtIndex ( oHeap, oCons,
				 NULLOBJID, NULLTYPETAG,
				 eshConsIdxCar, oCar, eshShortObjIdTag );
    if ( boundp ( oLast ) ) {
      fnClientObjectWriteAtIndex ( oHeap, oLast,
				   NULLOBJID, NULLTYPETAG, eshConsIdxCdr,
				   oCons, eshShortObjIdTag );
    }
    oLast	= oCons;
  }
  fnClientTransactionEnd ( oHeap, FALSE );
  nStopTime	= time ( NULL );
  fprintf ( stdout, "Done, %d seconds\n", nStopTime - nStartTime );
} /* fnTestList */

/* ----------------------------------------------------------------------- */
void fnTestMisc ()
{
  static const char	szFormat []	=
    "Type %-26.26s  tag %5d  objid size %4d  value size %4d\n";
  LPCLASSINFO	lpClassInfo;
  char		szAnswer [ 80 ];
  char		szHost [ 80 ], szTransport [ 80 ], szDirectory [ 80 ];
  BOOL		bContinue, bFound;
  int		nTypeTag, nObjIdSize, nValueSize;
  TYPEFLAGS	eTypeFlags;

  PROCEDURE	( fnTestMisc );

  bContinue	= TRUE;
  srand ( time ( NULL ) );

  fputs ( "\nMiscellaneous testing.\n", stdout );
  fflush ( stdout );

  while ( bContinue ) {
    fputs ( "\n(g)et classs info <type tag>\n"
	    "get (a)ll class infos\n"
	    "(s)plit URL\n"
	    "short (f)loat test\n"
	    "(q)uit ",
	    stdout );
    fflush ( stdout );
    szAnswer [ 0 ]	= '\0';
    fgets ( szAnswer, sizeof ( szAnswer ), stdin );
    fputc ( '\n', stdout );
    fflush ( stdout );
    switch ( szAnswer [ 0 ] ) {

    case 'a': case 'A':
      for ( bFound = fnLISPmapClassInfoFirst
	      ( &nTypeTag, szAnswer, sizeof (szAnswer ),
		&nObjIdSize, &nValueSize, &eTypeFlags );
	    bFound;
	    bFound = fnLISPmapClassInfoNext
	      ( &nTypeTag, szAnswer, sizeof (szAnswer ),
		&nObjIdSize, &nValueSize, &eTypeFlags ) ) {
	printf ( szFormat,
		 szAnswer, nTypeTag, nObjIdSize, nValueSize );
      }
      break;

    case 'f': case 'F':
      fnClientObjectPrettyPrint ( oHeapDefault, 32512, eshShortFloatTag,
				  szAnswer, sizeof ( szAnswer ) );
      printf ( "    1.0s0 == %s ?\n", szAnswer );
      fnClientObjectPrettyPrint ( oHeapDefault, 35968, eshShortFloatTag,
				  szAnswer, sizeof ( szAnswer ) );
      printf ( "12300.0s0 == %s ?\n", szAnswer );
      break;

    case 'g': case 'G':
      nTypeTag	= 0;
      sscanf ( & szAnswer [ 1 ], "%d", &nTypeTag );
      lpClassInfo	= fnFindClassInfo ( nTypeTag );
      if ( lpClassInfo ) {
	printf ( szFormat,
		 lpClassInfo->lpszTypeName,
		 lpClassInfo->nTypeTag,
		 lpClassInfo->nFixSizeObjId,
		 lpClassInfo->nFixSizeValue );
      } else {
	puts ( "Found no info for passed tag." );
      }
      break;

    case 's': case 'S':
      strcpy ( szHost, "<no host>" );
      strcpy ( szTransport, "<no transport>" );
      strcpy ( szDirectory, "<no directory>" );
      fnSplitURL ( & szAnswer [ 2 ], szHost, szTransport, szDirectory );
      printf ( "host='%s', transport='%s', directory='%s'\n",
	       szHost, szTransport, szDirectory );
      break;

    case '\0': case 'q': case 'Q':
      bContinue		= FALSE;
      break;
    }
  }
} /* fnTestMisc */

/* ----------------------------------------------------------------------- */
void fnTestPerf ()
{
  char		szAnswer [ 80 ];
  BOOL		bContinue;
  int		nElements;

  PROCEDURE	( fnTestPerf );

  bContinue	= TRUE;
  srand ( time ( NULL ) );

  fputs ( "\nPerformance testing.\n", stdout );
  fflush ( stdout );

  while ( bContinue ) {
    fputs ( "\nmake(l)ist [<elements>]\n"
	    "(q)uit ",
	    stdout );
    fflush ( stdout );
    szAnswer [ 0 ]	= '\0';
    fgets ( szAnswer, sizeof ( szAnswer ), stdin );
    fputc ( '\n', stdout );
    switch ( szAnswer [ 0 ] ) {

    case 'l': case 'L':
      nElements	= 1000;
      sscanf ( & szAnswer [ 1 ], "%d", &nElements );
      fnTestList ( oHeapDefault, nElements );
      break;

    case '\0': case 'q': case 'Q':
      bContinue		= FALSE;
      break;
    }
  }
} /* fnTestPerf */

/* ----------------------------------------------------------------------- */
int main ()
{
#if defined(NORPC)
  static char		szURL []	= "database";
#else
  static char		szURL []	= "tcp://localhost/database";
#endif

  char			szAnswer [ 80 ];
  BOOL			bContinue;

#if defined(NORPC)
  StoreSession ( 1 );
#endif

  fnGlobalSetErrorHandler ( fnGlobalErrorHandler );
  fnRegisterCcallable ( "fnLISPerrorCallback", fnGlobalErrorHandler );

  fnClientDbOpen ( szURL, "test", 0 );
  oHeapDefault	= fnClientDbCreateObject ( NULLOBJID, eshHeapTag, 0,
					   NULLTYPETAG, 0 );

  bContinue	= TRUE;
  fflush ( stdin );
  while ( bContinue ) {
    fputs ( "\f"
	    "\nPersistent Lisp OBject test module\n"
	    "\n"
	    "(B)Trees\n"
	    "(L)ocking and transactions\n"
	    "(P)erformance tests\n"
	    "(S)trings\n"
	    "S(t)abilise\n"
	    "(M)isc\n"
	    "\n"
	    "(Q)uit\n"
	    "\n"
	    "==> ",
	    stdout );
    fflush ( stdout );
    szAnswer [ 0 ]	= '\0';
    if ( ! fgets ( szAnswer, sizeof ( szAnswer ), stdin ) ) {
      break;
    }
    fputc ( '\n', stdout );
    fflush ( stdout );
    switch ( szAnswer [ 0 ] ) {
    case 'b': case 'B':
      fnTestBTree ();
      break;
    case 'l': case 'L':
      fnTestLocking ();
      break;
    case 'm': case 'M':
      fnTestMisc ();
      break;
    case 'p': case 'P':
      fnTestPerf ();
      break;
    case 's': case 'S':
      fnTestString ();
      break;
    case 't': case 'T':
      fnClientDbStabilise ( oHeapDefault );
      break;
    case '\0': case 'q': case 'Q':
      bContinue	= FALSE;
      break;
    default:
      break;
    }
  }
  fnClientDbClose ( oHeapDefault, FALSE );
  exit ( 0 );
  RETURN ( 0 );
} /* main */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
