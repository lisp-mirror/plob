/* -------------------------------------------------------------------------
| Module	lwobject.c
| Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
|		kirschke@informatik.uni-hamburg.de
| Date		22.10.92
| Description	
 ------------------------------------------------------------------------- */
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"_global.h"
#include	"_hash.h"
#include	"lwobject.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
MODULE ( __FILE__ );
enum {
  _MAX_LEVEL	= 1024
};

/* -------------------------------------------------------------------------
| Static types and variables
 ------------------------------------------------------------------------- */
static LWCLASS		lwClass2s [ 256 ];
static LWCLASS		lwClass1s [ 256 ];

static CONST char	szTooMuch []		= "...";
static CONST char	szNILnotRegistered []	=
  "NIL was not registered";

/* struct containing LispWorks data object reference information: */
typedef struct {
  WORD	wReferenced;	/* No of references to object - 1 */
  WORD	wPrintId;	/* Used in printf ( "#%lu=", wPrintId ) */
}	LWREFENTRY, FAR * LPLWREFENTRY;
static CONST LWREFENTRY	NullRef	= { 0, 0 };

/* struct containing a part of the arguments for the fnLWdoPut...
   functions: */
typedef struct {
  FILE		FAR * lpStream;		/* Output stream */
  WORD		wMaxLevel;		/* Max. level to print */
  LPHASHTABLE	lpObjTab;		/* Hash table with referenced objs */
  WORD		wPrintId;		/* Actual print ID */
}	LWPRINTARG, FAR * LPLWPRINTARG;

/* -------------------------------------------------------------------------
| Static function prototypes
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Function	fnLWgetClosure
| Arguments	  lwObject
|		The LispWorks LISP data object whose reference closure
|		is inserted into lpObjTab.
| Return	Number of referenced objects.
| Description	Inserts all objects into lpObjTab which are referenced
|		by lwObject. Only references relevant for printing the
|		object are considered, not all references contained in
|		lwObject.
 ------------------------------------------------------------------------- */
static int	fnLWgetClosure		( LWOBJECT lwObject,
					  LPHASHTABLE lpObjTab );

/* -------------------------------------------------------------------------
| Function	fnLWdoPutInt,...,fnLWdoPutUnknown
| Arguments	  lpPrintArg
|		Arguments needed for printing the object.
|		  wLevel
|		Current print level (used for terminating too deep nested
|		structures)
|		  lwObject
|		The object which should be printed
| Return	Number of printed (sub-)objects
| Description	Class-specific output functions; each function prints an
|		object of the corresponding type.
 ------------------------------------------------------------------------- */
static int	fnLWdoPutInt		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutCons		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutSymbol		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
#define		fnLWdoPutClass1		fnLWdoPutUnknown
static int	fnLWdoPutChar		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutMark		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
#define		fnLWdoPutMarkCatch	fnLWdoPutMark
#define		fnLWdoPutMarkSpecial	fnLWdoPutMark
#define		fnLWdoPutMarkUnbound	fnLWdoPutMark
#define		fnLWdoPutMarkSlotUnbound	fnLWdoPutMark
#define		fnLWdoPutMarkBadPointer	fnLWdoPutMark
#define		fnLWdoPutMarkRest	fnLWdoPutMark
static int	fnLWdoPutShortFloat	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutTLatter	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutArrayAux	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
					  LPLWOBJECT lplwTable,
					  int nCurrentDimension,
					  int nNoOfDimension,
					  LPLWOBJECT lplwDimension );
static int	fnLWdoPutArray		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutString		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutBitVector	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutVector		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutClosObject	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
#define		fnLWdoPutSingleFloat	fnLWdoPutUnknown
#define		fnLWdoPutLongFloat	fnLWdoPutUnknown
static int	fnLWdoPutStruct		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutLongInt	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutRatio		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutComplex	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
static int	fnLWdoPutUnknown	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );

/* -------------------------------------------------------------------------
| Function	fnLWdoPut
| Arguments	  lpPrintArg
|		Arguments needed for printing the object.
|		  wLevel
|		Current print level (used for terminating too deep nested
|		structures)
|		  lpszPrefix
|		Prefix string to print before the object (normally "" or
|		" ")
|		  lwObject
|		The object which should be printed
| Return	Number of printed (sub-)objects
| Description	'Work-horse' for fnLWfput, see lwobject.h
 ------------------------------------------------------------------------- */
static int	fnLWdoPut		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
					  LPCSTR lpszPrefix,
                                          LWOBJECT lwObject );

/* -------------------------------------------------------------------------
| Extern globals
 ------------------------------------------------------------------------- */
LPLWCLASS	lplwClass2s	= NULL;
LPLWCLASS	lplwClass1s	= NULL;

LWOBJECT	lwNil		= 0;
LWOBJECT	lwT		= 0;

/* -------------------------------------------------------------------------
| Static function bodies
 ------------------------------------------------------------------------- */
static int	fnLWgetClosure		( LWOBJECT lwObject,
					  LPHASHTABLE lpObjTab )
{
  LPLWCLASS1STRUCT	lpClass1;
  LPLWREFENTRY		lpRef;
  int			i, n, nInserted;
  LWOBJECT		lwI;

  ASSERT ( lpObjTab != NULL );
  nInserted	= 0;
  switch ( LWGETCLASS ( lwObject ) ) {
    /* Values: */
  case lwInt:
  case lwSymbol:
  case lwChar:
  case lwMarkCatch:
  case lwMarkSpecial:
  case lwMarkUnbound:
  case lwMarkSlotUnbound:
  case lwMarkBadPointer:
  case lwMarkRest:
  case lwShortFloat:
  case lwBitVector:
  case lwSingleFloat:
  case lwLongFloat:
  case lwLongInt:
  case lwRatio:
  case lwComplex:
    break;
    /* LispWorks data objects with references: */
  case lwCons:
    if ( lwNil ) {
      lwI	= lwObject;
      while ( lwI != lwNil ) {
	if ( LWCONSP ( lwI ) ) {
	  lpRef	= fnHashGet ( lpObjTab, lwI );
	  if ( lpRef ) {
	    /* Circular reference detected: */
	    lpRef->wReferenced++;
	    break;
	  }
	  fnHashInsert ( lpObjTab, lwI, &NullRef );
	  nInserted	+= fnLWgetClosure ( LWCAR ( lwI ), lpObjTab );
	  lwI		= LWCDR ( lwI );
	} else {
	  nInserted	= fnLWgetClosure ( lwI, lpObjTab );
	  break;
	}
      }
    } else {
      CERROR (( szNILnotRegistered ));
      lpRef	= fnHashGet ( lpObjTab, lwObject );
      if ( lpRef ) {
	lpRef->wReferenced++;
      } else {
	fnHashInsert ( lpObjTab, lwObject, &NullRef );
	nInserted	= fnLWgetClosure ( LWCAR ( lwObject ), lpObjTab ) +
	  fnLWgetClosure ( LWCDR ( lwObject ), lpObjTab );
      }
    }
    break;
  case lwTLatter:
    lpRef	= fnHashGet ( lpObjTab, lwObject );
    if ( lpRef ) {
      lpRef->wReferenced++;
    } else {
      fnHashInsert ( lpObjTab, lwObject, &NullRef );
      nInserted	= fnLWgetClosure ( LWCLASS1STRUCTPTR ( lwObject, lwTLatter )->
				   IsA.TLatter.lwValue, lpObjTab );
    }
    break;
  case lwArray:
    lpRef	= fnHashGet ( lpObjTab, lwObject );
    if ( lpRef ) {
      lpRef->wReferenced++;
    } else {
      fnHashInsert ( lpObjTab, lwObject, &NullRef );
      nInserted	= fnLWgetClosure ( LWCLASS1STRUCTPTR ( lwObject, lwArray )->
				   IsA.Array.lwData, lpObjTab );
    }
    break;
  case lwString:
    lpRef	= fnHashGet ( lpObjTab, lwObject );
    if ( lpRef ) {
      lpRef->wReferenced++;
    } else {
      fnHashInsert ( lpObjTab, lwObject, &NullRef );
      nInserted	= 1;
    }
    break;
  case lwVector:
    lpRef	= fnHashGet ( lpObjTab, lwObject );
    if ( lpRef ) {
      lpRef->wReferenced++;
    } else {
      fnHashInsert ( lpObjTab, lwObject, &NullRef );
      lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwVector );
      n		= LWINT ( lpClass1->IsA.Vector.lwnLength );
      for ( i = 0; i < n; i++ ) {
	nInserted	+= fnLWgetClosure ( lpClass1->IsA.Vector.lwData [ i ],
					    lpObjTab );
      }
    }
    break;
  case lwClosObject:
    lpRef	= fnHashGet ( lpObjTab, lwObject );
    if ( lpRef ) {
      lpRef->wReferenced++;
    } else {
      fnHashInsert ( lpObjTab, lwObject, &NullRef );
      nInserted	= 1;
    }
    break;
  case lwStruct:
    lpRef	= fnHashGet ( lpObjTab, lwObject );
    if ( lpRef ) {
      lpRef->wReferenced++;
    } else {
      fnHashInsert ( lpObjTab, lwObject, &NullRef );
      lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwStruct );
      n		= LWINT ( lpClass1->IsA.Struct.lwnLength ) - 1;
      for ( i = 0; i < n; i++ ) {
	nInserted	+= fnLWgetClosure ( lpClass1->IsA.Struct.lwData [ i ],
					    lpObjTab );
      }
    }
    break;
    /* unknown object: */
  default:
    CERROR (( "Found unknown object 0x%lX of class %s in fnLWgetClosure",
	      (DWORD) lwObject,
	      fnLWclassToString ( LWGETCLASS ( lwObject ), NULL, 0 ) ));
  }
  return nInserted;
} /* fnLWgetClosure */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutInt		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  fprintf ( lpPrintArg->lpStream, "%d", LWINT ( lwObject ) );
  return 1;
} /* fnLWdoPutInt */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutCons		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int		nPrinted, i, nClosing;
  LWOBJECT	lwI, lwLast;
  LPCSTR	lpszPrefix;
  LPLWREFENTRY	lpRef;

  ASSERT ( LWCONSP ( lwObject ) );
  nPrinted	= 0;
  fputc ( '(', lpPrintArg->lpStream );
  nClosing	= 1;
  if ( lwNil ) {
    lwI		= lwObject;
    if ( wLevel == lpPrintArg->wMaxLevel ) {
      while ( lwI != lwNil ) {
	nPrinted++;
	if ( LWGETCLASS ( lwI ) == lwCons ) {
	  lpRef	= fnHashGet ( lpPrintArg->lpObjTab, lwI );
	  if ( lpRef && lpRef->wReferenced > 0 )
	    /* Circular reference detected: */
	    break;
	  lwI	= LWCDR ( lwI );
	} else
	  break;
      }
      switch ( nPrinted ) {
      case 0:
	break;
      case 1:
	fputs ( szTooMuch, lpPrintArg->lpStream );
	break;
      default:
	fprintf ( lpPrintArg->lpStream, "%d*%s", nPrinted, szTooMuch );
	break;
      }
    } else {
      lpszPrefix	= szEmpty;
      lwLast		= lwNil;
      while ( lwI != lwNil ) {
	if ( LWCONSP ( lwI ) && lwI != lwLast ) {
	  lpRef		= fnHashGet ( lpPrintArg->lpObjTab, lwI );
	  if ( lpRef && lpRef->wReferenced > 0 ) {
	    /* Circular reference detected: */
	    if ( lwLast == lwNil )
	      lwLast	= lwI;
	    if ( lpRef->wPrintId == 0 ) {
	      /* The object wasn't printed up to now: */
	      lpRef->wPrintId	= lpPrintArg->wPrintId++;
	      fprintf ( lpPrintArg->lpStream, "%s. #%lu=(", lpszPrefix,
		        (DWORD) lpRef->wPrintId );
	      nClosing++;
	      lpszPrefix	= szEmpty;
	    }
	  }
	  nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel, lpszPrefix,
				       LWCAR ( lwI ) );
	  lpszPrefix	= szSpace;
	  lwI		= LWCDR ( lwI );
	} else {
	  fprintf ( lpPrintArg->lpStream, " ." );
	  nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel, szSpace, lwI );
	  break;
	}
      }
    }
  } else {
    CERROR (( szNILnotRegistered ));
    nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel, NULL, LWCAR ( lwObject ) );
    fprintf ( lpPrintArg->lpStream, " ." );
    nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel, szSpace,
			       LWCDR ( lwObject ) );
  }
  for ( i = 0; i < nClosing; i++ )
    fputc ( ')', lpPrintArg->lpStream );
  return nPrinted;
} /* fnLWdoPutCons */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutSymbol		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			nPrinted;
  LPLWCLASS1STRUCT	lpStructPkg;
  LPSTR			lpszString;

  nPrinted	= 0;

#if 0
  /* Ouput package name: */
  if ( (LWOBJECT) LWCLASS1STRUCTPTR ( lwObject, lwSymbol )->lwPackage ==
       lwNil ) {
    fputs ( "NIL!", lpPrintArg->lpStream );
  } else {
    lpStructPkg	=
      LWCLASS1STRUCTPTR ( LWCLASS1STRUCTPTR ( lwObject, lwSymbol )->lwPackage,
			  lwStruct );
    if ( LWGETCLASS ( lpStructPkg->IsA.Struct.lwData [ 1 ] ) ==
	 lwString ) {
      fputs ( LWCLASS1STRUCTPTR ( lpStructPkg->
				  IsA.Struct.lwData [ 1 ], lwString )->
	      IsA.String.sData, lpPrintArg->lpStream );
    } else {
      nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel + 1, NULL,
			       lpStructPkg->IsA.Struct.lwData [ 1 ] );
    }
  }
  fputc ( ':', lpPrintArg->lpStream );
#endif

  if ( LWGETCLASS ( LWCLASS1STRUCTPTR ( lwObject, lwSymbol )->
		    IsA.Symbol.lwsName ) == lwString ) {
    lpszString	=
      LWCLASS1STRUCTPTR ( LWCLASS1STRUCTPTR ( lwObject, lwSymbol )->
			  IsA.Symbol.lwsName, lwString )->IsA.String.sData;
    fputs ( lpszString, lpPrintArg->lpStream );
    nPrinted++;
  } else {
    fputs ( "#<Function ", lpPrintArg->lpStream );
    nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel, NULL,
			       LWCLASS1STRUCTPTR ( lwObject, lwSymbol )->
			       IsA.Symbol.lwConstant );
    fputc ( '>', lpPrintArg->lpStream );
  }

  return nPrinted;
} /* fnLWdoPutSymbol */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutChar		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
#if 0
  fputs ( "#\\", lpPrintArg->lpStream );
  fputc ( LWCHAR ( lwObject ), lpPrintArg->lpStream );
#else
  fprintf ( lpPrintArg->lpStream, "char-class=0x%lX#\\%c",
	    (DWORD) ( lwObject & 0xFF ), LWCHAR ( lwObject ) );
#endif
  return 1;
} /* fnLWdoPutChar */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutMark		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  char	szMark [ 32 ];

  fnLWclassToString ( LWGETCLASS ( lwObject ), szMark, sizeof ( szMark ) );
  fputs ( "#<", lpPrintArg->lpStream );
  fputs ( szMark, lpPrintArg->lpStream );
  fputc ( '>', lpPrintArg->lpStream );
  return 1;
} /* fnLWdoPutMark */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutShortFloat	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  fprintf ( lpPrintArg->lpStream, "%f", LWSHORTFLOAT ( lwObject ) );
  return 1;
} /* fnLWdoPutShortFloat */


/* ------------------------------------------------------------------------ */
static int	fnLWdoPutTLatter	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			nPrinted;
  LPLWCLASS1STRUCT	lpClass1;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwTLatter );
  nPrinted	= fnLWdoPut ( lpPrintArg, wLevel, NULL,
			      lpClass1->IsA.TLatter.lwValue );
  return nPrinted;
} /* fnLWdoPutTLatter */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutArrayAux	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
					  LPLWOBJECT lplwTable,
					  int nCurrentDimension,
					  int nNoOfDimension,
					  LPLWOBJECT lplwDimension )
{
  int	nPrinted, i, j, k, n;

  n		= LWINT ( lplwDimension [ nCurrentDimension ] );
  nPrinted	= 0;
  fputc ( '(', lpPrintArg->lpStream );
  if ( nCurrentDimension + 1 >= nNoOfDimension ) {
    /* Reached the last dimension: */
    for ( i = 0; i < n; i++ )
      nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel + 1,
			       ( i == 0 ) ? NULL : szSpace, lplwTable [ i ] );
  } else {
    /* Compute the size of one row by multiplication of all dimension sizes
       following the current dimension size: */
    for ( i = nCurrentDimension + 1, k = 1; i < nNoOfDimension; i++ ) {
      k	*= LWINT ( lplwDimension [ i ] );
    }
    for ( i = 0, j = 0; i < n; i++, j += k ) {
      if ( i > 0 )
	fputc ( ' ', lpPrintArg->lpStream );
      /* Well: A good example for *really needed* recursion: */
      nPrinted	+= fnLWdoPutArrayAux ( lpPrintArg, wLevel + 1,
				       & lplwTable [ j ],
				       nCurrentDimension + 1,
				       nNoOfDimension, lplwDimension );
    }
  }
  fputc ( ')', lpPrintArg->lpStream );
  return nPrinted;
} /* fnLWDoPutArrayAux */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutArray		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			nPrinted, i, n;
  LPLWCLASS1STRUCT	lpClass1;
  LPCSTR		lpszPrefix;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwArray );
  nPrinted	= 0;

  /* Set n to number of dimensions: */
  n	= LWWORD ( lpClass1->IsA.Array.lwnLength ) - 4;
  fprintf ( lpPrintArg->lpStream, "#%luA", (DWORD) n );
  if ( LWGETCLASS ( lpClass1->IsA.Array.lwData ) == lwVector ) {
    /* The array table is of type vector: */
    i	= 0;
    if ( lpClass1->IsA.Array.lwnDisplacedOffset != lwNil ) {
      i	= LWINT ( lpClass1->IsA.Array.lwnDisplacedOffset );
    }
    if ( lpClass1->IsA.Array.lwnFillPointer == lwNil ) {
      /* The fill-pointer is NIL, so print array 'as usual': */
      nPrinted	+=
	fnLWdoPutArrayAux ( lpPrintArg, wLevel,
			    & LWCLASS1STRUCTPTR ( lpClass1->
						  IsA.Array.lwData,
						  lwVector )->
			    IsA.Vector.lwData [ i ], 0, n,
			    lpClass1->IsA.Array.lwnDimension );
    } else {
      /* The fill-pointer is not NIL, so the array is 1-dimensional
	 (otherwise the next assert will fail): */
      ASSERT ( n == 1 );
      n		= LWINT ( lpClass1->IsA.Array.lwnFillPointer );
      lpszPrefix	= NULL;
      fputc ( '(', lpPrintArg->lpStream );
      for ( /* i = i */; i < n; i++ ) {
	fnLWdoPut ( lpPrintArg, wLevel + 1, lpszPrefix,
		    LWCLASS1STRUCTPTR ( lpClass1->IsA.Array.lwData,
				        lwUnknown )->
		    IsA.Vector.lwData [ i ] );
	lpszPrefix	= szSpace;
      }
      fputc ( ')', lpPrintArg->lpStream );
    }
  } else {
    /* The array table is of type bitvector: */
    fputc ( '[', lpPrintArg->lpStream );
    for ( i = 0; i < n; i++ ) {
      if ( i > 0 )
	fputc ( ',', lpPrintArg->lpStream );
      fprintf ( lpPrintArg->lpStream, "%lu",
	        LWWORD ( lpClass1->IsA.Array.lwnDimension [ i ] ) );
    }
    fputc ( ']', lpPrintArg->lpStream );
    nPrinted	+= fnLWdoPut ( lpPrintArg, lpClass1->IsA.Array.lwData,
			       NULL, wLevel + 1 );
  }
  return nPrinted;
} /* fnLWdoPutArray */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutString		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  LPLWCLASS1STRUCT	lpClass1;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwString );
  fputc ( '"', lpPrintArg->lpStream );
  fputs ( lpClass1->IsA.String.sData, lpPrintArg->lpStream );
  fputc ( '"', lpPrintArg->lpStream );
  return 1;
} /* fnLWdoPutString */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutBitVector	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			i, j, n;
  LPLWCLASS1STRUCT	lpClass1;
  LPBYTE		lpData;
  WORD			wData;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwBitVector );
  n		= LWWORD ( lpClass1->IsA.BitVector.lwnLength );
  fputs ( "#*", lpPrintArg->lpStream );
  for ( i = 0, j = 0,
        lpData = lpClass1->IsA.BitVector.bData, wData = (WORD) *(lpData++);
        i < n; i++ ) {
    fputc ( '0' + ( wData & 0x01 ), lpPrintArg->lpStream );
    if ( j++ < 7 ) {
      wData	>>= 1;
    } else {
      j		= 0;
      wData	= (WORD) *(lpData++);
    }
  }
  return 1;
} /* fnLWdoPutBitVector */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutVector		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			nPrinted, i, n;
  LPLWCLASS1STRUCT	lpClass1;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwVector );
  n		= LWWORD ( lpClass1->IsA.Vector.lwnLength );
  nPrinted	= 0;
#if 1
  fputs ( "#(", lpPrintArg->lpStream );
#else
  fprintf ( lpPrintArg->lpStream, "0x%X 0x%X#(",
	    LWCLASS1STRUCTPTR ( lwObject, lwUnknown )->wClass1,
	    LWCLASS1STRUCTPTR ( lwObject, lwUnknown )->wFlag );
#endif
  if ( wLevel == lpPrintArg->wMaxLevel ) {
    switch ( n ) {
    case 0:
      break;
    case 1:
      fputs ( szTooMuch, lpPrintArg->lpStream );
      break;
    default:
      fprintf ( lpPrintArg->lpStream, "%d*%s", n, szTooMuch );
      break;
    }
  } else {
    for ( i = 0; i < n; i++ ) {
      nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel,
			       ( i == 0 ) ? NULL : szSpace,
			       lpClass1->IsA.Vector.lwData [ i ] );
    }
  }
  fputc ( ')', lpPrintArg->lpStream );
  return nPrinted;
} /* fnLWdoPutVector */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutClosObject	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  LPLWCLASS1STRUCT	lpClass1;
  LWOBJECT		lwClosClassObject;
  LPLWCLASS1STRUCT	lpVectorClassData;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwClosObject );

  lwClosClassObject	=
    LWCLASS1STRUCTPTR ( lpClass1->IsA.ClosObject.lwClassWrapper,
		        lwVector )->IsA.Vector.lwData [ 3 ];
  /* lwClosClassObject now contains the class object of lwObject. It
     must be an instance of standard-class or a subclass of standard-
     class: */
  lpVectorClassData	=
    LWCLASS1STRUCTPTR ( LWCLASS1STRUCTPTR ( lwClosClassObject,
					    lwClosObject )->
		        IsA.ClosObject.lwInstanceData,
		        lwVector );
  /* lpVectorClass Data now contains the instance data vector of the
     class object of lwObject: */
  fputs ( "#<", lpPrintArg->lpStream );
  /* The class name is in the slot with index 1 in the instance data
     vector of the class object of lwObject: */
  fnLWdoPut ( lpPrintArg, wLevel+1, NULL,
	      lpVectorClassData->IsA.Vector.lwData [ 1 ] );
  fprintf ( lpPrintArg->lpStream, " 0x%lX>", (DWORD) lpClass1 );
  return 1;
} /* fnLWdoPutClosObject */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutStruct		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			nPrinted, i, n;
  LPLWCLASS1STRUCT	lpClass1, lpVectorStruct;
  LWOBJECT		lwSlotDesc;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwStruct );
  nPrinted	= 0;

  n			= LWWORD ( lpClass1->IsA.Struct.lwnLength ) - 1;
  lpVectorStruct	= LWCLASS1STRUCTPTR ( lpClass1->
					      IsA.Struct.lwStructDesc,
					      lwVector );
  fputs ( "#S(", lpPrintArg->lpStream );
  /* Index 9: struct name: */
  nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel + 1, NULL,
			       lpVectorStruct->IsA.Vector.lwData [ 9 ] );
  /* Index 11: list or vector with slot descriptions: */
  lwSlotDesc	= lpVectorStruct->IsA.Vector.lwData [ 11 ];
  for ( i = 0; i < n; i++ ) {
    switch ( LWGETCLASS ( lwSlotDesc ) ) {
    case lwCons:
      /* car of lwSlotDesc contains a 5 slot struct which is named
	 structure::defstruct-slot-description in LispWorks. Its slots
	 are: name, index, accessor, default, type, read-only. */
      fputc ( ' ', lpPrintArg->lpStream );
      nPrinted	+=
	fnLWdoPut ( lpPrintArg, wLevel + 1, NULL,
		    LWCLASS1STRUCTPTR ( LWCAR ( lwSlotDesc ),
				        lwStruct )->IsA.Struct.lwData [ 0 ] );
      lwSlotDesc	= LWCDR ( lwSlotDesc );
      break;
    case lwVector:
      fputc ( ' ', lpPrintArg->lpStream );
      nPrinted	+=
	fnLWdoPut ( lpPrintArg, wLevel + 1, NULL,
		    LWCLASS1STRUCTPTR ( lwSlotDesc, lwVector )->
		    IsA.Vector.lwData [ i + 1 ] );
      break;
    default:
      break;
    }
    fputc ( ' ', lpPrintArg->lpStream );
    nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel + 1, NULL,
			       lpClass1->IsA.Struct.lwData [ i ] );
  }
  fputc ( ')', lpPrintArg->lpStream );
  return nPrinted;
} /* fnLWdoPutStruct */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutLongInt	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			i, n;
  LPLWCLASS1STRUCT	lpClass1;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwLongInt );
  fputs ( "#x", lpPrintArg->lpStream );
  n = LWINT ( lpClass1->IsA.LongInt.lwnLength );
  for ( i = n - 1; i >= 0; i-- )
    fprintf ( lpPrintArg->lpStream, "%08.8X",
	      lpClass1->IsA.LongInt.wData [ i ] );
  return 1;
} /* fnLWdoPutLongInt */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutRatio		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			nPrinted;
  LPLWCLASS1STRUCT	lpClass1;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwRatio );
  nPrinted	= fnLWdoPut ( lpPrintArg, wLevel, NULL,
			      lpClass1->IsA.Ratio.lwEnumerator );
  nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel, "/",
			       lpClass1->IsA.Ratio.lwDenominator );
  return nPrinted;
} /* fnLWdoPutRatio */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutComplex	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  int			nPrinted;
  LPLWCLASS1STRUCT	lpClass1;

  lpClass1	= LWCLASS1STRUCTPTR ( lwObject, lwComplex );
  fputs ( "#C(", lpPrintArg->lpStream );
  nPrinted	= fnLWdoPut ( lpPrintArg, wLevel, NULL,
			      lpClass1->IsA.Complex.lwReal );
  nPrinted	+= fnLWdoPut ( lpPrintArg, wLevel, szSpace,
			       lpClass1->IsA.Complex.lwImaginary );
  fputc ( ')', lpPrintArg->lpStream );
  return nPrinted;
} /* fnLWdoPutComplex */

/* ------------------------------------------------------------------------ */
static int	fnLWdoPutUnknown	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject )
{
  if ( LWGETCLASS2 ( lwObject ) == lwClass1 ) {
    fprintf ( lpPrintArg->lpStream, "#<class 1 0x%x %s 0x%X>",
	      (unsigned int)
	      ( LWCLASS1STRUCTPTR ( lwObject, lwUnknown )->wHeader & 0xFF ),
	      fnLWclassToString ( LWGETCLASS ( lwObject ), NULL, 0 ),
	      lwObject );
  } else {
    fprintf ( lpPrintArg->lpStream, "#<class 2 0x%x %s 0x%X>",
	      ((unsigned int)(lwObject))&0xFF,
	      fnLWclassToString ( LWGETCLASS ( lwObject ), NULL, 0 ),
	      lwObject );
  }
  return 1;
} /* fnLWdoPutUnknown */


/* ------------------------------------------------------------------------ */
/* HK 17.09.92: Druckt das erhaltene Argument aus: */
static int	fnLWdoPut		( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
					  LPCSTR lpszPrefix,
                                          LWOBJECT lwObject )
{
  typedef int ( FAR * LPFNDOPUT )	( LPLWPRINTARG lpPrintArg,
					  WORD wLevel,
                                          LWOBJECT lwObject );
  static CONST LPFNDOPUT	lplpfnDoPut []	= {
    fnLWdoPutInt,
    fnLWdoPutCons,
    fnLWdoPutSymbol,
    fnLWdoPutClass1,
    fnLWdoPutChar,
    fnLWdoPutMarkCatch,
    fnLWdoPutMarkSpecial,
    fnLWdoPutMarkUnbound,
    fnLWdoPutMarkSlotUnbound,
    fnLWdoPutMarkBadPointer,
    fnLWdoPutMarkRest,
    fnLWdoPutShortFloat,
    fnLWdoPutTLatter,
    fnLWdoPutArray,
    fnLWdoPutString,
    fnLWdoPutBitVector,
    fnLWdoPutStruct,
    fnLWdoPutVector,
    fnLWdoPutClosObject,
    fnLWdoPutSingleFloat,
    fnLWdoPutLongFloat,
    fnLWdoPutLongInt,
    fnLWdoPutRatio,
    fnLWdoPutComplex
  };

  LPLWREFENTRY	lpRef;
  BOOL		bPrintObject;
  LWCLASS	lwClassOfObject;

  /* If this ASSERT fails, the enum type LWCLASS has been changed in its
     number of elements but lplpfnDoPut wasn't changed too: */
  ASSERT ( length ( lplpfnDoPut ) == lwMaximum );

  if ( lpszPrefix && *lpszPrefix )
    fputs ( lpszPrefix, lpPrintArg->lpStream );
  if ( ++wLevel > lpPrintArg->wMaxLevel ) {
    fputs ( szTooMuch, lpPrintArg->lpStream );
  } else {
    lpRef		= fnHashGet ( lpPrintArg->lpObjTab, lwObject );
    bPrintObject	= TRUE;
    if ( lpRef && lpRef->wReferenced > 0 ) {
      /* The object is referenced more than once: */
      bPrintObject	= lpRef->wPrintId == 0;
      if ( bPrintObject ) {
	/* The object wasn't printed up to now: */
	lpRef->wPrintId	= lpPrintArg->wPrintId++;
	fprintf ( lpPrintArg->lpStream, "#%lu=", (DWORD) lpRef->wPrintId );
      } else {
	/* The object was already printed: */
	fprintf ( lpPrintArg->lpStream, "#%lu#", (DWORD) lpRef->wPrintId );
      }
    }
    if ( bPrintObject ) {
      lwClassOfObject	= LWGETCLASS ( lwObject );
      if ( 0 <= lwClassOfObject &&
	   lwClassOfObject < length ( lplpfnDoPut ) ) {
	return ( * lplpfnDoPut [ lwClassOfObject ] )
	  ( lpPrintArg, wLevel, lwObject );
      }
      return fnLWdoPutUnknown ( lpPrintArg, wLevel, lwObject );
    }
  }
  return 1;
} /* fnLWdoPut */

/* -------------------------------------------------------------------------
| Extern function bodies
 ------------------------------------------------------------------------- */
LPLWCLASS	fnLWinitClass2s		( void )
{
  int	i;

  if ( ! lplwClass2s ) {
    lplwClass2s	= lwClass2s;
    for ( i = 0; i < length ( lwClass2s ); i++ )
      lwClass2s [ i ]	= lwUnknown;
    lwClass2s [ 0x00 ]	= lwInt;		/* 0000 */
    lwClass2s [ 0x01 ]	= lwCons;		/* 0001 */
    lwClass2s [ 0x02 ]	= lwSymbol;		/* 0010 */
    lwClass2s [ 0x03 ]	= lwClass1;		/* 0011 */
    lwClass2s [ 0x04 ]	= lwInt;		/* 0100 */
    lwClass2s [ 0x05 ]	= lwCons;		/* 0101 */
    lwClass2s [ 0x06 ]	= lwChar;		/* 0110 */
    lwClass2s [ 0x07 ]	= lwClass1;		/* 0111 */
    lwClass2s [ 0x08 ]	= lwInt;		/* 1000 */
    lwClass2s [ 0x09 ]	= lwCons;		/* 1001 */
    lwClass2s [ 0x0A ]	= lwSymbol;		/* 1010 */
    lwClass2s [ 0x0B ]	= lwClass1;		/* 1011 */
    lwClass2s [ 0x0C ]	= lwInt;		/* 1100 */
    lwClass2s [ 0x0D ]	= lwCons;		/* 1101 */
    lwClass2s [ 0x0E ]	= lwShortFloat;		/* 1110 */
    lwClass2s [ 0x0F ]	= lwClass1;		/* 1111 */
    for ( i = 16; i < length ( lwClass2s ); i += 16 )
	 memcpy ( & lwClass2s [ i ], & lwClass2s [ 0 ],
		  sizeof ( lwClass2s [ 0 ] ) * 16 );
    lwClass2s [ 0x16 ]	= lwMarkCatch;
    lwClass2s [ 0x26 ]	= lwMarkSpecial;
    lwClass2s [ 0x36 ]	= lwMarkUnbound;
    lwClass2s [ 0x46 ]	= lwMarkSlotUnbound;
    lwClass2s [ 0x66 ]	= lwMarkBadPointer;
    lwClass2s [ 0x76 ]	= lwMarkRest;
  }
  return lplwClass2s;
} /* fnLWinitClass2s */

/* ------------------------------------------------------------------------ */
LWCLASS		fnLWgetClass2		( LWOBJECT lwObject )
{
  return LWGETCLASS2 ( lwObject );
} /* fnLWgetClass2 */

/* ------------------------------------------------------------------------ */
LPLWCLASS	fnLWinitClass1s		( void )
{
  int	i;

  if ( ! lplwClass1s ) {
    lplwClass1s	= lwClass1s;
    for ( i = 0; i < length ( lwClass1s ); i++ )
      lwClass1s [ i ]	= lwUnknown;
    lwClass1s [ 0x36 ]	= lwTLatter;
    lwClass1s [ 0x56 ]	= lwArray;
    lwClass1s [ 0x66 ]	= lwString;
    lwClass1s [ 0x6E ]	= lwBitVector;
    lwClass1s [ 0x76 ]	= lwStruct;
    lwClass1s [ 0x7E ]	= lwVector;
    lwClass1s [ 0x9E ]	= lwClosObject;
    lwClass1s [ 0xC6 ]	= lwSingleFloat;
    lwClass1s [ 0xCE ]	= lwLongFloat;
    lwClass1s [ 0xE6 ]	= lwLongInt;
    lwClass1s [ 0xF6 ]	= lwRatio;
    lwClass1s [ 0xFE ]	= lwComplex;
  }
  return lplwClass1s;
} /* fnLWinitClass1s */

/* ------------------------------------------------------------------------ */
LWCLASS		fnLWgetClass1		( LWOBJECT lwObject )
{
  return LWGETCLASS1 ( lwObject );
} /* fnLWgetClass1 */

/* ------------------------------------------------------------------------ */
LWCLASS		fnLWgetClass		( LWOBJECT lwObject )
{
  return LWGETCLASS ( lwObject );
} /* fnLWgetClass */

/* ------------------------------------------------------------------------- */
int		fnLWfput		( LWOBJECT lwObject,
					  FILE FAR * lpStream )
{
  static LPHASHTABLE	lpObjectTab	= NULL;
  int			nReturn;
  LWPRINTARG		PrintArg;

  lpObjectTab	= ( lpObjectTab ) ? fnHashClear ( lpObjectTab ) :
    fnHashAlloc ( 100, sizeof ( LWREFENTRY ) );
  fnLWgetClosure ( lwObject, lpObjectTab );
  PrintArg.lpStream	= lpStream;
  PrintArg.wMaxLevel	= _MAX_LEVEL;
  PrintArg.lpObjTab	= lpObjectTab;
  PrintArg.wPrintId	= 1;
  fputs ( "\n;; ========================================\n", lpStream );
  nReturn		= fnLWdoPut ( &PrintArg, 0, NULL, lwObject );
  fputc ( '\n', lpStream );
  if ( PrintArg.wPrintId > 1 ) {
    fprintf ( lpStream, ";; Statistics:\n"
	      ";;    %lu referenced object(s)\n"
	      ";;    %lu object(s) with 2 or more references\n",
	      (DWORD) fnHashOccupied ( lpObjectTab ),
	      (DWORD) PrintArg.wPrintId - 1 );
  }
#if 0
  fnHashDebug ( lpObjectTab );
#endif
  return nReturn;
} /* fnLWput */

/* ------------------------------------------------------------------------- */
int		fnLWput			( LWOBJECT lwObject )
{
  int	nReturn;
  FILE	FAR * lpStream;

  lpStream	= stdout;
#if 0
  lpStream	= fopen ( "memdump", "w" );
#endif
  nReturn	= fnLWfput ( lwObject, lpStream );
  fflush ( lpStream );
  if ( lpStream != stdout )
    fclose ( lpStream );
  return nReturn;
} /* fnLWput */

/* ------------------------------------------------------------------------- */
LPSTR		fnLWclassToString	( LWCLASS lwClass, LPSTR lpszBuf,
					  size_t nBuf )
{
  static char		szDefaultBuf [ 32 ];

  static CONST char	szUnknown []		= "unknown";
  static CONST LPCSTR	lplpszClassNames []	= {
    "int",
    "cons",
    "symbol",
    "class1",
    "char",
    "mark_catch",
    "mark_special",
    "mark_unbound",
    "mark_slot_unbound",
    "mark_bad_pointer",
    "mark_rest",
    "short_float",
    "tlatter",
    "array",
    "string",
    "bitvector",
    "struct",
    "vector",
    "clos_object",
    "single_float",
    "long_float",
    "long_int",
    "ratio",
    "complex"
  };

  char		szBuf [ 32 ];

  /* If this ASSERT fails, the enum type LWCLASS has been changed in its
     number of elements but lplpszClassNames wasn't changed too: */
  ASSERT ( length ( lplpszClassNames ) == lwMaximum );

  if ( lwClass == lwUnknown ) {
    strcpy ( szBuf, szUnknown );
  } else if ( 0 <= lwClass && lwClass < length ( lplpszClassNames ) ) {
    strcpy ( szBuf, lplpszClassNames [ lwClass ] );
  } else {
    sprintf ( szBuf, "%s#0x%lX", szUnknown, (DWORD) lwClass );
  }
  if ( lpszBuf && nBuf ) {
    strncpy ( lpszBuf, szBuf, nBuf );
    return lpszBuf;
  }
  strncpy ( szDefaultBuf, szBuf, sizeof ( szDefaultBuf ) );
  return szDefaultBuf;
} /* fnLWclassToString */

/* ------------------------------------------------------------------------- */
void		fnLWregisterConst	( LWOBJECT lwObjectConst,
					  LWCONSTOBJ constObj )
{
  switch ( constObj ) {
  case lwNilConst:
    lwNil	= lwObjectConst;
    break;
  case lwTconst:
    lwT		= lwObjectConst;
    break;
  default:
    CERROR (( "Unknown constant object 0x%lX %d to register",
	      (DWORD) lwObjectConst, constObj ));
    break;
  }
} /* fnLWregisterConst */
