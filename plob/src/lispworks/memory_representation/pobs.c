/* -------------------------------------------------------------------------
| Module	pobs.c
| Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		09.11.93
| Description	
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<malloc.h>

#include	"_global.h"
#include	"_hash.h"

#include	"lwobject.h"
#include	"pobs.h"

MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern opaque types
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Static constants, types and variables
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Extern function prototypes
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Function	fnLISPcons
| Arguments	  
|		
| Return	A new cons
| Description	
 ------------------------------------------------------------------------- */
LWOBJECT	fnLISPcons		( LWOBJECT lwCar,
					  LWOBJECT lwCdr );

/* -------------------------------------------------------------------------
| Function	fnLISPallocSymbol
| Arguments	  
|		
| Return	A new symbol
| Description	
 ------------------------------------------------------------------------- */
LWOBJECT	fnLISPallocSymbol	( LWOBJECT lwsName,
					  LWOBJECT lwnGeneration );

/* -------------------------------------------------------------------------
| Function	fnLISPallocObject
| Arguments	  
|		
| Return	A new object
| Description	
 ------------------------------------------------------------------------- */
LWOBJECT	fnLISPallocObject	( LWOBJECT lwnSizeInBytes,
					  DWORD dwType,
					  LWOBJECT lwnGeneration );

/* -------------------------------------------------------------------------
| Static function prototypes
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Function	
| Arguments	  
|		
| Return	
| Description	
 ------------------------------------------------------------------------- */
static 	fn		(  );

/* -------------------------------------------------------------------------
| Static function bodies
 ------------------------------------------------------------------------- */
static 	fn		(  )
{
} /* fn */

/* -------------------------------------------------------------------------
| Extern function bodies
 ------------------------------------------------------------------------- */
void		fnPOBSinit		( LWOBJECT lwNil )
{
  fnLWregisterConst ( lwNil, lwNilConst );
} /* fnPOBSinit */

/* ----------------------------------------------------------------------- */
LWOBJECT	fnPOBSstoreObject	( LWOBJECT lwObjectToStore )
{
  fputs ( "Hier ist fnPOBSstoreObject\n", stdout );
  return 0;
} /* fnPOBSstoreObject */

/* ----------------------------------------------------------------------- */
LWOBJECT	fnPOBSstartTrace	( LWOBJECT lwSymbolAllocObject,
					  LWOBJECT lwSymbolMyAllocObject )
{
  puts ( "start-trace" );
  fflush ( stdout );
  LWSYMSTRUCTPTR ( lwSymbolAllocObject )->lwCode	=
    LWSYMSTRUCTPTR ( lwSymbolMyAllocObject )->lwCode;
  LWSYMSTRUCTPTR ( lwSymbolAllocObject )->lwConstant	=
    LWSYMSTRUCTPTR ( lwSymbolMyAllocObject )->lwConstant;
  return 0;
} /* fnPOBSstartTrace */

/* ----------------------------------------------------------------------- */
LWOBJECT	fnPOBStraceAlloc	( LWOBJECT lwnSize, LWOBJECT lwArg2,
					  LWOBJECT lwnArg3 )
{
  static FILE	FAR * lpStream = NULL;
  LWOBJECT	lwResult;

  puts ( "fnPOBStraceAlloc: Start." );
  fflush ( stdout );

  if ( ! lpStream )
    lpStream	= fopen ( "alloc_trace", "a" );
  fprintf ( lpStream, "fnPOBStraceAlloc:"
	    " lwnSize = %d, Arg2 = 0x%X, Arg 3 = %d\n",
	    lwnSize, lwArg2, lwnArg3 );
  fflush ( lpStream );
  lwResult	= fnLISPallocObject ( lwnSize, lwArg2, lwnArg3 );
  fprintf ( lpStream, " result = 0x%X\n", lwResult );
  fflush ( lpStream );
  return lwResult;
} /* fnPOBStraceAlloc */

/* ----------------------------------------------------------------------- */
LWOBJECT	fnPOBSloadObject	( LWOBJECT lwnObjId )
{
  static CONST DWORD	dwTypeString = 0x00000006;

  LWOBJECT	lwAllocated, lwObject;
  int		nSize;

  lwObject	= lwNil;
  nSize	= ( sizeof ( DWORD ) +
	    sizeof ( ((LPLWCLASS1STRUCT)NULL)->IsA.String ) -
	    sizeof ( ((LPLWCLASS1STRUCT)NULL)->IsA.String.sData ) + 4 );
  lwAllocated	= fnLISPallocObject ( nSize << 2, dwTypeString, 3 << 2 );
  lwObject	= lwAllocated | 3;
  fprintf ( stdout,
	    "Allocated size %d, cell address 0x%X, object address 0x%X"
	    ", mark 0x%X\n",
	    nSize, lwAllocated, lwObject,
	    LWCLASS1STRUCTPTR ( lwObject, lwUnknown )->dwMark );
  LWCLASS1STRUCTPTR ( lwObject, lwUnknown )->dwMark			=
    0x80000066;
  LWCLASS1STRUCTPTR ( lwObject, lwUnknown )->IsA.String.lwnLength	=
    3 << 2;
  memcpy ( LWCLASS1STRUCTPTR ( lwObject, lwUnknown )->IsA.String.sData,
	   "fooo", 4 );

  return lwObject;
} /* fnPOBSloadObject */
