/* -------------------------------------------------------------------------
| Module	generic.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		26.1.94
| Description	Support for generic functions in C
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
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Extern variables
 ------------------------------------------------------------------------- */
const char	szMissingPROCEDURE []	=
"Missing PROCEDURE macro at begin of function body.";

int		nGlobalInfoCache		= 0;

LPVOID FAR *	lpGlobalInfoCache /* [ nGlobalInfoCache ] */	= 
(LPVOID FAR *) NULL;

CLASSTAG	__nClassTagInfo__;

int		nGlobalMethodCache		= 0;

LPCACHENTRY	lpGlobalMethodCache /* [ nGlobalMethodCache ] */ =
(LPCACHENTRY) NULL;

CLASSTAG	__nClassTagMethod__;

/* -------------------------------------------------------------------------
| Macros, types & constants
 ------------------------------------------------------------------------- */
enum {
  nSizeMin	=  128,
  nSizeMax	= 1024
};

#define	INITIALIZE_CLASS_TABLE( lpTable, nSizeOfClassInfo ) \
(((lpTable)->pClasses!=NULL)? \
 (lpTable): \
 fnInitializeClassTable(lpTable,nSizeOfClassInfo))

#define GCLASSINFO( nSizeOfClassInfo )					\
struct {								\
  CLASSTAG	nClassTag;						\
  LPCSTR	lpszClassName;						\
  HASHTABLE	Methods;						\
  char		UserClassInfo [ nSizeOfClassInfo ];			\
}
typedef	GCLASSINFO ( 4 )	FAR * LPGCLASSINFO;

typedef struct {
  int		nSizeOfClassInfo;
  LPHASHTABLE	pClasses;
  LPHASHTABLE	pFunctions;	/* functions pointers -> function names */
}	CLASSTABLE, FAR * LPCLASSTABLE;

typedef struct {
  LPFNVOID	lpfnFunction;	/* function code pointer */
  LPCSTR	lpszFunction;	/* function name */
}	FUNCTIONINFO, FAR * LPFUNCTIONINFO;

typedef struct {
  LPFNGENERIC	lpfnGeneric;
  LPFNMETHOD	lpfnMethod;
}	METHODINFO, FAR * LPMETHODINFO;

typedef struct {
  int			nCalled;
  int			nFirstEnumArgumentUser;
  int			nSizeOfClassInfo;
  LPFNENUMCLASSINFO	lpfnEnumUser;
}	ENUMGCLASSINFO, FAR * LPENUMGCLASSINFO;

/* -------------------------------------------------------------------------
| static variables
 ------------------------------------------------------------------------- */
static CLASSTABLE	GlobalClassTable;

/* -------------------------------------------------------------------------
| static function declarations
 ------------------------------------------------------------------------- */
static LPCLASSTABLE	fnInitializeClassTable	( LPCLASSTABLE lpTable,
						  int nSizeOfClassInfo );
/* Type info enumeration callback which calls the user callback: */
static HASHENUM		fnEnumClassInfoCallback	( LPHASHTABLE lpHashTable,
						  HASHKEY dwHashKey,
						  LPVOID lpDataField,
						  size_t nSizeOfDataField,
						  LPVOID lpUserData );

/* -------------------------------------------------------------------------
| Static functions
 ------------------------------------------------------------------------- */
static LPCLASSTABLE	fnInitializeClassTable	( LPCLASSTABLE lpTable,
						  int nSizeOfClassInfo )
{
  PROCEDURE	( fnInitializeClassTable );

  if ( lpTable->pClasses == NULL && nSizeOfClassInfo >= 0 ) {
    lpTable->nSizeOfClassInfo	= nSizeOfClassInfo;
    lpTable->pClasses	=
      HashCreate ( NULL, 16,
		   nSizeOfClassInfo + sizeof ( * (LPGCLASSINFO) NULL ) - 4 );
  }

  if ( lpTable->pFunctions == NULL ) {
    lpTable->pFunctions	= HashCreate ( NULL, 16, sizeof ( FUNCTIONINFO ) );
  }

  RETURN ( lpTable );
} /* fnInitializeHashTable */

/* ----------------------------------------------------------------------- */
static HASHENUM		fnEnumClassInfoCallback	( LPHASHTABLE lpHashTable,
						  HASHKEY dwHashKey,
						  LPVOID lpDataField,
						  size_t nSizeOfDataField,
						  LPVOID lpUserData )
{
  PROCEDURE	( fnEnumClassInfoCallback );

  if ( ( * ((LPENUMGCLASSINFO)lpUserData)->lpfnEnumUser )
       ( ((LPENUMGCLASSINFO)lpUserData)->nFirstEnumArgumentUser,
	 ((LPGCLASSINFO)lpDataField)->nClassTag,
	 ((LPGCLASSINFO)lpDataField)->lpszClassName,
	 ( ((LPENUMGCLASSINFO)lpUserData)->nSizeOfClassInfo > 0 ) ?
	 ((LPGCLASSINFO)lpDataField)->UserClassInfo : NULL ) ) {
    ((LPENUMGCLASSINFO)lpUserData)->nCalled++;
    RETURN ( hashContinue );
  }
  RETURN ( hashBreak );
} /* fnEnumClassInfoCallback */

/* -------------------------------------------------------------------------
| Extern functions
 ------------------------------------------------------------------------- */
BOOL DLLEXPORT	fnRegisterFunction	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  LPFNVOID lpfnFunction,
					  LPCSTR lpszFunction )
{
  LPFUNCTIONINFO	lpFunctionInfo;
  FUNCTIONINFO		FunctionInfo;

  PROCEDURE	( fnRegisterFunction );
  INITIALIZECOMMON;
  INITIALIZE_CLASS_TABLE ( &GlobalClassTable, -1 );

  /* Search for function info: */
  lpFunctionInfo	= (LPFUNCTIONINFO)
    HashGet ( GlobalClassTable.pFunctions, (HASHKEY) lpfnFunction );
  if ( lpFunctionInfo ) {
    if ( strcmp ( lpFunctionInfo->lpszFunction, lpszFunction ) ) {
      _CERROR ( lpszFile, lpszProc, nLine,
	        ( "Use 1st name for function.",
		  "Function at address 0x%X was already registered with"
		  " name %s; now received name %s.", lpfnFunction,
		 lpFunctionInfo->lpszFunction, lpszFunction ) );
    } else {
      FunctionInfo.lpfnFunction	= lpfnFunction;
    }
  } else {
    /* Insert the generic info into the hash table: */
    FunctionInfo.lpfnFunction	= lpfnFunction;
    FunctionInfo.lpszFunction	= strdup ( lpszFunction );
    HashInsert ( GlobalClassTable.pFunctions,
		 (HASHKEY) lpfnFunction, &FunctionInfo );
  }
  RETURN ( TRUE );
} /* fnRegisterFunction */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT	fnRegisterClass		( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  CLASSTAG nClassTag,
					  LPCSTR lpszClassName,
					  LPVOID lpClassInfo,
					  int nSizeOfClassInfo )
{
  LPGCLASSINFO	lpInfo;

  PROCEDURE	( fnRegisterClass );
  INITIALIZECOMMON;
  INITIALIZE_CLASS_TABLE ( &GlobalClassTable, nSizeOfClassInfo );

  ASSERT ( lpszClassName != (LPCSTR) NULL && *lpszClassName != '\0' );

  lpInfo	= (LPGCLASSINFO)
    HashGet ( GlobalClassTable.pClasses, nClassTag );
  if ( lpInfo ) {
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Don't register second class.",
	        "Class tag %d used twice: for class %s and class %s.",
	        nClassTag, lpInfo->lpszClassName, lpszClassName ) );
    RETURN ( FALSE );
  }

  if ( lpClassInfo == NULL && nSizeOfClassInfo != 0 ) {
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Set size to 0.",
	        "Passed NULL class info pointer but"
	        " ( sizeof ( class info ) == %d ) != 0.",
	        nSizeOfClassInfo ) );
    nSizeOfClassInfo	= 0;
  }
  if ( nSizeOfClassInfo != GlobalClassTable.nSizeOfClassInfo ) {
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Use second value.",
	        "Specified invalid sizeof ( class info ) == %d; expected"
	        " value %d.",
	        nSizeOfClassInfo, GlobalClassTable.nSizeOfClassInfo ) );
	        
    nSizeOfClassInfo	= GlobalClassTable.nSizeOfClassInfo;
  }
  if ( nSizeOfClassInfo == 0 )
    lpClassInfo	= NULL;

  /* 1996/09/11 HK */
  /* lpInfo		= */
  /*   malloc ( sizeof ( GCLASSINFO ( nSizeOfClassInfo ) ) ); */
  lpInfo		= (LPGCLASSINFO)
    Malloc ( sizeof ( * (LPGCLASSINFO) NULL  ) + nSizeOfClassInfo - 4 );

  lpInfo->nClassTag	= nClassTag;
  lpInfo->lpszClassName	= strdup ( lpszClassName );
  HashCreate ( &lpInfo->Methods, 16, sizeof ( METHODINFO ) );
  if ( lpClassInfo != NULL ) {
    memcpy ( lpInfo->UserClassInfo, lpClassInfo, nSizeOfClassInfo );
  }
  HashInsert ( GlobalClassTable.pClasses, nClassTag, lpInfo );

  Free ( lpInfo );
  lpInfo		= (LPGCLASSINFO) NULL;

  RETURN ( TRUE );
} /* fnRegisterClass */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT	fnRegisterMethod	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  CLASSTAG nClassTag,
					  LPFNGENERIC lpfnGeneric,
					  LPCSTR lpszGeneric,
					  LPFNMETHOD lpfnMethod,
					  LPCSTR lpszMethod )
{
  LPGCLASSINFO		lpClassInfo;
  LPMETHODINFO		lpMethodInfo;
  METHODINFO		MethodInfo;
  LPFUNCTIONINFO	lpFunctionInfo;

  PROCEDURE		( fnRegisterMethod );
  INITIALIZECOMMON;

  ASSERT ( lpfnGeneric != NULL );
  ASSERT ( lpszGeneric != (LPCSTR) NULL && *lpszGeneric != '\0' );
  ASSERT ( lpfnMethod != NULL );
  ASSERT ( lpszMethod != (LPCSTR) NULL && *lpszMethod != '\0' );

  lpClassInfo	= (LPGCLASSINFO) NULL;
  if ( GlobalClassTable.pClasses != NULL ) {
    lpClassInfo	= (LPGCLASSINFO)
      HashGet ( GlobalClassTable.pClasses, nClassTag );
  }
  if ( lpClassInfo == NULL ) {
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Don't register generic function and method.",
	        "Can't register method %s\n"
		"       to generic function %s because\n"
	        "       class with tag %d is not yet registered.",
		lpszMethod, lpszGeneric, nClassTag ) );
    RETURN ( FALSE );
  }

  /* Search for method info: */
  lpMethodInfo	= (LPMETHODINFO)
    HashGet ( &lpClassInfo->Methods, (HASHKEY) lpfnGeneric );
  if ( lpMethodInfo != NULL ) {
    lpFunctionInfo	= (LPFUNCTIONINFO)
      HashGet ( GlobalClassTable.pFunctions,
		(HASHKEY) lpMethodInfo->lpfnMethod );
    _CERROR ( lpszFile, lpszProc, nLine,
	      ( "Ignore registering method.",
	        "There is already method %s registered for generic"
	        " function %s in class %s.",
	        lpFunctionInfo->lpszFunction, lpszGeneric,
	        lpClassInfo->lpszClassName ) );
    RETURN ( FALSE );
  }
  if ( fnRegisterFunction ( lpszFile, lpszProc, nLine,
			    (LPFNVOID) lpfnGeneric, lpszGeneric ) &&
       fnRegisterFunction ( lpszFile, lpszProc, nLine,
			    (LPFNVOID) lpfnMethod, lpszMethod ) ) {
    /* Insert the method info into the hash table: */
    MethodInfo.lpfnGeneric	= lpfnGeneric;
    MethodInfo.lpfnMethod	= lpfnMethod;
    HashInsert ( &lpClassInfo->Methods, (HASHKEY) lpfnGeneric, &MethodInfo );
  }
  RETURN ( TRUE );
} /* fnRegisterMethod */

/* ----------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnFindClassName		( CLASSTAG nClassTag )
{
  LPGCLASSINFO	lpClassInfo;

  PROCEDURE	( fnFindClassName );
  INITIALIZECOMMON;

  lpClassInfo	= (LPGCLASSINFO) NULL;
  if ( GlobalClassTable.pClasses != NULL ) {
    lpClassInfo	= (LPGCLASSINFO)
      HashGet ( GlobalClassTable.pClasses, nClassTag );
  }
  if ( lpClassInfo != NULL ) {
    RETURN ( lpClassInfo->lpszClassName );
  }
  RETURN ( (LPCSTR) NULL );
} /* fnFindClassName */

/* ----------------------------------------------------------------------- */
LPVOID DLLEXPORT	fnFindClassInfo		( CLASSTAG nClassTag )
{
  int		nNewSize;
  LPVOID	lpUserClassInfo;
  LPGCLASSINFO	lpClassInfo;

  PROCEDURE	( fnFindClassInfo );
  INITIALIZECOMMON;

  if ( (int) nClassTag < 0 ) {
    RETURN ( NULL );
  }

  if ( (int) nClassTag < nGlobalInfoCache &&
       lpGlobalInfoCache [ nClassTag ] != NULL ) {
    RETURN ( lpGlobalInfoCache [ nClassTag ] );
  }

  if ( nGlobalInfoCache < nClassTag && nClassTag < nSizeMax ) {
    /* Enlarge the info cache: */
    nNewSize		= nClassTag + nSizeMin;
    lpGlobalInfoCache	= ( lpGlobalInfoCache != NULL ) ?
      (LPVOID FAR *) ReAlloc ( lpGlobalInfoCache,
			       nNewSize  * sizeof ( LPVOID ) ) :
      (LPVOID FAR *) Malloc ( nNewSize  * sizeof ( LPVOID ) );
    /* Set new elements to 0: */
    memset ( & lpGlobalInfoCache [ nGlobalInfoCache ], 0,
	     ( nNewSize - nGlobalInfoCache ) * sizeof ( LPVOID ) );
    nGlobalInfoCache	= nNewSize;
  }

  lpClassInfo		= (LPGCLASSINFO) NULL;
  lpUserClassInfo	= NULL;
  if ( GlobalClassTable.pClasses != NULL &&
       GlobalClassTable.nSizeOfClassInfo > 0 ) {
    lpClassInfo	= (LPGCLASSINFO)
      HashGet ( GlobalClassTable.pClasses, nClassTag );
    if ( lpClassInfo != NULL ) {
      lpUserClassInfo	= lpClassInfo->UserClassInfo;
    }
  }
  if ( nClassTag < nGlobalInfoCache ) {
    /* Put class info into the cache: */
    lpGlobalInfoCache [ nClassTag ]	= lpUserClassInfo;
  }

  RETURN ( lpUserClassInfo );
} /* fnFindClassInfo */

/* ----------------------------------------------------------------------- */
typedef struct {
  LPCSTR	lpszName;
  LPFNVOID	lpfnFunction;
}	ENUMFUNC, FAR * LPENUMFUNC;

/* ----------------------------------------------------------------------- */
static HASHENUM	fnEnumFunctionCallback	( LPHASHTABLE	lpHashTable,
					  HASHKEY	dwHashKey,
					  LPVOID	lpDataField,
					  size_t	nSizeOfDataField,
					  LPVOID	lpUserData )
{
  PROCEDURE	( fnEnumFunctionCallback );

  if ( strcmp ( ((LPFUNCTIONINFO)lpUserData)->lpszFunction,
		((LPFUNCTIONINFO)lpDataField)->lpszFunction ) != 0 ) {
    RETURN ( hashContinue );
  }

  ((LPFUNCTIONINFO)lpUserData)->lpfnFunction	=
    ((LPFUNCTIONINFO)lpDataField)->lpfnFunction;

  RETURN ( hashBreak );
} /* fnEnumFunctionCallback */

/* ----------------------------------------------------------------------- */
LPFNVOID DLLEXPORT fnFindFunctionByName	( LPCSTR	lpszFunctionName )
{
  FUNCTIONINFO	EnumFunc;

  PROCEDURE	( fnFindFunctionByName );
  INITIALIZECOMMON;
  INITIALIZE_CLASS_TABLE ( &GlobalClassTable, -1 );

  EnumFunc.lpszFunction	= lpszFunctionName;
  EnumFunc.lpfnFunction	= (LPFNVOID) NULL;
  fnHashEnum ( GlobalClassTable.pFunctions, fnEnumFunctionCallback,
	       &EnumFunc );

  RETURN ( EnumFunc.lpfnFunction );
} /* fnFindFunctionByName */

/* ----------------------------------------------------------------------- */
LPFNMETHOD DLLEXPORT fnFindMethod	( CLASSTAG nClassTag,
					  LPFNGENERIC lpfnGeneric )
{
  int		nNewSize;
  LPGCLASSINFO	lpClassInfo;
  LPFNMETHOD	lpfnMethod;
  LPMETHODINFO	lpMethodInfo;

  PROCEDURE	( fnFindMethod );
  INITIALIZECOMMON;

  ASSERT ( lpfnGeneric != NULL );

  if ( nClassTag < nGlobalMethodCache &&
       lpGlobalMethodCache [ nClassTag ].lpKey == (LPVOID) lpfnGeneric ) {
    RETURN ( (LPFNMETHOD) lpGlobalMethodCache [ nClassTag ].lpValue );
  }

  if ( nGlobalMethodCache < nClassTag && nClassTag < nSizeMax ) {
    /* Enlarge the method cache: */
    nNewSize		= nClassTag + nSizeMin;
    lpGlobalMethodCache	= ( lpGlobalMethodCache != NULL ) ?
      (LPCACHENTRY) ReAlloc ( lpGlobalMethodCache,
			      nNewSize  * sizeof ( CACHENTRY ) ) :
      (LPCACHENTRY) Malloc ( nNewSize  * sizeof ( CACHENTRY ) );
    /* Set new elements to 0: */
    memset ( & lpGlobalMethodCache [ nGlobalMethodCache ], 0,
	     ( nNewSize - nGlobalMethodCache ) * sizeof ( CACHENTRY ) );
    nGlobalMethodCache	= nNewSize;
  }

  lpClassInfo	= (LPGCLASSINFO) NULL;
  lpfnMethod	= (LPFNMETHOD) NULL;
  if ( GlobalClassTable.pClasses != NULL ) {
    lpClassInfo	= (LPGCLASSINFO)
      HashGet ( GlobalClassTable.pClasses, nClassTag );
  }
  if ( lpClassInfo != NULL ) {
    lpMethodInfo	= (LPMETHODINFO)
      HashGet ( &lpClassInfo->Methods, (HASHKEY) lpfnGeneric );
    if ( lpMethodInfo != NULL ) {
      lpfnMethod	= lpMethodInfo->lpfnMethod;
    }
  }

  if ( nClassTag < nGlobalMethodCache ) {
    /* Put generic function and found method into the cache: */
    lpGlobalMethodCache [ nClassTag ].lpKey	= (LPVOID) lpfnGeneric;
    lpGlobalMethodCache [ nClassTag ].lpValue	= (LPVOID) lpfnMethod;
  }

  RETURN ( lpfnMethod );
} /* fnFindMethod */

/* ----------------------------------------------------------------------- */
int DLLEXPORT	fnEnumClassInfo		( int nFirstEnumArgument,
					  LPFNENUMCLASSINFO lpfnEnum )
{
  int			nCalled;
  ENUMGCLASSINFO	EnumClassInfo;

  PROCEDURE	( fnEnumClassInfo );
  INITIALIZECOMMON;

  nCalled	= 0;
  if ( GlobalClassTable.pClasses != NULL ) {
    EnumClassInfo.nCalled			= 0;
    EnumClassInfo.nFirstEnumArgumentUser	= nFirstEnumArgument;
    EnumClassInfo.lpfnEnumUser			= lpfnEnum;
    EnumClassInfo.nSizeOfClassInfo		=
      GlobalClassTable.nSizeOfClassInfo;
    fnHashEnum ( GlobalClassTable.pClasses, fnEnumClassInfoCallback,
		 &EnumClassInfo );
    nCalled					= EnumClassInfo.nCalled;
  }
  RETURN ( nCalled );
} /* fnEnumClassInfo */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT	fnClassInfoFirst	( LPCLASSTAG lpnClassTag,
					  LPCSTR FAR * lplpszClassName,
					  LPVOID FAR * lplpClassInfo )
{
  LPGCLASSINFO	lpClassInfo;

  PROCEDURE	( fnClassInfoFirst );
  INITIALIZECOMMON;

  if ( fnHashFirst ( GlobalClassTable.pClasses,
		     (LPHASHKEY) NULL, (LPVOID FAR *) &lpClassInfo,
		     (size_t *) NULL ) ) {
    if ( lpnClassTag != NULL ) {
      *lpnClassTag	= lpClassInfo->nClassTag;
    }
    if ( lplpszClassName != NULL ) {
      *lplpszClassName	= lpClassInfo->lpszClassName;
    }
    if ( lplpClassInfo != NULL ) {
      *lplpClassInfo	= ( GlobalClassTable.nSizeOfClassInfo > 0 ) ?
	&lpClassInfo->UserClassInfo : NULL;
    }
    RETURN ( TRUE );
  }
  fnHashLast ( GlobalClassTable.pClasses );
  RETURN ( FALSE );
} /* fnClassInfoFirst */

/* ----------------------------------------------------------------------- */
BOOL DLLEXPORT	fnClassInfoNext		( LPCLASSTAG lpnClassTag,
					  LPCSTR FAR * lplpszClassName,
					  LPVOID FAR * lplpClassInfo )
{
  LPGCLASSINFO	lpClassInfo;

  PROCEDURE	( fnClassInfoNext );
  INITIALIZECOMMON;

  if ( fnHashNext ( GlobalClassTable.pClasses,
		    (LPHASHKEY) NULL, (LPVOID FAR *) &lpClassInfo,
		    (size_t *) NULL ) ) {
    if ( lpnClassTag != NULL ) {
      *lpnClassTag	= lpClassInfo->nClassTag;
    }
    if ( lplpszClassName != NULL ) {
      *lplpszClassName	= lpClassInfo->lpszClassName;
    }
    if ( lplpClassInfo != NULL ) {
      *lplpClassInfo	= ( GlobalClassTable.nSizeOfClassInfo > 0 ) ?
	&lpClassInfo->UserClassInfo : NULL;
    }
    RETURN ( TRUE );
  }
  fnHashLast ( GlobalClassTable.pClasses );
  RETURN ( FALSE );
} /* fnClassInfoNext */

/* ----------------------------------------------------------------------------
| Module initialization / Deinitialization
 --------------------------------------------------------------------------- */
void		fnInitializeGenericModule	( void )
{
  PROCEDURE	( fnInitializeGenericModule );
  RETURN ( VOID );
} /* fnInitializeGenericModule */

/* ------------------------------------------------------------------------- */
void		fnDeinitializeGenericModule	( void )
{
  BOOL		bMapped;
  LPGCLASSINFO	pInfo = (LPGCLASSINFO) NULL;

  PROCEDURE	( fnDeinitializeGenericModule );

  if ( GlobalClassTable.pClasses != NULL ) {
    fnHashLast ( GlobalClassTable.pClasses );
    for ( bMapped = fnHashFirst ( GlobalClassTable.pClasses, (LPHASHKEY) NULL,
				  (LPVOID FAR *) &pInfo, (size_t *) NULL );
	  bMapped;
	  bMapped = fnHashNext ( GlobalClassTable.pClasses, (LPHASHKEY) NULL,
				 (LPVOID FAR *) &pInfo, (size_t *) NULL ) ) {
      ASSERT ( pInfo != NULL );
      fnHashDestroy ( &pInfo->Methods );
    }
    fnHashDestroy ( GlobalClassTable.pClasses );
  }

  fnHashDestroy ( GlobalClassTable.pFunctions );

  if ( lpGlobalInfoCache != NULL ) {
    Free ( lpGlobalInfoCache );
    lpGlobalInfoCache	= (LPVOID FAR *) NULL;
  }
  nGlobalInfoCache	= 0;

  if ( lpGlobalMethodCache != NULL ) {
    Free ( lpGlobalMethodCache );
    lpGlobalMethodCache	= (LPCACHENTRY) NULL;
  }
  nGlobalMethodCache	= 0;

  RETURN ( VOID );
} /* fnDeinitializeGenericModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
