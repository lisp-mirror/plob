/* ---------------------------------------------------------------------------
| Module	global.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		22.10.92
| Description	Defines some common C stuff.
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
 -------------------------------------------------------------------------- */

#ifndef	_GLOBAL_H_INCLUDED
#define	_GLOBAL_H_INCLUDED

#if	WIN32
#include	<windows.h>
#endif

#include	<stdio.h>	/* FILE */
#include	<sys/types.h>	/* caddr_t */
#include	<setjmp.h>

#if !defined(DLLEXPORT)
#if WIN32

#if (BUILDDLL+0)
/* Create a DLL: */
#define	DLLEXPORT	__declspec(dllexport) __cdecl
#else
/* Use a DLL: */
#define	DLLEXPORT	__declspec(dllimport) __cdecl
#endif

#else

#define	DLLEXPORT

#endif
#endif

#if !defined(DLLEXPORTVAR)
#if WIN32

#if (BUILDDLL+0)
/* Create a DLL: */
#define	DLLEXPORTVAR	__declspec(dllexport)
#else
/* Use a DLL: */
#define	DLLEXPORTVAR	__declspec(dllimport)
#endif

#else

#define	DLLEXPORTVAR

#endif
#endif

#ifdef	FALSE
#undef	FALSE
#endif	/* #ifdef	FALSE */
#ifdef	TRUE
#undef	TRUE
#endif	/* #ifdef	TRUE */
#ifdef	NULL
#undef	NULL
#endif	/* #ifdef	NULL */

/* ----------------------------------------------------------------------------
| Constants, types
 --------------------------------------------------------------------------- */
#define	CONST			const
#ifndef FAR
#define	FAR
#endif
#define	NULL			((LPVOID)0)

#ifndef	MAX_FNAME
enum {
  max_fname	= 256
};
#define	MAX_FNAME max_fname
#endif	/* #ifdef MAX_FNAME */

#define		UNREADABLE_OBJECT_PREFIX	"#<"
#define		UNREADABLE_OBJECT_SUFFIX	">"

extern DLLEXPORTVAR const char	szEmpty []		/* = "" */;
extern DLLEXPORTVAR const char	szAt []			/* = "@" */;
extern DLLEXPORTVAR const char	szNULL []		/* = "NULL" */;
extern DLLEXPORTVAR const char	szSpace []		/* = " " */;
extern DLLEXPORTVAR const char	szStdIn []		/* = "stdin" */;
extern DLLEXPORTVAR const char	szStdOut []		/* = "stdout" */;
extern DLLEXPORTVAR const char	szStdErr []		/* = "stderr" */;
extern DLLEXPORTVAR const char	szTimeFormat []		/* = "%Y/%m/%d %T" */;

/* Format strings for fopen(); these one take care to open the stream
   always in binary mode. */
extern DLLEXPORTVAR const char	szStreamAppend []	/* = "a" */;
extern DLLEXPORTVAR const char	szStreamAppendRead []	/* = "a+" */;
extern DLLEXPORTVAR const char	szStreamRead []		/* = "r" */;
extern DLLEXPORTVAR const char	szStreamReadWrite []	/* = "r+" */;
extern DLLEXPORTVAR const char	szStreamWrite []	/* = "w" */;
extern DLLEXPORTVAR const char	szStreamWriteTruncate []/* = "w+" */;

/* An enum type for compare results.  Unfortunately, the values given
   here must reflect those of the enum type COMPARETAG in plobtype.h */
typedef enum {
  cmpLessEqual		= -2,
  cmpLess		= -1,
  cmpEqual		=  0,
  cmpGreater		=  1,
  cmpGreaterEqual	=  2
}	COMPARE, * LPCOMPARE;

#if WIN32

#define caddr_t LPVOID

#else

typedef char *		LPSTR;
typedef const char *	LPCSTR;

typedef void *		LPVOID;
typedef const void *	LPCVOID;

typedef int *		LPINT;
typedef int		( * LPFNINT ) ();

typedef long		LONG, * LPLONG;
typedef unsigned char	BYTE, * LPBYTE;
typedef unsigned short	DBYTE, * LPDBYTE;
typedef unsigned int	WORD, * LPWORD;
typedef unsigned long	DWORD, * LPDWORD;
typedef unsigned long	ULONG, * LPULONG;

#endif

typedef void		( * LPFNVOID ) ();

typedef FILE *		LPFILE;

typedef enum { FALSE, TRUE }	BOOL, * LPBOOL;
extern DLLEXPORTVAR LPCSTR	szBool2Verb [ 2 ]	/* = */
/* { "failed", "succeeded" } */;
#define	Bool2Verb( b )		szBool2Verb[((BOOL)(b))!=FALSE]

/* ----------------------------------------------------------------------------
| Macros
 --------------------------------------------------------------------------- */
#if WIN32
#define sleep( seconds )	Sleep ( (seconds)*1000 )
#endif
#define	length( array )		(sizeof(array)/sizeof(array[0]))
#define	MIN( x, y )		(((x)<(y))?(x):(y))
#define	MAX( x, y )		(((x)>(y))?(x):(y))
#define	ABS( x )		((x)>0?(x):-(x))
#define SGN( x )		(((x)>0)?1:(((x)<0)?-1:0))
#define	OFFSET( StructType, StructComponent )	\
((int)(&((StructType*)0)->StructComponent))

#ifndef	NODEBUG

#define	_ASSERT( expr, szMsg )	((expr)?(TRUE):\
				 (_fnGlobalAssertFailed(__szFile__,__szProc__,\
							__LINE__,(szMsg)),\
				  FALSE))
#define	ASSERT( expr )		_ASSERT ( expr, #expr )

#else

#define	_ASSERT( expr, szMsg )	TRUE
#define	ASSERT(expr)		_ASSERT ( expr, NULL )

#endif	/* #ifndef NODEBUG */

#ifdef	__DATE__
#define	_COMPILE_DATE	" compiled " __DATE__
#else
#define	_COMPILE_DATE
#endif /* #ifdef DATE */

#ifdef	__TIME__
#define	_COMPILE_TIME	" " __TIME__
#else
#define	_COMPILE_TIME
#endif /* #ifdef DATE */

#define	MODULE( szModule )					\
static const char __szFile__ []		= szModule;		\
static const char __szSccsId__ []	= "@(#) " szModule	\
_COMPILE_DATE _COMPILE_TIME

extern DLLEXPORTVAR const char	__szProcEntering__ []	/* = */
/* "%s(%d): Entering %s ...\n" */;
extern DLLEXPORTVAR const char	__szProcLeaving__ []	/* = */
/* "%s(%d): Leaving %s\n" */;
extern DLLEXPORTVAR int		__nStackLevel__		/* = 0 */;

extern DLLEXPORTVAR int	nGlobalFlagWord			/* = 0 */;
#define		GetFlagWord()			nGlobalFlagWord

/* 1998/07/16 HK: Debug: */
#if 0

#define	_PROCEDURE( Proc, szProc ) \
static const LPFNVOID __fnProc__	= (LPFNVOID) Proc; \
static const char __szProc__ []		= szProc; \
if ((GetFlagWord()&2)!=0&&strstr(__szProc__,"OneLock")==NULL) { \
  char szMessage [ 256 ]; \
  sprintf ( szMessage, __szProcEntering__, \
	    __nStackLevel__++, __szFile__, __LINE__, __szProc__ ); \
  /* fputs ( szMessage, stdout ); */ \
  /* fflush ( stdout ); */ \
  INFO (( szMessage )); \
} else { \
  __nStackLevel__++; \
}
#define	RETURN( expr ) \
{ \
  if ((GetFlagWord()&2)!=0&&strstr(__szProc__,"OneLock")==NULL) { \
    char szMessage [ 256 ]; \
    sprintf ( szMessage, __szProcLeaving__, \
	      --__nStackLevel__, __szFile__, __LINE__, __szProc__ ); \
    /* fputs ( szMessage, stdout ); */ \
    /* fflush ( stdout ); */ \
    INFO (( szMessage )); \
  } else { \
    --__nStackLevel__; \
  } \
  return expr; \
}

#elif 0

#include	<malloc.h>
#include	<unistd.h>
extern DLLEXPORTVAR const char	__szMallocedBytes__ []	/* = */
/* "Allocated %d bytes." */;
extern DLLEXPORTVAR const char	__szFreedBytes__ []	/* = */
/* "Freed %d bytes." */;
extern DLLEXPORTVAR const char	__szBrkIncreased__ []	/* = */
/* "Brk increased by %d bytes." */;
#define	_PROCEDURE( Proc, szProc )				\
static const LPFNVOID __fnProc__	= (LPFNVOID) Proc;	\
static const char __szProc__ []		= szProc;		\
struct mallinfo __MallInfoEntry__	= mallinfo ();		\
void * __BrkEntry__			= sbrk ( 0 )
#define	RETURN( expr )						\
{								\
  struct mallinfo __MallInfoExit__	= mallinfo ();		\
  int	__nAllocated__	=					\
    ( __MallInfoExit__.usmblks + __MallInfoExit__.uordblks ) -	\
    ( __MallInfoEntry__.usmblks + __MallInfoEntry__.uordblks );	\
  void * __BrkExit__			= sbrk ( 0 );		\
  int	__nBrkIncrease__		=			\
    (int) ( __BrkExit__ - __BrkEntry__ );			\
  if ( __nAllocated__ < 0 ) {					\
    LOG (( __szFreedBytes__, -__nAllocated__ ));		\
  } else if ( __nAllocated__ > 0 ) {				\
    LOG (( __szMallocedBytes__, __nAllocated__ ));		\
  }								\
  if ( __nBrkIncrease__ > 4096 ) {				\
    LOG (( __szBrkIncreased__, __nBrkIncrease__ ));		\
  }								\
  return expr;							\
}

#else

#define	_PROCEDURE( Proc, szProc )				\
static const LPFNVOID __fnProc__	= (LPFNVOID) Proc;	\
static const char __szProc__ []		= szProc
#define	RETURN( expr )			return expr

#endif

#define	PROCEDURE( Proc )	_PROCEDURE ( Proc, #Proc )
#if defined(VOID)
#undef VOID	/* in Windows/NT defined in winnt.h, around line 71 */
/* also defined on IRIX */
#endif
#define	VOID

/* Write a record to the log file: */
#define	_LOG( File, Proc, Line, Format )	\
     (__lpszErrorFile__=(LPCSTR)(File),\
      __lpszErrorProc__=(LPCSTR)(Proc),\
      __nErrorLine__=(Line),\
      _fnGlobalLog Format)

#define	LOG( Format )	_LOG ( __szFile__, __szProc__, __LINE__, Format )

/* An enum type for errror severities. Unfortunately, the values given
   here must reflect those of the enum type ERRLVL in plob.h */
typedef enum {
  errNone,	/* 0: No error was encountered. */
  errInfo,	/* 1: Information message. */
  errWarn,	/* 2: Warning message. */
  errCError,	/* 3: Continuable error. */
  errError,	/* 4: Error occurred. */
  errFatal,	/* 5: Fatal error was encountered. */
  errMin	= errNone,
  errMax	= errFatal,
  errNoE	= errMax - errMin + 1
} ERRORLEVEL, * PERRORLEVEL;

extern DLLEXPORTVAR LPCSTR	ppszErrorLevel2String [ errNoE ]
  /* = { "none", "info", "warning",
         "continuable error", "error", "fatal error" } */;

/* Show an info: */
#define	_INFO( File, Proc, Line, Format )	\
     (__lpszErrorFile__=(LPCSTR)(File),\
      __lpszErrorProc__=(LPCSTR)(Proc),\
      __eErrorLevel__=errInfo,\
      __nErrorLine__=(Line),\
      _fnGlobalError Format)
#define	INFO( Format )	_INFO ( __szFile__, __szProc__, __LINE__, \
				Format )

/* Show a warning: */
#define	_WARN( File, Proc, Line, Format )	\
     (__lpszErrorFile__=(LPCSTR)(File),\
      __lpszErrorProc__=(LPCSTR)(Proc),\
      __eErrorLevel__=errWarn,\
      __nErrorLine__=(Line),\
      _fnGlobalError Format)
#define	WARN( Format )	_WARN ( __szFile__, __szProc__, __LINE__, \
				Format )

/* Show a continuable error: */
#define	_CERROR( File, Proc, Line, Format )	\
     (__lpszErrorFile__=(LPCSTR)(File),\
      __lpszErrorProc__=(LPCSTR)(Proc),\
      __eErrorLevel__=errCError,\
      __nErrorLine__=(Line),\
      _fnGlobalCError Format)
#define	CERROR( Format )	_CERROR ( __szFile__, __szProc__, __LINE__, \
					  Format )

/* Raise an error. Program execution continues at CATCHERROR, if
   called before the error occurred; otherwise the current process
   will exit: */
#define	_ERROR( File, Proc, Line, Format )	\
     (__lpszErrorFile__=(LPCSTR)(File),\
      __lpszErrorProc__=(LPCSTR)(Proc),\
      __eErrorLevel__=errError,\
      __nErrorLine__=(Line),\
      _fnGlobalError Format)
#if WIN32 && defined(ERROR)
#undef ERROR	/* defined in wingdi.h, around line 89 */
#endif
#define	ERROR( Format )	_ERROR ( __szFile__, __szProc__, __LINE__, \
				 Format )

/* Raise a fatal error. Program execution continues at CATCHERROR, if
   called before the error occurred; otherwise the current process
   will exit: */
#define	_FATAL( File, Proc, Line, Format )	\
     (__lpszErrorFile__=(LPCSTR)(File),\
      __lpszErrorProc__=(LPCSTR)(Proc),\
      __eErrorLevel__=errFatal,\
      __nErrorLine__=(Line),\
      _fnGlobalError Format)
#define	FATAL( Format )	_FATAL ( __szFile__, __szProc__, __LINE__, \
				 Format )

/* Catch any error raised by ASSERT or ERROR. Example:
   void foo ( void ) {
     ERROR (( "Don't call foo!" ));
   }
   void baz ( void ) {
     if ( CATCHERROR ) {
       ... do some error processing here ...
       return;
     }
     foo ();
   }
*/
#define	CATCHERROR		(__bJmpBufErrorValid__=(BOOL)\
				 (__nJmpBufError__=\
				  (setjmp(__jmpbufError__))==0),\
				 !__bJmpBufErrorValid__)
#define	UNCATCHERROR		(__bJmpBufErrorValid__=(BOOL)FALSE)
#define THROWERROR(code)	((__bJmpBufErrorValid__)?\
				 (__bJmpBufErrorValid__=FALSE,\
				  longjmp(__jmpbufError__,code),\
				  TRUE):\
				 FALSE)


/* ----------------------------------------------------------------------------
| Function protoypes
 --------------------------------------------------------------------------- */
extern DLLEXPORTVAR int			__nJmpBufError__;
extern DLLEXPORTVAR jmp_buf		__jmpbufError__;
extern DLLEXPORTVAR BOOL		__bJmpBufErrorValid__;
extern DLLEXPORTVAR const char *	__szProc__	/* = szEmpty */;
extern DLLEXPORTVAR const LPFNVOID	__fnProc__	/* = NULL */;

typedef int	( * LPFNERROR )		( ERRORLEVEL	eLevel,
					  LPCSTR	lpszContinue,
					  LPCSTR	lpszErrorMsg );

/* Set error handler: */
BOOL DLLEXPORT	fnGlobalSetErrorHandler	( LPFNERROR lpfnError );

BOOL DLLEXPORT	_fnGlobalAssertFailed	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  ... );
extern DLLEXPORTVAR LPCSTR		__lpszErrorFile__;
extern DLLEXPORTVAR LPCSTR		__lpszErrorProc__;
extern DLLEXPORTVAR ERRORLEVEL		__eErrorLevel__;
extern DLLEXPORTVAR int 		__nErrorLine__;

LPFILE DLLEXPORT fnLogOpen		( void );
void DLLEXPORT	fnLogClose		( void );
LPCSTR DLLEXPORT fnLogSetDirectory	( LPCSTR	lpszDirectory );
void DLLEXPORT	fnLogNextVersion	( void );

typedef LPSTR	( * PFNMKLOGPROMPT )	( LPSTR lpszBuffer,
					  size_t nBuffer );
void DLLEXPORT	fnSetFnMkLogPrompt	( PFNMKLOGPROMPT pfnMkLogPrompt );

int DLLEXPORT	_fnGlobalLog		( LPCSTR lpszFormat, ... );

int DLLEXPORT	_fnGlobalCError		( LPCSTR lpszContinue,
					  LPCSTR lpszFormat, ... );
int DLLEXPORT	_fnGlobalError		( LPCSTR lpszFormat, ... );

int DLLEXPORT	fnGlobalErrorHandler	( ERRORLEVEL	eLevel,
					  LPCSTR	lpszContinue,
					  LPCSTR	lpszErrorMsg );

LPCSTR DLLEXPORT fnGetHost		( void );
LPCSTR DLLEXPORT fnGetUser		( void );

int DLLEXPORT	fnStrCmp		( LPCSTR lpszString1,
					  LPCSTR lpszString2 );
#define		STRCMP( s1, s2 )	fnStrCmp(s1,s2)

int DLLEXPORT	strnncmp		( LPCSTR lpszString1,
					  size_t nMaxString1,
					  LPCSTR lpszString2,
					  size_t nMaxString2 );

int DLLEXPORT	strnnicmp		( LPCSTR lpszString1,
					  size_t nMaxString1,
					  LPCSTR lpszString2,
					  size_t nMaxString2 );

LPSTR DLLEXPORT	fnStrLwr		( LPSTR lpszToLower );
#define		STRLWR( s )		fnStrLwr(s)

LPSTR DLLEXPORT	fnStrUpr		( LPSTR lpszToUpper );
#define		STRUPR( s )		fnStrUpr(s)

int DLLEXPORT	fnStrICmp		( LPCSTR lpszString1,
					  LPCSTR lpszString2 );
#define		STRICMP( s1, s2 )	fnStrICmp(s1,s2)

int DLLEXPORT	fnStrNICmp		( LPCSTR lpszString1,
					  LPCSTR lpszString2,
					  size_t nMaxString );
#define		STRNICMP( s1, s2, n )	fnStrNICmp(s1,s2,n)

/* ----------------------------------------------------------------------------
| Module initialization / Deinitialization
 --------------------------------------------------------------------------- */
extern DLLEXPORTVAR BOOL	__bInitializeCommon__		/* = TRUE */;
#if ! defined(INITIALIZECOMMON)
void		fnInitializeCommon	( void );
#define	INITIALIZECOMMON		((__bInitializeCommon__)?\
					 (fnInitializeCommon(),TRUE):FALSE)
#endif	/* ! INITIALIZECOMMON */

extern DLLEXPORTVAR BOOL	__bDeinitializeCommon__		/* = FALSE */;
void		fnDeinitializeCommon	( void );

void		fnInitializeGlobalModule	( void );
void		fnDeinitializeGlobalModule	( void );

#endif /* #ifndef _GLOBAL_H_INCLUDED */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
