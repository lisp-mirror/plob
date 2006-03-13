/* -------------------------------------------------------------------------
| Module	generic.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		26.1.94
| Description	Support for generic functions in C
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
#if defined(LISP)
/* ... */
#elif ! defined(C2C)
#include	"c2c.h"
#endif

/* -------------------------------------------------------------------------
| Generic functions in C
 ------------------------------------------------------------------------- */
#define	DefineGeneric( result_type, c_function_name,			\
		       lisp_function_name, function_args )		\
DefineFunction(result_type,c_function_name,lisp_function_name,function_args)

#define	BeginGeneric( result_type,				\
		      c_function_name, lisp_function_name,	\
		      function_args )				\
BeginFunction (result_type,c_function_name,			\
	       lisp_function_name,function_args )		\
     LPFNMETHOD	lpfnMethod = NULL;

#define	EndGeneric( c_function_name )				\
EndFunction(c_function_name)

#if defined(C2C)

/* -------------------------------------------------------------------------
| 'Classes' are represented to the rest of the world as numeric
| 'class tags'. Associated with a class tag is a class-info; the size
| of the class info has to be equal for all 'classes' registered:
 ------------------------------------------------------------------------- */
typedef int	CLASSTAG, * LPCLASSTAG;

/* -------------------------------------------------------------------------
| Registering of 'classes'; please use RegisterClass,
| not fnRegisterClass:
 ------------------------------------------------------------------------- */
#if WIN32 && defined(RegisterClass)
#undef RegisterClass	/* defined in winuser.h, around line 2662 */
#endif
#define		RegisterClass( nClassTag, lpszClassName, lpClassInfo )	\
fnRegisterClass(__szFile__,__szProc__,__LINE__,				\
		(nClassTag),(lpszClassName),				\
		(lpClassInfo),sizeof(*(lpClassInfo)))
BOOL DLLEXPORT	fnRegisterClass		( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  CLASSTAG nClassTag,
					  LPCSTR lpszClassName,
					  LPVOID lpClassInfo,
					  int nSizeOfClassInfo );

/* -------------------------------------------------------------------------
| Registering of functions; please use RegisterFunction,
| not fnRegisterFunction:
 ------------------------------------------------------------------------- */
#define		RegisterFunction( lpfnFunction )	\
fnRegisterFunction(__szFile__,__szProc__,__LINE__,	\
		   (LPFNVOID)(lpfnFunction),#lpfnFunction)

BOOL DLLEXPORT	fnRegisterFunction	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  LPFNVOID lpfnFunction,
					  LPCSTR lpszFunction );

/* -------------------------------------------------------------------------
| Registering of 'methods'; please use RegisterMethod,
| not fnRegisterMethod:
 ------------------------------------------------------------------------- */
#ifdef __cplusplus
typedef int	( * LPFNGENERIC )	( ... );
typedef int	( * LPFNMETHOD )	( ... );
typedef LPVOID	( * LPFNPMETHOD )	( ... );
#else
typedef int	( * LPFNGENERIC )	();
typedef int	( * LPFNMETHOD )	();
typedef LPVOID	( * LPFNPMETHOD )	();
#endif
#define		RegisterMethod( nClassTag, lpfnGeneric, lpfnMethod )	\
fnRegisterMethod(__szFile__,__szProc__,__LINE__,			\
		 (nClassTag),						\
		 (LPFNGENERIC)(lpfnGeneric),#lpfnGeneric,		\
		 (LPFNMETHOD)(lpfnMethod),#lpfnMethod)

BOOL DLLEXPORT	fnRegisterMethod	( LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine,
					  CLASSTAG nClassTag,
					  LPFNGENERIC lpfnGeneric,
					  LPCSTR lpszGeneric,
					  LPFNMETHOD lpfnMethod,
					  LPCSTR lpszMethod );

/* -------------------------------------------------------------------------
| Mapping class tag -> class name
 ------------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnFindClassName		( CLASSTAG nClassTag );

/* -------------------------------------------------------------------------
| Mapping class tag -> class info
 ------------------------------------------------------------------------- */
extern int	nGlobalInfoCache;
extern LPVOID *	lpGlobalInfoCache /* [ nInfoCache ] */;

extern CLASSTAG	__nClassTagInfo__;

#define		FindClassInfo( nClassTag ) \
(__nClassTagInfo__=(nClassTag),\
 (((int)__nClassTagInfo__<0)?NULL:\
  (((int)__nClassTagInfo__<nGlobalInfoCache&&\
    lpGlobalInfoCache[__nClassTagInfo__]!=NULL)?\
   lpGlobalInfoCache[__nClassTagInfo__]:fnFindClassInfo(__nClassTagInfo__))))

LPVOID DLLEXPORT	fnFindClassInfo		( CLASSTAG nClassTag );

/* -------------------------------------------------------------------------
| Cache entries
 ------------------------------------------------------------------------- */
typedef struct {
  LPVOID	lpKey;
  LPVOID	lpValue;
}	CACHENTRY, FAR * LPCACHENTRY;

/* -------------------------------------------------------------------------
| Mapping string -> function
 ------------------------------------------------------------------------- */
LPFNVOID DLLEXPORT fnFindFunctionByName	( LPCSTR	lpszFunctionName );

/* -------------------------------------------------------------------------
| Mapping class tag -> method
 ------------------------------------------------------------------------- */

extern int		nGlobalMethodCache;
extern LPCACHENTRY	lpGlobalMethodCache /* [ nMethodCache ] */;

extern CLASSTAG		__nClassTagMethod__;

extern CONST char	szMissingPROCEDURE []	/* =
"Missing PROCEDURE macro at begin of function body." */;

#define		_FindMethod( nClassTag, lpfnGeneric ) \
(__nClassTagMethod__=(nClassTag), \
 (__nClassTagMethod__<nGlobalMethodCache&& \
  lpGlobalMethodCache[__nClassTagMethod__].lpKey==(LPVOID)(lpfnGeneric))? \
 (LPFNMETHOD)lpGlobalMethodCache[__nClassTagMethod__].lpValue: \
 fnFindMethod(__nClassTagMethod__,(LPFNGENERIC)(lpfnGeneric)))

/* __fnProc__ is declared by the PROCEDURE-macro from _global.h: */
#define		FindMethod( nClassTag )					\
((__fnProc__)?								\
 _FindMethod(nClassTag,__fnProc__):					\
 (ERROR((szMissingPROCEDURE)),(LPFNMETHOD)NULL))

LPFNMETHOD DLLEXPORT fnFindMethod	( CLASSTAG nClassTag,
					  LPFNGENERIC lpfnGeneric );

/* -------------------------------------------------------------------------
| Enumeration function (called 'map'-function in LISP). The lpfnEnum
| function should return TRUE for continuing the enumeration and FALSE
| for stopping it; fnEnumClassInfo returns with the total number of calls
| to lpfnEnum.
 ------------------------------------------------------------------------- */
/* Class info enumeration function: */
typedef BOOL	( * LPFNENUMCLASSINFO ) ( int nFirstEnumArgument,
					  CLASSTAG nClassTag,
					  LPCSTR lpszClassName,
					  LPVOID lpClassInfo );
int DLLEXPORT	fnEnumClassInfo		( int nFirstEnumArgument,
					  LPFNENUMCLASSINFO lpfnEnum );

BOOL DLLEXPORT	fnClassInfoFirst	( LPCLASSTAG lpnClassTag,
					  LPCSTR * lplpszClassName,
					  LPVOID * lplpClassInfo );
BOOL DLLEXPORT	fnClassInfoNext		( LPCLASSTAG lpnClassTag,
					  LPCSTR * lplpszClassName,
					  LPVOID * lplpClassInfo );

/* ----------------------------------------------------------------------------
| Module initialization / Deinitialization
 --------------------------------------------------------------------------- */
void		fnInitializeGenericModule	( void );
void		fnDeinitializeGenericModule	( void );

#endif /* #if defined(C2C) */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
