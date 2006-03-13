/* -------------------------------------------------------------------------
| Module	c2c.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		17.12.93
| Description	Generator macros for C.
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

#ifndef	C2C
#define	C2C	1

/* Constant declaration: */
#define	DefineConstant( c_const_name, lisp_const_name,	\
		        const_value, const_desc )	\
enum { c_const_name = const_value }

/* Type mapping: */
#define	MapType( from_type, to_type )
#define	DefineType( base_type, define_type )		\
typedef base_type define_type

/* Enums */
#define	BeginEnum( type_name )		typedef enum {
#define	EndEnum( type_name )		} type_name, * P##type_name
#define enumerator( c_const_name, lisp_const_name,	\
		    const_value, const_desc )		\
c_const_name = const_value

/* Structs: */
#define	DefineStruct( struct_name )			\
typedef struct struct_name##TAG {
#define	slot( slot_type, slot_name )			\
slot_type slot_name
#define	EndStruct( struct_name )			\
}	struct_name, * P##struct_name

/* Function arguments: */
#define	argument( arg_type, type_mod, arg_name )	\
arg_type type_mod arg_name
#define	voidResult	void
/* noArguments for functions with no arguments: */
#define	voidArgument	void
#define	voidArguments	voidArgument

/* Argument passing conventions (possible values for type_mod of macro
   FUNARG): */
#define	value_out	*
#define	value_in
#define	vector_out
#define	vector_in	value_in

/* Function declaration: */

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

#define	DefineFunction( result_type,				\
		        c_function_name, lisp_function_name,	\
		        function_args )				\
result_type DLLEXPORT c_function_name function_args

#define	DefineCallable( result_type,				\
		        c_function_name, lisp_function_name,	\
		        function_args )				\
DefineFunction ( result_type,					\
		 c_function_name, lisp_function_name,		\
		 function_args )

#ifdef NOEXCEPTION

/* Disable exception handling. */
#define	C2C_TRY_EXCEPTION
#define	C2C_CATCH_EXCEPTION

#else

#define	C2C_TRY_EXCEPTION	TRY_EXCEPTION
#define	C2C_CATCH_EXCEPTION	CATCH_EXCEPTION

#endif

#define	BeginFunction( result_type,				\
		       c_function_name, lisp_function_name,	\
		       function_args )				\
DefineFunction ( result_type, c_function_name, 			\
		 lisp_function_name, function_args )		\
{								\
  static const char __szCProc__ []	= #c_function_name;	\
  _PROCEDURE ( c_function_name,					\
	       #c_function_name " (#'" lisp_function_name ")" );\
  C2C_TRY_EXCEPTION;
#define	EndFunction( c_function_name )				\
  C2C_CATCH_EXCEPTION;						\
}

#define	add( x, y )		((x)+(y))
#define	mul( x, y )		((x)*(y))
/* 'and' introduces the next argument: */
#define	and			,
#define	bitwise_or( x, y )	((x)|(y))
#define	hex( number )		0x##number
#define	oct( number )		0##number
#define	shift_left( x, n )	((x)<<(n))
#define subtract( x, y )	((x)-(y))

typedef char			CHAR;
#define	as_is			void
typedef int			FIXNUM, INTEGER;
typedef double			DOUBLE_FLOAT;
#define	VECTOR(type,size)	type *
typedef void *			POINTER;
typedef float			SINGLE_FLOAT;
#define	STRING(size)		char *
typedef const char *		CONST_STRING;

#endif

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
