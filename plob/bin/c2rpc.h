/* -------------------------------------------------------------------------
| Module	c2rpc.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/17
| Description	Generator macros for RPC .x files
|		The rather strange looking 'token' =|| is replaced by '#'
|		in script c2rpc.
 ------------------------------------------------------------------------- */

#ifndef	C2RPC_H
#define	C2RPC_H

#ifndef	RPC
#define	RPC	1
#endif

#define	glu	@g@	/* Paste left and right token */
#define	nln	@n@	/* Expand to a new line character */

/* Paste 2 .. 5 token into a single token: */
#define	paste2(t1,t2)		t1 glu t2
#define	paste3(t1,t2,t3)	t1 glu t2 glu t3
#define	paste4(t1,t2,t3,t4)	t1 glu t2 glu t3 glu t4
#define	paste5(t1,t2,t3,t4,t5)	t1 glu t2 glu t3 glu t4 glu t5

#define	MARKERLABEL(label) paste3(@,label,@)
#define	MARKER(label) nln MARKERLABEL(label)
#define	ENDMARKER(label) nln paste3(@,end,@) MARKERLABEL(label)

/* Constant declaration: */
#define	DefineConstant(c_const_name,lisp_const_name,	\
		       const_value,const_desc )	\
MARKER(const) const c_const_name = const_value;		\
ENDMARKER(const)

/* Type mapping: */
#define	MapType(from_type,to_type)			\
MARKER(typedef) typedef from_type to_type; ENDMARKER(typedef)
#define	DefineType(from_type,to_type)			\
MARKER(typedef) typedef from_type to_type; ENDMARKER(typedef)

/* Enums: */
#define	BeginEnum(type_name)				\
MARKER(enum) enum type_name {
#define	EndEnum(type_name)				\
}; ENDMARKER(enum)

#define enumerator(c_const_name,lisp_const_name,const_value,const_desc) \
  c_const_name = const_value

/* Structs: */
#define	DefineStruct(struct_name)	MARKER(typedef) struct struct_name {
#define	slot(slot_type,slot_name)	slot_type slot_name
#define	EndStruct(struct_name)		}; ENDMARKER(typedef)

#define	add(x,y)		((x)+(y))
#define	mul(x,y)		((x)*(y))
/* 'and' introduces the next argument: */
#define	and			, nln
#define	bitwise_or(x,y)		(OR(x|y))
#define	hex(number)		(HEX(#number))
#define	shift_left(x,n)		(SHIFTLEFT(x<n))
#define subtract(x,y )		((x)-(y))

#define	voidArguments	voidArgument
#define	voidArgument	void

/* Argument passing conventions (possible values for type_mod of macro
   FUNARG): */
#define	value_out	MARKERLABEL(out)
#define	value_in
#define vector_out	MARKERLABEL(out)
#define	vector_in	value_in

/* Function arguments: */
#define	argument(arg_type,type_mod,arg_name)				\
arg_type type_mod arg_name; nln

/* voidResult for functions with no result: */
#define	voidResult	void

/* Function declaration: */
#define	DefineFunction(result_type,c_function_name,			\
		       lisp_function_name,function_args )		\
MARKER(args) result_type nln c_function_name nln function_args		\
ENDMARKER(args)								\
MARKER(function)							\
result_type c_function_name ( MARKERLABEL(arguments) ) = nln MARKERLABEL(counter) ;\
ENDMARKER(function)

#define	BOOL			bool_t
#define CHAR			char
#define	FIXNUM			int
#define	INTEGER			int
#define	DOUBLE_FLOAT		double
#define	VECTOR(type,size)	paste4(type##_vector_t,<,SIZEOF_##type(size),>)
#define	SINGLE_FLOAT		float
#define	STRING(size)		paste4(string_t,<,size,>)
#define	CONST_STRING		paste4(string_t,<,MARKERLABEL(conststring),>)

%#if RPC_HDR
%#include	<rpc/rpc.h>
%typedef CLIENT * PCLIENT;
%#include	<global.h>
%#endif
%#if RPC_CLNT || RPC_SVC
%#include	<string.h>
%#endif
%#ifndef as_is
%#define	as_is		void
%#endif
%#ifndef ABS
%#define	ABS(x)		(((x)<0)?-(x):(x))
%#endif
%#ifndef MIN
%#define	MIN(x,y)	(((x)<(y))?(x):(y))
%#endif
%#ifndef MAX
%#define	MAX(x,y)	(((x)>(y))?(x):(y))
%#endif
%#ifndef	PASTE
%#define PASTE(token1,token2)	token1##token2
%#endif
%#ifndef	PASTE2
%#define PASTE2(token1,token2)	PASTE(token1,token2)
%#endif
%#ifndef	EMPTY
%#define EMPTY
%#endif
%#ifndef _SVCSUFFIX
%#if LINUX
%#define	_SVCSUFFIX	_svc
%#else
%#define	_SVCSUFFIX	EMPTY
%#endif
%#endif
%#if !defined(DLLEXPORT)
%#if WIN32
%#if (BUILDDLL+0)	/* Create a DLL: */
%#define	DLLEXPORT	__declspec(dllexport) __cdecl
%#else			/* Use a DLL: */
%#define	DLLEXPORT	__declspec(dllimport) __cdecl
%#endif
%#else
%#define	DLLEXPORT
%#endif
%#endif
%#if !defined(DLLEXPORTVAR)
%#if WIN32
%#if (BUILDDLL+0)	/* Create a DLL: */
%#define	DLLEXPORTVAR	__declspec(dllexport)
%#else			/* Use a DLL: */
%#define	DLLEXPORTVAR	__declspec(dllimport)
%#endif
%#else
%#define	DLLEXPORTVAR
%#endif
%#endif
%#if WIN32
%#define	strdup		_strdup
%#endif

DefineType ( opaque, as_is_vector_t<> );
#define	SIZEOF_as_is(size)	(mul(size,sizeof(int)))
DefineType ( opaque, void_vector_t<> );
#define	SIZEOF_void(size)	(mul(size,sizeof(int)))
DefineType ( int, int_vector_t<> );
#define	SIZEOF_int(size)	size
DefineType ( unsigned int, u_int_vector_t<> );
#define	SIZEOF_u_int(size)	size
DefineType ( float, float_vector_t<> );
#define	SIZEOF_float(size)	size
DefineType ( double, double_vector_t<> );
#define	SIZEOF_double(size)	size
DefineType ( string, string_t<> );
#define	SIZEOF_string(size)	size

#endif /* #ifndef C2RPC_H */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
