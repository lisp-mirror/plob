/* -------------------------------------------------------------------------
| Module	c2lisp.h
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Copyright	(C) 1993,1994 Heiko Kirschke
| Date		17.12.93
| Description	Generator macros for LISP.
 ------------------------------------------------------------------------- */

#ifndef	C2LISP_H
#define	C2LISP_H

;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on __DATE__ __TIME__
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------

#ifndef	LISP
#define	LISP	1
#endif

#define	GLU	@g@		/* Paste left and right token */
#define	NLN	@n@		/* Expand to a new line character */
#define HSH	@h@		/* Hash character `#' */
#define SPC	@s@		/* Space character ` ' */
#define SPC2	SPC SPC		/* 2 space characters `  ' */
#define SPC4	SPC2 SPC2	/* 4 space characters `    ' */
#define	NLI	NLN SPC2	/* Newline and indent 2 spaces */
#define	NLII	NLN SPC4	/* Newline and indent 4 spaces */

/* Paste 2 .. 5 token into a single token: */
#define	PASTE2(t1,t2)		t1 GLU t2
#define	PASTE3(t1,t2,t3)	t1 GLU t2 GLU t3
#define	PASTE4(t1,t2,t3,t4)	t1 GLU t2 GLU t3 GLU t4
#define	PASTE5(t1,t2,t3,t4,t5)	t1 GLU t2 GLU t3 GLU t4 GLU t5

/* Stringinize: */
#define	STRINGINIZE3( token ) #token
#define	STRINGINIZE2( token ) STRINGINIZE3(token)
#define	STRINGINIZE( token ) STRINGINIZE2(token)

/* Single characters: */
#define BO ( GLU
#define BC GLU )
#define DASH GLU - GLU
#define READ_TIME_EVAL(form) HSH GLU . GLU form
#define KEYWORD(symbol)	: GLU symbol

/* Put brackets around expr: */
#define BR(expr) BO expr BC

/* Some Common LISP commands: */

#define _clrhash(table) BR(clrhash table)

#define define_foreign_callable(result_type,c_function_name,lisp_function_name,function_args) \
NLN \
BR(define DASH foreign DASH callable NLI result_type NLI c_function_name NLI lisp_function_name NLI function_args)

#define define_foreign_function(result_type,c_function_name,lisp_function_name,function_args) \
NLN \
BR(define DASH foreign DASH function NLI result_type NLI c_function_name NLI lisp_function_name NLI function_args)

#define _defvar(name,init,doc) BR(defvar NLI name NLI init NLI doc)

#define _defconstant(name,init,doc) BR(defconstant NLI name NLI init NLI doc)

#define eval_when(cond,form) BR(eval DASH when NLI BO cond BC NLI form)

#define _gethash(key,table) BR(gethash key table)

#define in_package(name) NLN BR(in DASH package name)

#define make_hash_table(args) BR(make DASH hash DASH table args)

#define parse_integer(args) BR(parse DASH integer args)

#define _quote(expr) BR(quote expr)

#define read_from_string(args) BR(read DASH from DASH string args)

#define _setf(place,value) BR(setf place value)

#define _when(cond,forms) BR(when NLI cond NLI forms)

/* Some Common LISP symbols: */
#define keyword_array KEYWORD(array)
#define keyword_as_is KEYWORD(as DASH is)
#define keyword_c_string KEYWORD(c DASH string)
#define keyword_char KEYWORD(char)
#define keyword_character KEYWORD(character)
#define keyword_compile_toplevel KEYWORD(compile DASH toplevel)
#define keyword_const_c_string KEYWORD(const DASH c DASH string)
#define keyword_double KEYWORD(double)
#define keyword_double_float KEYWORD(double DASH float)
#define keyword_execute KEYWORD(execute)
#define keyword_fixnum KEYWORD(fixnum)
#define keyword_float KEYWORD(float)
#define keyword_int KEYWORD(int)
#define keyword_integer KEYWORD(integer)
#define keyword_load_toplevel KEYWORD(load DASH toplevel)
#define keyword_lisp KEYWORD(lisp)
#define keyword_lisp_single_float KEYWORD(lisp DASH single DASH float)
#define keyword_pointer KEYWORD(pointer)
#define keyword_radix KEYWORD(radix)
#define keyword_simple_vector KEYWORD(simple DASH vector)
#define keyword_single_float KEYWORD(single DASH float)
#define keyword_string KEYWORD(string)
#define keyword_test KEYWORD(test)
#define keyword_value_out KEYWORD(value DASH out)
#define keyword_value_in KEYWORD(value DASH in)
#define keyword_vector_out KEYWORD(vector DASH out)
#define keyword_vector_in KEYWORD(vector DASH in)
#define keyword_void KEYWORD(void)

/* Some Common LISP variables: */
#define last_enum_hash_table last DASH enum DASH hash DASH table

#define	_EVAL(expr)\
NLN \
eval_when(keyword_compile_toplevel keyword_load_toplevel keyword_execute,\
	  expr)

/* Constant declaration: */
#define	DefineConstant(c_const_name,lisp_const_name,const_value,const_desc)\
_EVAL(_defconstant(READ_TIME_EVAL(read_from_string(lisp_const_name)),\
		   const_value,\
		   const_desc))

/* Type mapping: */
#define	MapType(from_type,to_type) \
_EVAL(_defconstant(C2L_##to_type,\
		   C2L_##from_type,\
		   "Constant defined by C macro MapType."))
#define	DefineType(from_type,to_type) \
_EVAL(_defconstant(C2L_##to_type,\
		   C2L_##from_type,\
		   "Constant defined by C macro DefineType."))

/* Enums: */
#if 1

/* C enums with doc strings in a hash table: */
#define	DOCBEGINENUM(type_name)\
NLN \
BO eval DASH when BO keyword_compile_toplevel keyword_load_toplevel \
                     keyword_execute BC NLI \
  BO progn NLI \
    _defvar(PASTE3(*,##type_name##,*),\
            make_hash_table(keyword_test _quote(equal)),\
	    "Variable defined by C macro BeginEnum.") NLI \
    _clrhash(PASTE3(*,##type_name##,*)) NLI \
    BO let BO BO last_enum_hash_table PASTE3(*,##type_name##,*) BC BC
#define	DOCENDENUM(type_name) \
BC BC BC NLN
#define	DOCENUMERATOR(const_value,const_desc) \
_setf(_gethash(const_value,last_enum_hash_table),const_desc) NLI

#else

/* C enums without doc strings in a hash table: */
#define	DOCBEGINENUM(type_name)
#define	DOCENDENUM(type_name)
#define	DOCENUMERATOR(const_value,const_descr)

#endif

#define	BeginEnum(type_name) \
DOCBEGINENUM (type_name)
#define	EndEnum(type_name) \
DOCENDENUM (type_name) \
_EVAL(_defconstant(C2L_##type_name,\
		   FIXNUM,\
		   "Constant defined by C macro EndEnum."))
#define enumerator(c_const_name,lisp_const_name,const_value,const_desc) \
DOCENUMERATOR(const_value,const_desc) \
DefineConstant(c_const_name,lisp_const_name,const_value,const_desc)

#define add(x,y)		BR(+ x y)
#define mul(x,y)		BR(* x y)
#define	bitwise_or(x,y)		BR(logior x y)
#define	hex(number)		parse_integer(#number keyword_radix 16)
#define	shift_left(x,n)		BR(ash x n)
#define subtract(x,y)		BR(- x y)

#define	voidArguments		voidArgument
#define	C2L_AS_IS		AS_IS
#define	C2L_CHAR		CHAR
#define	C2L_CONST_STRING	CONST_STRING
#define	C2L_DOUBLE_FLOAT	DOUBLE_FLOAT
#define	C2L_FIXNUM		FIXNUM
#define	C2L_INTEGER		INTEGER
#define	C2L_POINTER		POINTER
#define	C2L_SINGLE_FLOAT	SINGLE_FLOAT
#define	C2L_STRING		STRING
#define	C2L_VECTOR		VECTOR
#define	C2L_VOID_VECTOR		VOID_VECTOR
#define	C2L_voidResult		voidResult

/* Argument passing conventions (possible values for type_mod of macro
   argument): */
#define	value_out		keyword_value_out
#define	value_in		keyword_value_in
#define	vector_out		keyword_vector_out
#define	vector_in		keyword_vector_in
/* 'and' introduces the next argument: */
#define	and
/* Functions with no arguments: */
#define	voidArgument

/* Function arguments: */
#define	argument(arg_type,type_mod,arg_name) \
BR(arg_name READ_TIME_EVAL(C2L_##arg_type) type_mod) NLII
/* voidArguments for functions with no arguments: */

#define	DefineFunction(result_type,c_function_name,lisp_function_name,function_args) \
define_foreign_function(READ_TIME_EVAL(C2L_##result_type),\
			#c_function_name,\
			READ_TIME_EVAL(read_from_string(lisp_function_name)),\
			function_args)

#define	DefineCallable(result_type,c_function_name,lisp_function_name,function_args) \
define_foreign_callable(READ_TIME_EVAL(C2L_##result_type),\
			#c_function_name,\
			READ_TIME_EVAL(read_from_string(lisp_function_name)),\
			function_args)

#if defined(LISPWORKS3)
#include	<c2lispworks3.h>
#elif defined(LISPWORKS4)
#include	<c2lispworks4.h>
#elif defined(ALLEGRO4)
#include	<c2allegro4.h>
#elif defined(ALLEGRO5)
#include	<c2allegro5.h>
#elif defined(another_lisp_system)
#include	<c2another_lisp_system.h>
#else
#error Missing target LISP system.
#endif

#ifdef PACKAGE
in_package(PACKAGE)
#endif

#endif /* #ifndef C2LISP_H */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/