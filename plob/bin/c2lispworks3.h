/* -------------------------------------------------------------------------
| Module	c2lispworks.h
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Copyright	(C) 1993,1994 Heiko Kirschke
| Date		17.12.93
| Description	Generator macros for LispWorks LISP 3.x.
|		The rather strange looking 'token' =|| is replaced by '#'
|		in script c2lisp.
 ------------------------------------------------------------------------- */

#ifndef C2LISPWORKS3_H
#define C2LISPWORKS3_H

#if ! defined(LISP)
#include	<c2lisp.h>
#endif

#include	<c2lispworks.h>

/* voidResult for functions with no result: */
#define	voidResult		nil

#define AS_IS			keyword_as_is
#define	CHAR			keyword_character
#define	CONST_STRING		keyword_string
#define	DOUBLE_FLOAT		keyword_double_float
#define	FIXNUM			keyword_fixnum
#define	INTEGER			keyword_integer
#define POINTER			keyword_integer
#define	SINGLE_FLOAT		keyword_single_float
#define	STRING(size)		keyword_string
#define	_VECTOR_as_is		keyword_as_is
#define	_VECTOR_void		keyword_simple_vector
#define	_VECTOR_int		keyword_simple_vector
#define	_VECTOR_u_int		keyword_simple_vector
#define	VECTOR(type,size)	_VECTOR_##type

#endif /* C2LISPWORKS3_H */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
