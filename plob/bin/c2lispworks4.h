/* -------------------------------------------------------------------------
| Module	c2lispworksnt.h
| Author	Heiko Kirschke
|		Heiko.Kirschke@poet.de
| Copyright	(C) 1993-1998 Heiko Kirschke
| Date		1998/04 Created
| Description	Generator macros for LispWorks Windows/NT LISP 4.x.
|		The rather strange looking 'token' =|| is replaced by '#'
|		in script c2lisp.
 ------------------------------------------------------------------------- */

#ifndef C2LISPWORKS4_H
#define C2LISPWORKS4_H

#if ! defined(LISP)
#include	<c2lisp.h>
#endif

#include	<c2lispworks.h>

/* voidResult for functions with no result: */
#define	voidResult		nil

#define AS_IS			keyword_as_is
#define	CHAR			keyword_char
#define	CONST_STRING		keyword_const_c_string
#define	DOUBLE_FLOAT		keyword_double_float
#define	FIXNUM			keyword_int
#define	INTEGER			keyword_int
#define POINTER			keyword_pointer
#define	SINGLE_FLOAT		keyword_lisp_single_float
#define	STRING(size)		keyword_c_string
#define	_VECTOR_as_is		AS_IS
#define	_VECTOR_void		POINTER
#define	_VECTOR_int		POINTER
#define	_VECTOR_u_int		POINTER
#define	VECTOR(type,size)	_VECTOR_##type

#endif /* C2LISPWORKS4_H */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
