/* -------------------------------------------------------------------------
| Module	c2allegro5.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1998/06/04
| Description	Generator macros for Allegro LISP 5.x.
|		The rather strange looking 'token' =|| is replaced by '#'
|		in script c2lisp.
 ------------------------------------------------------------------------- */

#ifndef C2ALLEGRO5_H
#define C2ALLEGRO5_H

#if ! defined(LISP)
#include	<c2lisp.h>
#endif

#include	<c2allegro.h>

/* voidResult for functions with no resultkeyword_ */
#define	voidResult		keyword_void

#define AS_IS			keyword_lisp
#define	CHAR			keyword_char
#define	CONST_STRING		keyword_string
#define	DOUBLE_FLOAT		keyword_double
#define	FIXNUM			keyword_int
#define	INTEGER			keyword_int
#define POINTER			INTEGER
#define	SINGLE_FLOAT		keyword_float
#define	STRING(size)		keyword_string
#define	_VECTOR_as_is		AS_IS
#define	_VECTOR_void		keyword_array
#if 1
#define	_VECTOR_int		POINTER
#define	_VECTOR_u_int		POINTER
#else
#define	_VECTOR_int		keyword_array
#define	_VECTOR_u_int		keyword_array
#endif
#define	VECTOR(type,size)	_VECTOR_##type

#endif /* C2ALLEGRO5_H */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
