/* -------------------------------------------------------------------------
| Module	c2allegro4.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1997/03/04
| Description	Generator macros for Allegro LISP 4.x.
|		The rather strange looking 'token' =|| is replaced by '#'
|		in script c2lisp.
|
| Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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

#ifndef C2ALLEGRO4_H
#define C2ALLEGRO4_H

#if ! defined(LISP)
#include	<c2lisp.h>
#endif

#include	<c2allegro.h>

/* voidResult for functions with no resultkeyword_ */
#define	voidResult		keyword_void

#define AS_IS			keyword_lisp
#define	CHAR			keyword_character
#define	CONST_STRING		keyword_string
#define	DOUBLE_FLOAT		keyword_double_float
#define	FIXNUM			keyword_fixnum
#define	INTEGER			keyword_integer
#define POINTER			FIXNUM
#define	SINGLE_FLOAT		keyword_single_float
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

#endif /* C2ALLEGRO4_H */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
