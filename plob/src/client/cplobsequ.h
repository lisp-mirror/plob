/* -------------------------------------------------------------------------
| Module	cplobsequ.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Copyright	(C) 1996 Heiko Kirschke
| Date		1996/09/23
| Description	PLOB client header file:
|		Macros and functions for usage by the PLOB client
 ------------------------------------------------------------------------- */

#include	"plobsequ.h"

/* -------------------------------------------------------------------------
| String
 ------------------------------------------------------------------------- */
#define	string_ptr( oObjId )			\
fnStringPtr((oObjId),__szFile__,__szProc__,__LINE__)

/* -------------------------------------------------------------------------
| Bit-vector
 ------------------------------------------------------------------------- */
#define	bit_vector_ptr( oObjId )					\
_VALUE_PTR ( unsigned char, oObjId, eshBitVectorTag )

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
LPSTR		fnStringPtr		( OBJID oObjId,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
