/* -------------------------------------------------------------------------
| Module	splobsequ.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
|
| Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
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
LPSTR DLLEXPORT		fnStringPtr	( OBJID oObjId,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/