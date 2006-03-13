/* -------------------------------------------------------------------------
| Module	splob.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
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

#include	"plob.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
#define	_SVREF_OBJID( oObjId, nRawIndex, eshTypeTag )	\
*fnObjId2ObjPtr((oObjId),(nRawIndex),(SHTYPETAG)(eshTypeTag),	\
		__szFile__,__szProc__,__LINE__)

#define	_VALUE_PTR( Type, oObjId, eshTypeTag )	\
((Type*)fnObjId2ValPtr((oObjId),(eshTypeTag),	\
		       __szFile__,__szProc__,__LINE__))

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */
extern const char	szOptionRestarted []	/* = "-restarted" */;

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
LPCSTR DLLEXPORT	fnTypeString	( SHTYPETAG	nTypeTag );
OBJID DLLEXPORT		fnCreateObject	( SHTYPETAG nTypeTag,
					  FIXNUM nSlots,
					  SHTYPETAG eTypeTagValues,
					  FIXNUM nNumberOfValues );
void DLLEXPORT		fnDestroyObject	( OBJID oObjId,
					  BOOL bKill );

/* -------------------------------------------------------------------------
| Functions used in macros
 ------------------------------------------------------------------------- */
LPOBJID DLLEXPORT	fnObjId2ObjPtr	( OBJID oObjId,
					  psint nRawIndex,
					  SHTYPETAG nTypeTag,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine );
LPVOID DLLEXPORT	fnObjId2ValPtr	( OBJID oObjId,
					  SHTYPETAG nTypeTag,
					  LPCSTR lpszFile,
					  LPCSTR lpszProc,
					  int nLine );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
