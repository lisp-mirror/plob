/* -------------------------------------------------------------------------
| Module	plobsequ.c
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1996/09/23
| Description	PLOB source code common for server and client.
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobintern.h"
#include	"plobmisc.h"
#include	"plobtype.h"
#include	"plobsequ.h"
#include	"plobstruct.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| Module initialization function
 ------------------------------------------------------------------------- */
void		fnInitCommonSequModule	( void )
{
  static CLASSINFO ClassInfoTable []	= {
    { IMMEDIATE_TYPE_INFO ( eshSignedByte2Tag,
			    SZCOMMONLISP, "(signed-byte 2)", 2, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshSignedByte4Tag,
			    SZCOMMONLISP, "(signed-byte 4)", 4, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshSignedByte8Tag,
			    SZCOMMONLISP, "(signed-byte 8)", 8, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshSignedByte16Tag,
			    SZCOMMONLISP, "(signed-byte 16)", 16, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshSignedByte32Tag,
			    SZCOMMONLISP, "(signed-byte 32)", 32, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshUnsignedByte1Tag,
			    SZCOMMONLISP, "(unsigned-byte 1)", 1, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshUnsignedByte2Tag,
			    SZCOMMONLISP, "(unsigned-byte 2)", 2, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshUnsignedByte4Tag,
			    SZCOMMONLISP, "(unsigned-byte 4)", 4, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshUnsignedByte8Tag,
			    SZCOMMONLISP, "(unsigned-byte 8)", 8, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshUnsignedByte16Tag,
			    SZCOMMONLISP, "(unsigned-byte 16)", 16, NULL ) },
    { IMMEDIATE_TYPE_INFO ( eshUnsignedByte32Tag,
			    SZCOMMONLISP, "(unsigned-byte 32)", 32, NULL ) },
    { (SHTYPETAG) eshConsTag,
        SZCOMMONLISP,
	"cons",
	eshConsObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeNoFlags },
    { (SHTYPETAG) eshArrayTag,
        SZCOMMONLISP,
	"array",
	eshArrayObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeVarSizeObjIdP },
    { (SHTYPETAG) eshStringTag,
        SZCOMMONLISP,
	"simple-string",
	eshStringObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeVarSizeValueP },
    { (SHTYPETAG) eshBitVectorTag,
        SZCOMMONLISP,
	"bit-vector",
	eshBitVectorObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeVarSizeValueP },
    { (SHTYPETAG) eshVectorTag,
        SZCOMMONLISP,
	"vector",
	eshVectorObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeVarSizeObjIdP },
    { (SHTYPETAG) eshIVectorTag,
        SZCOMMONLISP,
	"ivector",
	eshIVectorObjIdSize * sizeof ( psint ) * nBitsPerByte,
	0,
	(LPCSTR) NULL,
	typeVarSizeValueP }
  };

  int		i;

  PROCEDURE	( fnInitCommonSequModule );

  /* Register classes: */
  for ( i = 0; i < length ( ClassInfoTable ); i++ ) {
    RegisterPlobClass ( & ClassInfoTable [ i ] );
  }

  RETURN ( VOID );
} /* fnInitCommonSequModule */

/* ----------------------------------------------------------------------- */
void		fnDeinitCommonSequModule	( void )
{
  PROCEDURE	( fnDeinitCommonSequModule );

  RETURN ( VOID );
} /* fnDeinitCommonSequModule */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
