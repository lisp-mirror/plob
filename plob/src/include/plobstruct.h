/* -------------------------------------------------------------------------
| Module	plobstruct.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		9.3.94
| Description	PLOB structure functions.
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

#if defined(LISP)
;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plobstruct.h
;;;; --------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif
#if defined(C2C)

/* ----------------------------------------------------------------------- */
/* Symbol names */
extern const char	szNIL []	/* = "NIL" */;
extern const char	szT []		/* = "T" */;
extern const char	szREAD []	/* = "READ" */;
extern const char	szWRITE []	/* = "WRITE" */;
extern const char	szREADWRITE []	/* = "READ-WRITE" */;
extern const char	szPACKAGE []	/* = "PACKAGE" */;

/* Package names */
#define			SZCOMMONLISP	"COMMON-LISP"
extern const char	szCOMMONLISP []	/* = SZCOMMONLISP */;
#define			SZKEYWORD	"KEYWORD"
extern const char	szKEYWORD []	/* = SZKEYWORD */;
#define			SZPLOB		"PLOB"
extern const char	szPLOB []	/* = SZPLOB */;

void			fnInitCommonStructModule	( void );
void			fnInitializeStructModule	( void );
void			fnDeinitializeStructModule	( void );
void			fnDeinitCommonStructModule	( void );

#endif

#include	"plobconst.h"

/* -------------------------------------------------------------------------
| Structure
 ------------------------------------------------------------------------- */

DefineConstant ( eshStructureTag, "+structure-type-tag+", hex ( 38 ),
		 "Type tag for plob objects of type STRUCTURE." );

BeginEnum ( SHSTRUCTIDX )
  enumerator ( eshStructIdxDesc, "+structure-location-description+", 0,
	       "Index of structure description field.\
 This field references a structure description (which is itself a\
 structure) describing the structure." )
  and
  enumerator ( eshStructObjIdSize, "+structure-size+", 0,
	       "Size of of plob structure cell in words." )
EndEnum ( SHSTRUCTIDX );

/*  Predefined structure: structure-description */
BeginEnum ( SHSTRUCTDESCIDX )
  enumerator ( eshStructDescrIdxName,
	       "+structure-description-location-name+",
	       add ( STRUCTIDXOFFSET, 0 ),
	       "Index of structure description name field." )
  and
  enumerator ( eshStructDescrIdxVersion,
	       "+structure-description-location-version-number+",
	       add ( STRUCTIDXOFFSET, 1 ),
	       "Index of structure description field with a version number." )
  and
  enumerator ( eshStructDescrIdxTimeStamp,
	       "+structure-description-location-time-stamp+",
	       add ( STRUCTIDXOFFSET, 2 ),
	       "Index of structure description field with a time stamp\
 in Common LISP Universal Time reduced to minutes, i.e. the time stamp\
 contains a Common LISP Universal Time divided by 60." )
  and
  enumerator ( eshStructDescrIdxSchemaEvolution,
	       "+structure-description-location-schema-evolution+",
	       add ( STRUCTIDXOFFSET, 3 ),
	       "Index of structure description field which contains the\
 kind of schema evolution." )
  and
  enumerator ( eshStructDescrIdxNextGen,
	       "+structure-description-location-next-generation+",
	       add ( STRUCTIDXOFFSET, 4 ),
	       "Index of structure description field with reference to\
 description of the next generation." )
  and
  enumerator ( eshStructDescrIdxConstructor,
	       "+structure-description-location-constructor+",
	       add ( STRUCTIDXOFFSET, 5 ),
	       "Index of structure description field with name of\
 constructor function." )
  and
  enumerator ( eshStructDescrIdxDependent,
	       "+structure-description-location-dependent+",
	       add ( STRUCTIDXOFFSET, 6 ),
	       "Index of structure description field with the\
 dependent flag." )
  and
  enumerator ( eshStructDescrIdxPNSlots,
	       "+structure-description-location-persistent-slot-numbers+",
	       add ( STRUCTIDXOFFSET, 7 ),
	       "Index of structure description field with the number of\
 persistent slots." )
  and
  enumerator ( eshStructDescrIdxNSlots,
	       "+structure-description-location-slot-numbers+",
	       add ( STRUCTIDXOFFSET, 8 ),
	       "Index of structure description field with the total number of\
 slots." )
  and
  enumerator ( eshStructDescrIdxSlots,
	       "+structure-description-location-slot-descriptions+",
	       add ( STRUCTIDXOFFSET, 9 ),
	       "Index of structure description slot description list." )
  and
  enumerator ( eshStructDescSize,
	       "+structure-description-size+",
	       add ( STRUCTIDXOFFSET, 10 ),
	       "Size of of plob structure slot description in words." )
EndEnum ( SHSTRUCTDESCIDX );

/* Predefined structure: structure-slot-description */
BeginEnum ( SHSTRUCTSLOTDESCIDX )
  enumerator ( eshStructSlotDescrIdxName,
	       "+structure-slot-description-location-name+",
	       add ( STRUCTIDXOFFSET, 0 ),
	       "Index of structure slot description name field." )
  and
  enumerator ( eshStructSlotDescrInitArg,
	       "+structure-slot-description-location-initarg+",
	       add ( STRUCTIDXOFFSET, 1 ),
	       "Index of structure slot description initarg field." )
  and
  enumerator ( eshStructSlotDescrIdxReader,
	       "+structure-slot-description-location-reader+",
	       add ( STRUCTIDXOFFSET, 2 ),
	       "Index of structure slot description reader field." )
  and
  enumerator ( eshStructSlotDescrIdxLocation,
	       "+structure-slot-description-location-location+",
	       add ( STRUCTIDXOFFSET, 3 ),
	       "Index of structure slot description location field." )
  and
  enumerator ( eshStructSlotDescrIdxInit,
	       "+structure-slot-description-location-init+",
	       add ( STRUCTIDXOFFSET, 4 ),
	       "Index of structure slot description init field." )
  and
  enumerator ( eshStructSlotDescrIdxType,
	       "+structure-slot-description-location-type+",
	       add ( STRUCTIDXOFFSET, 5 ),
	       "Index of structure slot description type field." )
  and
  enumerator ( eshStructSlotDescrIdxExtent,
	       "+structure-slot-description-location-extent+",
	       add ( STRUCTIDXOFFSET, 6 ),
	       "Index of structure slot description extent field." )
  and
  enumerator ( eshStructSlotDescrIdxDeferred,
	       "+structure-slot-description-location-deferred+",
	       add ( STRUCTIDXOFFSET, 7 ),
	       "Index of structure slot description deferred field." )
  and
  enumerator ( eshStructSlotDescSize,
	       "+structure-slot-description-size+",
	       add ( STRUCTIDXOFFSET, 8 ),
	       "Size of of plob structure slot description in words." )
EndEnum ( SHSTRUCTSLOTDESCIDX );

/* Predefined structure: package */
BeginEnum ( SHPACKAGEIDX )
  enumerator ( eshPackageIdxName, "+package-location-name+",
	       add ( STRUCTIDXOFFSET, 0 ),
	       "Index of package name field." )
  and
  enumerator ( eshPackageIdxInternals, "+package-location-internals+",
	       add ( STRUCTIDXOFFSET, 1 ),
	       "Index of package internal symbol table field." )
  and
  enumerator ( eshPackageIdxExternals, "+package-location-externals+",
	       add ( STRUCTIDXOFFSET, 2 ),
	       "Index of package external symbol table field." )
  and
  enumerator ( eshPackageSize, "+package-size+",
	       add ( STRUCTIDXOFFSET, 3 ),
	       "Size of of plob package in words." )
EndEnum ( SHPACKAGEIDX );

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerDbCreateStructures, "c-sh-create-structures",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in,
			      oShortObjIdStructDescr )
		   and
		   argument ( FIXNUM, value_in, nObjIds )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_out, pObjIds )
		   and
		   argument ( u_int, value_out, pnSlots ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientDbCreateStructure, "c-sh-create-structure",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in,
			      oShortObjIdStructDescr ) ) );
#endif	/* ! RPC */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
