/* -------------------------------------------------------------------------
| Module	plobclos.h
| Author	Heiko Kirschke
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		9.3.94
| Description	PLOB CLOS instance functions.
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

#if defined(LISP)
;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plobclos.h
;;;; --------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#if defined(C2C)

#define	instance_wrapper( oObjId )	\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshInstIdxClassWrapper ), \
	       eshInstanceTag )
#define	instance_data( oObjId )	\
_SVREF_OBJID ( oObjId, Cooked2RawIndex ( eshInstIdxDataVector ), \
	       eshInstanceTag )

#define	make_instance()		\
     fnCreateObject((SHTYPETAG)eshInstanceTag,0,(SHTYPETAG)NULLTYPETAG,0)
#define	instancep( oSelf )	(typetagof(oSelf)==eshInstanceTag)

void			fnInitCommonCLOSModule		( void );
void			fnInitializeCLOSModule		( void );
void			fnDeinitializeCLOSModule	( void );
void			fnDeinitCommonCLOSModule	( void );

#endif	/* defined(C2C) */

/* -------------------------------------------------------------------------
| CLOS instance
 ------------------------------------------------------------------------- */

DefineConstant ( eshInstanceTag, "+instance-type-tag+", hex ( 48 ),
		 "Type tag for plob objects of type clos instance." );

BeginEnum ( SHINSTIDX )
  enumerator ( eshInstIdxClassWrapper,
	       "+clos-location-class-wrapper+", 0,
	       "Index of plob CLOS instance class wrapper. This is a\
 reference to an instance of class class-description." )
  and
  enumerator ( eshInstIdxDataVector,
	       "+clos-location-data-vector+", 1,
	       "Index of plob CLOS instance data vector." )
  and
  enumerator ( eshInstObjIdSize, "+clos-instance-size+", 2,
	       "Size of plob CLOS instance in words." )
EndEnum ( SHINSTIDX );

#define	PLOBIDXOFFSET	0

/* Predefined class: PLOB description: */
BeginEnum ( SHPLOBDESCRIDX )
  enumerator ( eshPlobDescrSize,
	       "+plob-description-size+", PLOBIDXOFFSET,
	       "Size of PLOB description in words." )
EndEnum ( SHPLOBDESCRIDX );

/* Predefined class: CLOS class description: */
BeginEnum ( SHCLASSDESCRIDX )
  enumerator ( eshClassDescrIdxName,
	       "+class-description-location-name+",
	       add ( PLOBIDXOFFSET, 0 ),
	       "Index of CLOS class description name field." )
  and
  enumerator ( eshClassDescrIdxSuperclasses,
	       "+class-description-location-superclasses+",
	       add ( PLOBIDXOFFSET, 1 ),
	       "Index of CLOS class description superclasses field." )
  and
  enumerator ( eshClassDescrIdxPrecedenceList,
	       "+class-description-location-precedence-list+",
	       add ( PLOBIDXOFFSET, 2 ),
	       "Index of CLOS class description precedence list field." )
  and
  enumerator ( eshClassDescrIdxMetaclass,
	       "+class-description-location-metaclass+",
	       add ( PLOBIDXOFFSET, 3 ),
	       "Index of CLOS class description metaclass field." )
  and
  enumerator ( eshClassDescrIdxVersion,
	       "+class-description-location-version+",
	       add ( PLOBIDXOFFSET, 4 ),
	       "Index of CLOS class description version field." )
  and
  enumerator ( eshClassDescrIdxTimeStamp,
	       "+class-description-location-time-stamp+",
	       add ( PLOBIDXOFFSET, 5 ),
	       "Index of CLOS class description time stamp field." )
  and
  enumerator ( eshClassDescrIdxSchemaEvolution,
	       "+class-description-location-schema-evolution+",
	       add ( PLOBIDXOFFSET, 6 ),
	       "Index of CLOS class description schema evolution field." )
  and
  enumerator ( eshClassDescrIdxNextGen,
	       "+class-description-location-next-generation+",
	       add ( PLOBIDXOFFSET, 7 ),
	       "Index of CLOS class description field with the reference\
 to the next generation." )
  and
  enumerator ( eshClassDescrIdxDirMethods,
	       "+class-description-location-direct-methods+",
	       add ( PLOBIDXOFFSET, 8 ),
	       "Index of CLOS class description field with the direct\
 method descriptions." )
  and
  enumerator ( eshClassDescrIdxPNSlots,
	       "+class-description-location-persistent-slot-numbers+",
	       add ( PLOBIDXOFFSET, 9 ),
	       "Index of CLOS class description field with the number of\
 persistent slots." )
  and
  enumerator ( eshClassDescrIdxNSlots,
	       "+class-description-location-slot-numbers+",
	       add ( PLOBIDXOFFSET, 10 ),
	       "Index of CLOS class description field with the total\
 number of slots." )
  and
  enumerator ( eshClassDescrIdxDirSlots,
	       "+class-description-location-direct-slots+",
	       add ( PLOBIDXOFFSET, 11 ),
	       "Index of CLOS class description field with the\
 descriptions of the direct slots." )
  and
  enumerator ( eshClassDescrIdxEffSlots,
	       "+class-description-location-effective-slots+",
	       add ( PLOBIDXOFFSET, 12 ),
	       "Index of CLOS class description field with the\
 descriptions of the effective slots." )
  and
  enumerator ( eshClassDescrIdxConstructor,
	       "+class-description-location-constructor+",
	       add ( PLOBIDXOFFSET, 13 ),
	       "Index of CLOS class description field with the\
 instance constructor function." )
  and
  enumerator ( eshClassDescrIdxDependent,
	       "+class-description-location-dependent+",
	       add ( PLOBIDXOFFSET, 14 ),
	       "Index of CLOS class description field with the\
 instance dependent flag." )
  and
  enumerator ( eshClassDescrIdxPList,
	       "+class-description-location-plist+",
	       add ( PLOBIDXOFFSET, 15 ),
	       "Index of CLOS class description field with the\
 class' property list." )
  and
  enumerator ( eshClassDescrIdxPrototype,
	       "+class-description-location-prototype+",
	       add ( PLOBIDXOFFSET, 16 ),
	       "Index of CLOS class description field with the\
 prototype instance." )
  and
  enumerator ( eshClassDescrSize,
	       "+class-description-size+", add ( PLOBIDXOFFSET, 17 ),
	       "Size of PLOB CLOS class description in words." )
EndEnum ( SHCLASSDESCRIDX );

#define	SLOTIDXOFFSET	add ( PLOBIDXOFFSET, 9 )

/* Predefined class: CLOS slot description: */
BeginEnum ( SHCLASSLOTIDX )
  enumerator ( eshClassSlotDescrIdxName,
	       "+slot-description-location-name+",
	       add ( PLOBIDXOFFSET, 0 ),
	       "Index of CLOS slot description name field." )
  and
  enumerator ( eshClassSlotDescrIdxInitArg,
	       "+slot-description-location-init-args+",
	       add ( PLOBIDXOFFSET, 1 ),
	       "Index of CLOS slot description initarg field." )
  and
  enumerator ( eshClassSlotDescrIdxInitForm,
	       "+slot-description-location-init-form+",
	       add ( PLOBIDXOFFSET, 2 ),
	       "Index of CLOS slot description initform field." )
  and
  enumerator ( eshClassSlotDescrIdxInitFunc,
	       "+slot-description-location-init-function+",
	       add ( PLOBIDXOFFSET, 3 ),
	       "Index of CLOS slot description initform field." )
  and
  enumerator ( eshClassSlotDescrIdxType,
	       "+slot-description-location-type+",
	       add ( PLOBIDXOFFSET, 4 ),
	       "Index of CLOS slot description type field." )
  and
  enumerator ( eshClassSlotDescrIdxAlloc,
	       "+slot-description-location-allocation+",
	       add ( PLOBIDXOFFSET, 5 ),
	       "Index of CLOS slot description allocation field." )
  and
  enumerator ( eshClassSlotDescrIdxExtent,
	       "+slot-description-location-extent+",
	       add ( PLOBIDXOFFSET, 6 ),
	       "Index of CLOS slot description extent field." )
  and
  enumerator ( eshClassSlotDescrIdxDeferred,
	       "+slot-description-location-deferred+",
	       add ( PLOBIDXOFFSET, 7 ),
	       "Index of CLOS slot description deferred field." )
  and
  enumerator ( eshClassSlotDescrIdxIndex,
	       "+slot-description-location-index+",
	       add ( PLOBIDXOFFSET, 8 ),
	       "Index of CLOS slot description index field." )
  and
  enumerator ( eshClassSlotDescrSize,
	       "+slot-description-size+", SLOTIDXOFFSET,
	       "Size of PLOB CLOS slot description in words." )
EndEnum ( SHCLASSLOTIDX );

/* Predefined class: CLOS direct slot description: */
BeginEnum ( SHDIRCLASSLOTIDX )
  enumerator ( eshClassDirSlotDescrIdxReaders,
	       "+slot-description-location-readers+",
	       add ( SLOTIDXOFFSET, 0 ),
	       "Index of CLOS direct slot description readers field." )
  and
  enumerator ( eshClassDirSlotDescrIdxWriters,
	       "+slot-description-location-writers+",
	       add ( SLOTIDXOFFSET, 1 ),
	       "Index of CLOS direct slot description writers field." )
  and
  enumerator ( eshClassDirSlotDescrSize,
	       "+direct-slot-description-size+",
	       add ( SLOTIDXOFFSET, 2 ),
	       "Size of PLOB CLOS direct slot description in words." )
EndEnum ( SHDIRCLASSLOTIDX );

/* Predefined class: CLOS effective slot description: */
BeginEnum ( SHEFFCLASSLOTIDX )
  enumerator ( eshClassEffSlotDescrIdxLocation,
	       "+slot-description-location-location+",
	       add ( SLOTIDXOFFSET, 0 ),
	       "Index of CLOS effective slot description location field." )
  and
  enumerator ( eshClassEffSlotDescrSize,
	       "+effective-slot-description-size+",
	       add ( SLOTIDXOFFSET, 1 ),
	       "Size of PLOB CLOS effective slot description in words." )
EndEnum ( SHEFFCLASSLOTIDX );

/* Predefined class: CLOS method description: */
BeginEnum ( SHMETHODIDX )
  enumerator ( eshMethodIdxName,
	       "+method-description-location-name+",
	       add ( PLOBIDXOFFSET, 0 ),
	       "Index of CLOS method description function field." )
  and
  enumerator ( eshMethodIdxFunction,
	       "+method-description-location-function+",
	       add ( PLOBIDXOFFSET, 1 ),
	       "Index of CLOS method description function field." )
  and
  enumerator ( eshMethodIdxLambdaList,
	       "+method-description-location-lambda-list+",
	       add ( PLOBIDXOFFSET, 2 ),
	       "Index of CLOS method description  field." )
  and
  enumerator ( eshMethodIdxSpecializers,
	       "+method-description-location-specializers+",
	       add ( PLOBIDXOFFSET, 3 ),
	       "Index of CLOS method description  field." )
  and
  enumerator ( eshMethodIdxQualifiers,
	       "+method-description-location-qualifiers+",
	       add ( PLOBIDXOFFSET, 4 ),
	       "Index of CLOS method description  field." )
  and
  enumerator ( eshMethodSize,
	       "+method-description-size+",
	       add ( PLOBIDXOFFSET, 5 ),
	       "Size of PLOB CLOS method description in words." )
EndEnum ( SHMETHODIDX );

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerDbCreateInstances, "c-sh-create-instances",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in,
			      oShortObjIdClassDescr )
		   and
		   argument ( FIXNUM, value_in, nObjIds )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_out, pObjIds )
		   and
		   argument ( u_int, value_out, pnSlots )
		   and
		   argument ( VECTOR ( u_int, nObjIds ),
			      vector_out, pDatas ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientDbCreateInstance, "c-sh-create-instance",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in,
			      oShortObjIdClassDescr ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerInstanceWriteWrapper,
		 "c-sh-write-instance-wrapper",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		   and
		   argument ( SHORTOBJID, value_in,
			      oShortObjIdClassDescr ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientInstanceWriteWrapper,
		 "c-sh-write-instance-wrapper",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		   and
		   argument ( SHORTOBJID, value_in,
			      oShortObjIdClassDescr ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHLOCK,
		 fnServerInstanceWriteData,
		 "c-sh-write-instance-data",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdData ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHLOCK,
		 fnClientInstanceWriteData,
		 "c-sh-write-instance-data",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdInstance )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdData ) ) );
#endif	/* ! RPC */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
