/* -------------------------------------------------------------------------
| Module	splobroot.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1996/09/23
| Description	PLOB server header file:
|		Macros and functions for usage by the PLOB server
|
| Copyright	PLOB! Copyright 1994--2002 Heiko Kirschke.
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

#include	"plobroot.h"

/* -------------------------------------------------------------------------
| Utility functions:
 ------------------------------------------------------------------------- */
/* Set oGlobalMinObjId, oGlobalMaxObjId to their correct values: */
void DLLEXPORT	fnSetGlobalMinMaxObjId	( void );

/* -------------------------------------------------------------------------
| Functions on administrating PLOB client users:
 ------------------------------------------------------------------------- */

#define		Users()			\
((boundp(oUsersCache))?oUsersCache:fnUsers())

/* Returns the object containing all known client users, i.e. a BTree: */
OBJID DLLEXPORT	fnUsers			( void );
OBJID DLLEXPORT	fnUserInsertByName	( LPCSTR	lpszUserName,
					  int		nUserID );
OBJID DLLEXPORT	fnUserSearchByName	( LPCSTR	lpszUserName );
OBJID DLLEXPORT	fnUserDeleteByName	( LPCSTR	lpszUserName );

extern OBJID	oUsersCache		/* = NULLOBJID */;

/* -------------------------------------------------------------------------
| Functions on administrating PLOB client groups:
| (1996/10/07 HK: Not yet supported)
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Functions on administrating PLOB client machines:
 ------------------------------------------------------------------------- */

#define		Machs()			\
((boundp(oMachsCache))?oMachsCache:fnMachs())

/* Returns the object containing all known client machines, i.e. a BTree: */
OBJID DLLEXPORT	fnMachs			( void );
/* Create a machine: */
OBJID DLLEXPORT	fnCreateMachine		( int		nAddr [ 4 ],
					  OBJID		oLoginP );
/* Get/set login flag of a machine: */
OBJID DLLEXPORT	fnMachLoginP		( OBJID		oMachine,
					  OBJID		oLoginP );
OBJID DLLEXPORT	fnMachInsertByAddr	( int		nAddr [ 4 ],
					  OBJID		oLoginP );
OBJID DLLEXPORT	fnMachSearchByAddr	( int		nAddr [ 4 ] );
OBJID DLLEXPORT	fnMachDeleteByAddr	( int		nAddr [ 4 ] );

extern OBJID	oMachsCache		/* = NULLOBJID */;

/* -------------------------------------------------------------------------
| Functions on administrating PLOB client sessions:
 ------------------------------------------------------------------------- */

#define		Sessions()		\
((boundp(oSessionsCache))?oSessionsCache:fnSessions())

/* Returns the object containing all known client sessions,
   i.e. a BTree containing persistent heaps: */
OBJID DLLEXPORT	fnSessions		( void );
BTREERESULT DLLEXPORT fnSessionInsert	( OBJID		oSession );
OBJID DLLEXPORT	fnSessionSearch		( OBJID		oSession );
OBJID DLLEXPORT	fnSessionDelete		( OBJID		oSession );

extern OBJID	oSessionsCache		/* = NULLOBJID */;

/* -------------------------------------------------------------------------
| Get the LISP root object:
 ------------------------------------------------------------------------- */
OBJID DLLEXPORT	fnReadLispRoot		( void );

/* -------------------------------------------------------------------------
| Functions on administrating PLOB:
 ------------------------------------------------------------------------- */
/* Check if oUser, oMachine is allowed to log in: */
BOOL DLLEXPORT		fnLoginP	( OBJID		oUser,
					  OBJID		oMachine,
					  LPSTR		lpszReason,
					  size_t	nReason );
/* Check if oUser, oMachine is the administrator: */
typedef enum {
  eaNoAdmin = -1,	/* No administrator defined up to now */
  eaAdminFalse,		/* Passed user, machine isn't the administrator */
  eaAdminTrue		/* Passed user, machine is the administrator */
}	ADMINP, * PADMINP;
ADMINP DLLEXPORT	fnAdminP	( OBJID		oUser,
					  OBJID		oMachine );
/* Make oUser, oMachine the administrator: */
void DLLEXPORT		fnAdminSet	( OBJID		oUser,
					  OBJID		oMachine );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
