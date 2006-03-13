/* -------------------------------------------------------------------------
| Module	plobadmin.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/02/04
| Description	PLOB administration functions.
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

#if defined(LISP)
;;;; -------------------------------------------------------------------------
;;;; For further comments look into file plobadmin.h
;;;; -------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#if defined(C2C)
enum {
  /* Default timeout value for RPC calls: */
  nDefaultTimeout	= 25 /* seconds */
};
#endif

#if ! defined(LISP)	/* server: */
DefineFunction ( FIXNUM,
		 fnServerGetPortByDirectory,
		 "c-sh-get-Port-by-directory",
		 ( argument ( CONST_STRING, vector_in, szDirectory )
		   and
		   argument ( GETACTION, value_in, eAction ) ) );
DefineFunction ( FIXNUM,
		 fnServerGetPID, "c-sh-get-pid",
		 ( voidArgument ) );
DefineFunction ( FIXNUM, /* Returns server's PID */
		 fnServerGetDirectory, "c-sh-get-directory",
		 ( argument ( FIXNUM, value_in, nDirectory )
		   and
		   argument ( STRING ( nDirectory),
			      vector_out, szDirectory ) ) );
#endif	/* ! LISP */

#if ! defined(RPC)	/* client: */
DefineFunction ( BOOL,
		 fnClientCreateDatabase, "c-sh-create-database",
		 ( argument ( CONST_STRING, vector_in, szURL ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BOOL,
		 fnServerExit, "c-sh-exit",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bForceExit ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BOOL,
		 fnClientExit, "c-sh-exit",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bForceExit ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BOOL,
		 fnServerDbReset, "c-sh-reset",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bForceReset ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BOOL,
		 fnClientDbReset, "c-sh-reset",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bForceReset ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( BOOL,
		 fnServerRestart, "c-sh-restart",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bForceRestart ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( BOOL,
		 fnClientRestart, "c-sh-restart",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( BOOL, value_in, bForceRestart ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHORTOBJID,
		 fnServerSuspend, "c-sh-suspend",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( CONST_STRING, vector_in, szReason ) ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientSuspend, "c-sh-suspend",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( CONST_STRING, vector_in, szReason ) ) );
#endif	/* ! RPC */

#if ! defined(LISP)	/* server: */
DefineFunction ( SHORTOBJID,
		 fnServerResume, "c-sh-resume",
		 ( voidArgument ) );
#endif	/* ! LISP */
#if ! defined(RPC)	/* client: */
DefineFunction ( SHORTOBJID,
		 fnClientResume, "c-sh-resume",
		 ( voidArgument ) );
#endif	/* ! RPC */

#if defined(C2C)

/* -------------------------------------------------------------------------
| Constants
 ------------------------------------------------------------------------- */

enum { nMasterPort	= 0 };

/* Name of `local' host. */
extern DLLEXPORTVAR const char	szLocalName []		/* = "local" */;

/* Name of stablestore database file. */
extern DLLEXPORTVAR const char	szStablestore []	/* = "stablestore" */;

extern DLLEXPORTVAR const char	szLocalhost	[]	/* = "localhost" */;
extern DLLEXPORTVAR const char	szTcp []		/* = "tcp" */;

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */
/* Check if pszDirname names a stable store directory: */
BOOL DLLEXPORT		fnDatabaseP		( LPCSTR	pszDirectory );

/* Store/delete the PID of the current process into pszDirectory: */
int			fnStorePid		( LPCSTR	pszDirectory );
void			fnUnstorePid		( LPCSTR	pszDirectory );

/* Copy to pszDirectory the database associated to rpc port
   nRpcPort: */
LPSTR			fnGetDirectoryByPort	( LPSTR		pszDirectory,
						  size_t	nDirectory,
						  int		nRpcPort );

/* Return the rpc Port number associated to directory pszDirectory: */
int			fnGetPortByDirectory	( LPCSTR	pszDirectory );

/* Return the PID of a server working on nRpcPort: */
int			fnGetPidByPort		( LPCSTR	pszHost,
						  LPCSTR	pszTransport,
						  int		nRpcPort,
						  int	nTimeout /* sec */ );

/* Start a server on pszDirectory. The server will work on
   pszDirectory relativ to the current directory. Returns the rpc
   Port number of the server started, or -1 in case of failure: */
int DLLEXPORT		fnStartLocalServer	( LPCSTR	pszDirectory,
						  GETACTION	eAction,
						  int	nRpcPortServer );

/* -------------------------------------------------------------------------
| Module initialization functions
 ------------------------------------------------------------------------- */
void			fnInitCommonAdminModule		( void );
void			fnInitializeAdminModule		( void );
void			fnDeinitializeAdminModule	( void );
void			fnDeinitCommonAdminModule	( void );
#endif /* #if defined(C2C) */

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
