/* -------------------------------------------------------------------------
| Module	plobdadmin.h
| Author	Heiko Kirschke
|		kirschke@informatik.uni-hamburg.de
| Date		1998/11/23
| Description	PLOB admin utility
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

#if ! defined(PLOBDADMIN_H)
#define	PLOBDADMIN_H

#include	<ctype.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	WIN32
#include	<direct.h>
#else
#include	<unistd.h>
#endif

#include	"global.h"
#include	"hash.h"
#include	"generic.h"
#include	"trmalloc.h"
#include	"postore.h"

#include	"cplob.h"
#include	"cplobintern.h"
#include	"cplobmisc.h"
#include	"cplobtype.h"
#include	"cplobsequ.h"
#include	"cploblock.h"
#include	"cplobheap.h"
#include	"cplobbtree.h"
#include	"cplobnumber.h"
#include	"cplobroot.h"
#include	"cplobadmin.h"
#include	"cplobstruct.h"
#include	"cplobclos.h"

#define		RPCNOTYPES
#include	"plobd.h"

/* -------------------------------------------------------------------------
| Application structure
 ------------------------------------------------------------------------- */
typedef enum {
  eSilent,
  eVerbose,
  eVeryVerbose,
  eVeryVeryVerbose,
  eDefaultVerbose	= eVerbose
}	VERBOSE, * PVERBOSE;

typedef enum {
  eFromCommandLine,
  eFromFile,
  eFromInteractive
}	CMDSOURCE, * PCMDSOURCE;

typedef struct {
  LPFILE	pStreamIn;
  LPSTR		pszFilename;
  int		nLine;
}	SOURCEFILE, * PSOURCEFILE;

typedef struct {
  int		argc;
  char **	argv;

  BOOL		bJmpBufErrorValid;
  jmp_buf	jmpbufError;

  CMDSOURCE	eSource0;
  CMDSOURCE	eSource;

  int		nFiles;
  SOURCEFILE	Files [ 16 ];
  FILE *	pStreamOut;

  VERBOSE	eVerbose;

  char *	pszURL;
  SHORTOBJID	oHeap;
  char *	pszRootDirectory;

  int		nCommand;
  int		nMaxCommand;
  char **	ppszCommand;

}	ADMINAPPL, * PADMINAPPL;

#endif	/* ! defined(PLOBDADMIN_H) */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
