/* -------------------------------------------------------------------------
| Module	dos2unix.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1999-08-24
| Description	Change end-of-line CR+LF into LF
|		When compiling this file, take care to use cygwin's
|		gcc if possible, since this will support cygwin's
|		canonical drive designators (e.g., "/cygwin/c/tmp/"
|		instead of "c:/tmp/").
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

/* ----------------------------------------------------------------------- */
#define length(array)		(sizeof(array)/sizeof((array)[0]))

/* ----------------------------------------------------------------------- */
typedef const char *		STRING;
#define	null			((void*)0)

/* ----------------------------------------------------------------------- */
typedef enum {
  isMin,
  isLF	= isMin,	/* char is a LF char */
  isCR,			/* char is a CR char */
  isChar,		/* char is no CR or LF char */
  isMax	= isChar,
  isLen	= isMax - isMin + 1
}	CHARCLASS;
typedef const CHARCLASS *	PCHARCLASS;

typedef enum {
  labelMin,
  scannedChar	= labelMin,
  scannedCR,
  labelMax	= scannedCR,
  labelLen	= labelMax - labelMin + 1,
  labelStart	= scannedChar
}	STATELABEL;
typedef const STATELABEL *	PSTATELABEL;

typedef struct {
  STRING	inFilename;
  FILE *	inStream;
  STRING	outFilename;
  FILE *	outStream;
}	ENVIRONMENT;
typedef const ENVIRONMENT *	PENVIRONMENT;

typedef void ( * PFNACTION ) ( char input, PENVIRONMENT environment );

/* State transition with an associated action: */
typedef struct {
  PFNACTION	action;
  STATELABEL	next;
}	TRANSITION;
typedef const TRANSITION *	PTRANSITION;

typedef struct {
  STATELABEL	label;
  TRANSITION	transition [ isLen ];
}	STATE, * PSTATE;

/* ----------------------------------------------------------------------- */
static void	noOperation	( char		input,
				  PENVIRONMENT	environment );
static void	writeChar	( char		input,
				  PENVIRONMENT	environment );
static void	writeCRandChar	( char		input,
				  PENVIRONMENT	environment );

/* ----------------------------------------------------------------------- */
/* The deterministic finite automaton transforming CR, LF lineends into LF: */
static const STATE dfa [ labelLen ]	= {
  {
    scannedChar,
    {
      /* isLF */	{ writeChar,		scannedChar },
      /* isCR */	{ noOperation,		scannedCR },
      /* isChar */	{ writeChar,		scannedChar }
    }
  },

  {
    scannedCR,
    {
      /* isLF */	{ writeChar,		scannedChar },
      /* isCR */	{ writeCRandChar,	scannedChar },
      /* isChar */	{ writeCRandChar,	scannedChar }
    }
  }
};

/* ----------------------------------------------------------------------- */
static const char cygdrive []	= "/cygdrive/";

/* ----------------------------------------------------------------------- */
static CHARCLASS	getCharClass	( char	input )
{
  switch ( input ) {
  case '\n':
    return isLF;
  case '\r':
    return isCR;
  default:
    return isChar;
  }
} /* getCharClass */

/* ----------------------------------------------------------------------- */
static void	noOperation	( char		input,
				  PENVIRONMENT	environment )
{
} /* noOperation */

/* ----------------------------------------------------------------------- */
static void	writeChar	( char		input,
				  PENVIRONMENT	environment )
{
  fputc ( input, environment->outStream );
} /* writeChar */

/* ----------------------------------------------------------------------- */
static void	writeCRandChar	( char		input,
				  PENVIRONMENT	environment )
{
  fputc ( '\r', environment->outStream );
  fputc ( input, environment->outStream );
} /* writeCRandChar */

/* ----------------------------------------------------------------------- */
int	main	( int		argc,
		  const char *	argv [] )
{
  char		input;
  ENVIRONMENT	environment;
  STATELABEL	current;

  environment.inFilename	= "stdin";
  environment.inStream		= stdin;
  environment.outFilename	= "stdout";
  environment.outStream		= stdout;

  current			= labelStart;

  if ( argc >= 2 &&
       ( argv [ 1 ] [ 0 ] != '-' || argv [ 1 ] [ 1 ] != '\0' ) ) {
    environment.inFilename	= argv [ 1 ];
    environment.inStream	= fopen ( environment.inFilename, "rb" );
    if ( environment.inStream == null ) {
      fprintf ( stderr, "could not open input file %s\n",
		environment.inFilename );
      environment.inFilename	= null;
      return 1;
    }
  }

  if ( argc >= 3 &&
       ( argv [ 2 ] [ 0 ] != '-' || argv [ 2 ] [ 1 ] != '\0' ) ) {
    environment.outFilename	= argv [ 2 ];
    environment.outStream	= fopen ( environment.outFilename, "wb" );
    if ( environment.outStream == null ) {
      fprintf ( stderr, "could not open output file %s\n",
		environment.outFilename );
      environment.outFilename	= null;
      if ( environment.inStream != stdin ) {
	fclose ( environment.inStream );
	environment.inStream	= null;
      }
      environment.inFilename	= null;
      return 2;
    }
  }

  while ( ( input = fgetc ( environment.inStream ) ) != EOF ) {
    PTRANSITION	transition;
    transition	= & dfa [ current ].transition [ getCharClass ( input ) ];
    /* Perform action associated to state transition: */
    transition->action ( input, &environment );
    /* Advance to next state: */
    current	= transition->next;
  }

  if ( environment.outStream != stdout ) {
    fclose ( environment.outStream );
    environment.outStream	= null;
  }
  environment.outFilename	= null;

  if ( environment.inStream != stdin ) {
    fclose ( environment.inStream );
    environment.inStream	= null;
  }
  environment.inFilename	= null;

  return 0;
} /* main */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
