/* -------------------------------------------------------------------------
| Module	main.c
| Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		27.10.93
| Description	Test main program
 ------------------------------------------------------------------------- */

#include	<stdio.h>
#include	<stdlib.h>

#include	"_global.h"
#include	"_hash.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* -------------------------------------------------------------------------
| main ()
 ------------------------------------------------------------------------- */
int		main	( int argc, char * argv [] );
int		main	( int argc, char * argv [] )
{
  fnHashDebug ( NULL );
  return 0;
} /* main */
