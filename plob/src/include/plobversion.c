/* -------------------------------------------------------------------------
| Module	plobversion.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		2005-04-19
| Description	Echo TeX macros containing version information
|
| Copyright	PLOB! Copyright 1994--2005 Heiko Kirschke.
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

#include	"plobversion.h"
#include	<stdio.h>

int	main	( int		argc,
		  char *	argv [] )
{
  puts ( "\\def\\thisversion{" STRINGVERSION "}\n"
	 "\\def\\thisday{" STRINGVERSIONDAY "}\n"
	 "\\def\\thismonth{" STRINGVERSIONMONTH "}\n"
	 "\\def\\thisyear{" STRINGVERSIONYEAR"}\n"
	 "\\def\\thisauthor{" STRINGAUTHOR "}\n"
	 "\\def\\thisemail{" STRINGEMAIL "}\n"
	 "\\def\\thisurl{" STRINGURL "}\n"
	 "\\def\\thisproject{" STRINGPROJECT "}\n"
	 "\\def\\thislist{" STRINGLIST "}\n"
	 "\\def\\thislistreq{" STRINGLISTREQ "}" );
  return 0;
}

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/