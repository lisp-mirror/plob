/* -------------------------------------------------------------------------
| Module	plobversion.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/03/04
| Description	PLOB version number
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

#ifdef C2TEX
% This file was generated from plobversion.h;
% changes done here will be lost!
#endif

/* Current version of PLOB: */

#define	PLOBVERSIONMAJOR	2
#define	PLOBVERSIONMINOR	10

#define	PLOBVERSIONDAY		1
#define	PLOBVERSIONMONTH	May
#define	PLOBVERSIONYEAR		2002

/* Older version of PLOB: */
/*
#define	PLOBVERSIONMAJOR	2
#define	PLOBVERSIONMINOR	09

#define	PLOBVERSIONDAY		22
#define	PLOBVERSIONMONTH	May
#define	PLOBVERSIONYEAR		2000

#define	PLOBVERSIONMAJOR	2
#define	PLOBVERSIONMINOR	08

#define	PLOBVERSIONDAY		1
#define	PLOBVERSIONMONTH	December
#define	PLOBVERSIONYEAR		1998

#define	PLOBVERSIONMAJOR	2
#define	PLOBVERSIONMINOR	07

#define	PLOBVERSIONDAY		18
#define	PLOBVERSIONMONTH	September
#define	PLOBVERSIONYEAR		1998

#define	PLOBVERSIONMAJOR	2
#define	PLOBVERSIONMINOR	06

#define	PLOBVERSIONDAY		26
#define	PLOBVERSIONMONTH	June
#define	PLOBVERSIONYEAR		1998
*/

#define	PLOBAUTHOR Heiko Kirschke
/*
#define	PLOBEMAIL Heiko.Kirschke@poet.de
#define	PLOBEMAIL kirschke@informatik.uni-hamburg.de
*/
/*
#define	PLOBEMAIL Heiko.Kirschke@acm.org
#define	PLOBURL www.lisp.de/software/plob/
*/
#define	PLOBEMAIL Heiko.Kirschke@acm.org
#define	PLOBURL plob.sourceforge.net
#define	PLOBPROJECT www.sourceforge.net/projects/plob

#define PLOBLIST plob-discussion@lists.sourceforge.net
#define PLOBLISTREQ plob-discussion-request@lists.sourceforge.net

#if ! defined(PASTE2)
#define	PASTE23( token1, token2 ) token1##token2
#define	PASTE22( token1, token2 ) PASTE23(token1,token2)
#define	PASTE2( token1, token2 ) PASTE22(token1,token2)
#endif

#if ! defined(PASTE3)
#define	PASTE33( token1, token2, token3 ) token1##token2##token3
#define	PASTE32( token1, token2, token3 ) PASTE33(token1,token2,token3)
#define	PASTE3( token1, token2, token3 ) PASTE32(token1,token2,token3)
#endif

#define	PLOBVERSION PASTE2(PLOBVERSIONMAJOR,PLOBVERSIONMINOR)

#if ! defined(STRINGINIZE)
#define	STRINGINIZE3( token ) #token
#define	STRINGINIZE2( token ) STRINGINIZE3(token)
#define	STRINGINIZE( token ) STRINGINIZE2(token)
#endif

#define	STRINGVERSIONMAJOR STRINGINIZE(PLOBVERSIONMAJOR)
#define	STRINGVERSIONMINOR STRINGINIZE(PLOBVERSIONMINOR)
#define	STRINGVERSION STRINGVERSIONMAJOR "." STRINGVERSIONMINOR

#define	STRINGVERSIONDAY STRINGINIZE(PLOBVERSIONDAY)
#define	STRINGVERSIONMONTH STRINGINIZE(PLOBVERSIONMONTH)
#define	STRINGVERSIONYEAR STRINGINIZE(PLOBVERSIONYEAR)

#define	STRINGAUTHOR STRINGINIZE(PLOBAUTHOR)
#define	STRINGEMAIL STRINGINIZE(PLOBEMAIL)
#define	STRINGURL STRINGINIZE(PLOBURL)
#define	STRINGPROJECT STRINGINIZE(PLOBPROJECT)
#define	STRINGLIST STRINGINIZE(PLOBLIST)
#define	STRINGLISTREQ STRINGINIZE(PLOBLISTREQ)

#ifdef C2TEX
#include	"c2tex.h"
#endif

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
