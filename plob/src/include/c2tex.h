/* -------------------------------------------------------------------------
| Module	c2tex.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/04/16
| Description	Convert C version info to TeX
| Copyright	(C) 1993,1994 Heiko Kirschke
| Date		17.12.93
| Description	Generator macros for C.
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

PASTE3(\def,\thisversion,PASTE3({,PASTE3(PLOBVERSIONMAJOR,.,PLOBVERSIONMINOR),}))
PASTE3(\def,\thisday,PASTE3({,PLOBVERSIONDAY,}))
PASTE3(\def,\thismonth,PASTE3({,PLOBVERSIONMONTH,}))
PASTE3(\def,\thisyear,PASTE3({,PLOBVERSIONYEAR,}))

PASTE3(\def,\thisauthor,PASTE3({,PLOBAUTHOR,}))
PASTE3(\def,\thisemail,PASTE3({,PLOBEMAIL,}))
PASTE3(\def,\thisurl,PASTE3({,PLOBURL,}))
PASTE3(\def,\thisproject,PASTE3({,PLOBPROJECT,}))
PASTE3(\def,\thislist,PASTE3({,PLOBLIST,}))
PASTE3(\def,\thislistreq,PASTE3({,PLOBLISTREQ,}))

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
