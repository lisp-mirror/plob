/* -------------------------------------------------------------------------
| Module	plobconst.h
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		4.3.94
| Description	Some constant #define's used in C and LISP
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

#if ! defined(PLOBCONST_H)
#define	PLOBCONST_H

/* -------------------------------------------------------------------------
| Marker tag
 ------------------------------------------------------------------------- */
#define	ESHMARKERTAG			hex(04)

/* -------------------------------------------------------------------------
| Number of lower free bits in a long (POSTORE) objid. These bits are
| used as a type tag for immediate values (see comments at enum type
| SHTYPETAG)
 ------------------------------------------------------------------------- */
#define	NTAGBITS			3
#define	NFIXNUMBITOFFSET		2
#define	NBITSPERBYTE			8

/* -------------------------------------------------------------------------
| Size of a POSTORE word
 ------------------------------------------------------------------------- */
#define	NSIZEOFPOSTOREWORD		4 /* bytes */

/* -------------------------------------------------------------------------
| Object flags
 ------------------------------------------------------------------------- */
/* Get current value of the dependent flag: */
#define	FLAGDEPENDENTGET		-1
#define	FLAGDEPENDENTNONE		0
#define	FLAGDEPENDENTREAD		hex(04)
#define	FLAGDEPENDENTWRITE		hex(08)
#define	FLAGDEPENDENTREADWRITE		\
bitwise_or(FLAGDEPENDENTREAD,FLAGDEPENDENTWRITE)

/* -------------------------------------------------------------------------
| Lock levels and modes
 ------------------------------------------------------------------------- */
#define	ESHLOCKERRORFIRST		-9
#define	ESHLOCKERRORLAST		-1

#define	ESHLOCKCONFLICTELEMENT		-3
#define	ESHLOCKCONFLICTVECTOR		-2
#define	ESHLOCKCONFLICTSTORE		-1
#define	ESHLOCKCONFLICTFIRST		ESHLOCKCONFLICTELEMENT
#define	ESHLOCKCONFLICTLAST		ESHLOCKCONFLICTSTORE

#define	ESHLOCKLEVELNOTHING		hex(00)
#define	ESHLOCKLEVELELEMENT		hex(01)
#define	ESHLOCKLEVELVECTOR		hex(02)
#define	ESHLOCKLEVELSTORE		hex(03)

#define	ESHLOCKMODENOTHING		hex(00)
#define	ESHLOCKMODEREADONLY		hex(04)
#define	ESHLOCKMODEREAD			hex(08)
#define	ESHLOCKMODEWRITE		hex(10)
#define	ESHLOCKMODEREADONLYINTENT	hex(20)
#define	ESHLOCKMODEREADINTENT		hex(40)
#define	ESHLOCKMODEWRITEINTENT		hex(80)

/* -------------------------------------------------------------------------
| Structures
 ------------------------------------------------------------------------- */
/* All predefined structures 'inherit' from SHSTRUCTIDX; their indices
   start therefore at STRUCTIDXOFFSET, not at 0. */
#define	STRUCTIDXOFFSET			1

#endif /* #if ! defined(PLOBCONST_H) */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
