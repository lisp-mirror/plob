/*-
 * Copyright (c) 1992 Henry Spencer.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer of the University of Toronto.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)regex.h	8.2 (Berkeley) 1/3/94
 *
 | Module	plobregex.h
 | Author	Heiko Kirschke
 |		mailto:Heiko.Kirschke@acm.org
 | Date		2005-05-10
 | Description	Foreign language interface to PLOB regular expressions
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
 |  SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
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
  ------------------------------------------------------------------------ */

#if defined(LISP)
;;;; -------------------------------------------------------------------------
;;;; For further comments look into file plobregex.h
;;;; -------------------------------------------------------------------------

#elif ! defined(C2C) && ! defined(RPC)
#include	"c2c.h"
#endif

#if defined(C2C) && !WIN32 &&!SOLARIS
#include <sys/cdefs.h>
#endif

#if defined(C2C)
/* types */
typedef off_t regoff_t;

typedef struct {
	int re_magic;
	size_t re_nsub;		/* number of parenthesized subexpressions */
        const char *re_endp;	/* end pointer for REG_PEND */
	struct re_guts *re_g;	/* none of your business :-) */
} regex_t;

typedef struct {
	regoff_t rm_so;		/* start of match */
	regoff_t rm_eo;		/* end of match */
} regmatch_t;
#endif

/* regcomp() flags */
BeginEnum ( REGCOMP )
  enumerator ( eregBasic /* REG_BASIC */, "+regex-basic+",	oct ( 0 ),
	       "Compile obsolete (``basic'') REs that are the default." )
  and
  enumerator ( eregExtended /* REG_EXTENDED */, "+regex-extended+",oct ( 1 ),
	        "Compile modern (``extended'') REs, rather than the\
 obsolete (``basic'') REs that are the default." )
  and
  enumerator ( eregICase /* REG_ICASE */, "+regex-icase+",	oct ( 2 ),
	       "Compile for matching that ignores upper/lower case\
 distinctions." )
  and
  enumerator ( eregNoSub /* REG_NOSUB */, "+regex-nosub+",	oct ( 4 ),
	       "Compile for matching that need only report success\
 or failure, not what was matched." )
  and
  enumerator ( eregNewline /* REG_NEWLINE */, "+regex-newline+",oct ( 10 ),
	       "Compile for newline-sensitive matching. By default,\
 newline is a completely ordinary character with no special meaning\
 in either REs or strings. With this flag, `[^' bracket expressions\
 and `.' never match newline, a `^' anchor matches the null string\
 after any newline in the string in addition to its normal function,\
 and the `$' anchor matches the null string before any newline in\
 the string in addition to its normal function." )
  and
  enumerator ( eregNoSpec /* REG_NOSPEC */, "+regex-nospec+",	oct ( 20 ),
	       "Compile with recognition of all special characters\
 turned off. All characters are thus considered ordinary, so the\
 ``RE'' is a literal string. This is an extension, compatible with\
 but not specified by POSIX 1003.2, and should be used with caution\
 in software intended to be portable to other systems. REG_EXTENDED\
 and REG_NOSPEC may not be used in the same call to regcomp." )
  and
  enumerator ( eregPEnd /* REG_PEND */, "+regex-pend+",		oct ( 40 ),
	       "The regular expression ends, not at the first NUL,\
 but just before the character pointed to by the re_endp member of\
 the structure pointed to by preg. The re_endp member is of type\
 const char *. This flag permits inclusion of NULs in the RE; they\
 are considered ordinary characters. This is an extension, compatible\
 with but not specified by POSIX 1003.2, and should be used with\
 caution in software intended to be portable to other systems." )
  and
  enumerator ( eregDump /* REG_DUMP */, "+regex-dump+",		oct ( 200 ),
	       "Unknown flag." )
  /* PLOB extra flags: */
  and
  enumerator ( eregNotMatching, "+regex-not-matching+",		oct ( 400 ),
	       "Signal a match for strings NOT matching the\
 pattern." )
  /* PLOB internal used flags: */
  and
  enumerator ( eregCompiled, "+regex-compiled+",		oct ( 1000 ),
	       "PLOB internal: The regular expression was\
 compiled already." )
EndEnum ( REGCOMP );

/* regerror() flags */
BeginEnum ( REGERROR )
  enumerator ( eregOk, "+regex-ok+",				0,
	       "no error on call to regcomp() resp. match found at regexec()" )
  and
  enumerator ( eregNoMatch /* REG_NOMATCH */, "+regex-nomatch+",1,
	       "regexec() failed to match" )
  and
  enumerator ( eregBadPat /* REG_BADPAT */, "+regex-badpat+",	2,
	       "invalid regular expression" )
  and
  enumerator ( eregECollate /* REG_ECOLLATE */, "+regex-ecollate+",3,
	       "invalid collating element" )
  and
  enumerator ( eregECType /* REG_ECTYPE */, "+regex-ectype+",	4,
	       "invalid character class" )
  and
  enumerator ( eregEEscape /* REG_EESCAPE */, "+regex-eescape+",5,
	       "backslash applied to unescapable character" )
  and
  enumerator ( eregESubreg /* REG_ESUBREG */, "+regex-esubreg+",6,
	       "invalid backreference number" )
  and
  enumerator ( eregEBrack /* REG_EBRACK */, "+regex-ebrack+",	7,
	       "brackets [ ] not balanced" )
  and
  enumerator ( eregEParen /* REG_EPAREN */, "+regex-eparen+",	8,
	       "parentheses ( ) not balanced" )
  and
  enumerator ( eregEBrace /* REG_EBRACE */, "+regex-ebrace+",	9,
	       "braces { } not balanced" )
  and
  enumerator ( eregBadBr /* REG_BADBR */, "+regex-badbr+",	10,
	       "invalid repetition count(s) in { }" )
  and
  enumerator ( eregERange /* REG_ERANGE */, "+regex-erange+",	11,
	       "invalid character range in [ ]" )
  and
  enumerator ( eregESpace /* REG_ESPACE */, "+regex-espace+",	12,
	       "ran out of memory" )
  and
  enumerator ( eregBadRpt /* REG_BADRPT */, "+regex-badrpt+",	13,
	       "?, *, or + operand invalid" )
  and
  enumerator ( eregEmpty /* REG_EMPTY */, "+regex-empty+",	14,
	       "empty (sub)expression" )
  and
  enumerator ( eregAssert /* REG_ASSERT */, "+regex-assert+",	15,
	       "``can't happen''-you found a bug" )
  and
  enumerator ( eregInvArg /* REG_INVARG */, "+regex-invarg+",	16,
	       "invalid argument, e.g. negative-length string" )
  and
  enumerator ( eregAtoI /* REG_ATOI */,  "+regex-atoi+",	255,
	       "convert name to number (!)" )
  and
  enumerator ( eregItoA /* REG_ITOA */, "+ereg-itoa+",		oct ( 400 ),
	       "convert number to name (!)" )
EndEnum ( REGERROR );

/* regexec() flags */
BeginEnum ( REGEXEC )
  enumerator ( eregNotBol /* REG_NOTBOL */, "+regex-notbol+",	oct ( 1 ),
	       "The first character of the string is not the beginning\
 of a line, so the `^' anchor should not match before it. This does not\
 affect the behavior of newlines under REG_NEWLINE." )
  and
  enumerator ( eregNotEol /* REG_NOTEOL */, "+regex-noteol+",	oct ( 2 ),
	       "The NUL terminating the string does not end a line,\
 so the `$' anchor should not match before it. This does not affect\
 the behavior of newlines under REG_NEWLINE." )
  and
  enumerator ( eregStartEnd /* REG_STARTEND */, "+regex-startend+",oct ( 4 ),
	       "The string is considered to start at\
 string + pmatch[0].rm_so and to have a terminating NUL located at\
 string + pmatch[0].rm_eo (there need not actually be a NUL at that\
 location), regardless of the value of nmatch. See below for the\
 definition of pmatch and nmatch. This is an extension, compatible\
 with but not specified by POSIX 1003.2, and should be used with\
 caution in software intended to be portable to other systems. Note\
 that a non-zero rm_so does not imply REG_NOTBOL; REG_STARTEND\
 affects only the location of the string, not how it is matched." )
  and
  enumerator ( eregTrace /* REG_TRACE */, "+regex-trace+",	oct ( 400 ),
	       "tracing of execution" )
  and
  enumerator ( eregLarge /* REG_LARGE */, "+regex-large",	oct ( 1000 ),
	       "force large representation" )
  and
  enumerator ( eregBackR /* REG_BACKR */, "+regex-backref+",	oct ( 2000 ),
	       "force use of backref code" )
EndEnum ( REGEXEC );

#if defined(C2C)
REGERROR	fnRegComp(regex_t *preg, const char *pattern, REGCOMP cflags );
size_t		fnRegError(REGERROR errcode, const regex_t *preg,
			   char *errbuf, size_t errbuf_size);
REGERROR	fnRegExec(const regex_t *preg, const char *string,
			  size_t nmatch, regmatch_t pmatch[], REGEXEC eflags);
void		fnRegFree(regex_t *preg);

/* -------------------------------------------------------------------------
| PLOB regular expression instance type
 ------------------------------------------------------------------------- */
typedef struct {
  /* --- Header: --------------------------------------------------------- */
  PLOBHEADER	Header;		/* PLOB header information */
  /* --- Persistent references: ------------------------------------------ */
  OBJID		oPattern;	/* The source pattern, string */
  OBJID		oFlagsRegComp;	/* Compile flags for regcomp (), fixnum */
  OBJID		oErrRegComp;	/* Error code from call to regcomp(), fixnum */
  OBJID		oFlagsRegExec;	/* Default flags for regexec(), fixnum */
  /* --- Persistent values: ---------------------------------------------- */
  time_t	timeRegEx;	/* Time the regex was compiled */
  regex_t	regEx;		/* Compiled regex */
}	PLOBREGEX, * LPPLOBREGEX;

void	fnInitCommonRegExModule		( void );
void	fnInitializeRegExModule		( void );
void	fnDeinitializeRegExModule	( void );
void	fnDeinitCommonRegExModule	( void );

OBJID DLLEXPORT	fnMakeRegEx		( LPCSTR	lpszPattern,
					  REGCOMP	eFlagsComp,
					  REGEXEC	eFlagsRegExec );

#define		make_regex(p,c,e)	fnMakeRegEx(p,c,e)
#define		regexp(oSelf)		(typetagof(oSelf)==eshRegExTag)

#endif /* C2C */

DefineConstant ( eshRegExTag, "+regex-type-tag+", hex ( 120 ),
		 "Type tag for plob objects of type regular expression." );

BeginEnum ( SHREGEXIDX )
  enumerator ( eshRegExIdxPattern, "+regex-location-pattern+", 0,
	       "Index of plob regex pattern field." )
  and
  enumerator ( eshRegExIdxFlagsRegComp, "+regex-location-flags-reg-comp+", 1,
	       "Index of plob regex flags regcomp field." )
  and
  enumerator ( eshRegExIdxErrRegComp, "+regex-location-err-reg-comp+", 2,
	       "Index of plob regex err regcomp field." )
  and
  enumerator ( eshRegExIdxFlagsRegExec, "+regex-location-flags-reg-exec+", 3,
	       "Index of plob regex flags regexec field." )
EndEnum ( SHREGEXIDX );

DefineFunction ( SHORTOBJID,
		 fnShortMakeRegEx, "c-sh-make-regex",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( CONST_STRING, vector_in, lpszPattern )
		   and
		   argument ( REGCOMP, value_in, eFlagsComp )
		   and
		   argument ( REGEXEC, value_in, eFlagsRegExec ) ) );

DefineFunction ( REGERROR,
		 fnShortCompileRegEx, "c-sh-compile-regex",
		 ( argument ( SHORTOBJID, value_in, oShortObjIdHeap )
		   and
		   argument ( SHORTOBJID, value_in, oShortObjIdRegEx )
		   and
		   argument ( STRING ( nRegExErrMsg ), vector_out, pszRegExErrMsg )
		   and
		   argument ( FIXNUM, value_in, nRegExErrMsg ) ) );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
