/*-
 * Copyright (c) 1992, 1993, 1994 Henry Spencer.
 * Copyright (c) 1992, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer.
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
 *	@(#)regfree.c	8.3 (Berkeley) 3/20/94
 *
 * $Header$
 */

/* ----------------------------------------------------------------------- */
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"
#include	"postore.h"
#include	"plob.h"
#include	"plobintern.h"

MODULE ( __FILE__ );

#if !WIN32 && !SOLARIS
#include <sys/cdefs.h>
#endif

#include <sys/types.h>
#include <limits.h>
#include "plobregex.h"

#if !WIN32 && !SOLARIS
#include "utils.h"
#endif
#include "regex2.h"

/*
 - regfree - free everything
 = extern void regfree(regex_t *);
 */
void		fnRegFree(regex_t *preg)
{
  struct re_guts *g;

  PROCEDURE ( fnRegFree );

  if (preg->re_magic != MAGIC1)	/* oops */
    return;			/* nice to complain, but hard */

  g = preg->re_g;
  if (g == NULL || g->magic != MAGIC2)	/* oops again */
    return;
  preg->re_magic = 0;		/* mark it invalid */
  g->magic = 0;			/* mark it invalid */

  if (g->strip != NULL)
    Free((char *)g->strip);
  if (g->sets != NULL)
    Free((char *)g->sets);
  if (g->setbits != NULL)
    Free((char *)g->setbits);
  if (g->must != NULL)
    Free(g->must);
  if (g->charjump != NULL)
    Free(&g->charjump[CHAR_MIN]);
  if (g->matchjump != NULL)
    Free(g->matchjump);
  Free((char *)g);
}

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
