/* -------------------------------------------------------------------------
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
/**
 * 1998/02/16 HK: Hacked up version of signal.h and asm/sigcontext.h
 * found in Linux kernel version 2.0.0. Since signal.h is used in
 * Windows/NT, too, all the sigaction stuff belonging into signal.h
 * has been implemented here.
 */
#ifndef _WIN32SIG_H
#define _WIN32SIG_H

#if !defined(_WIN32)
#error ERROR: Only Win32 targets supported!
#endif

#include	<signal.h>

/*
 * As documented in the iBCS2 standard..
 *
 * The first part of "struct _fpstate" is just the
 * normal i387 hardware setup, the extra "status"
 * word is used to save the coprocessor status word
 * before entering the handler.
 */
struct _fpreg {
	unsigned short significand[4];
	unsigned short exponent;
};

struct _fpstate {
	unsigned long 	cw,
			sw,
			tag,
			ipoff,
			cssel,
			dataoff,
			datasel;
	struct _fpreg	_st[8];
	unsigned long	status;
};

struct sigcontext_struct {
	unsigned short gs, __gsh;
	unsigned short fs, __fsh;
	unsigned short es, __esh;
	unsigned short ds, __dsh;
	unsigned long edi;
	unsigned long esi;
	unsigned long ebp;
	unsigned long esp;
	unsigned long ebx;
	unsigned long edx;
	unsigned long ecx;
	unsigned long eax;
	unsigned long trapno;
	unsigned long err;
	unsigned long eip;
	unsigned short cs, __csh;
	unsigned long eflags;
	unsigned long esp_at_signal;
	unsigned short ss, __ssh;
	struct _fpstate * fpstate;
	unsigned long oldmask;
	unsigned long cr2;
};

#define SA_SHIRQ	0x04000000
#define SA_STACK	0x08000000
#define SA_RESTART	0x10000000
#define SA_INTERRUPT	0x20000000
#define SA_NOMASK	0x40000000
#define SA_ONESHOT	0x80000000

/* Type of a signal handler.  */
typedef void (*__sighandler_t)(int,struct sigcontext_struct sc );

/* Get rid of MS VC's SIG_ macros: */
#ifdef	SIG_DFL
#undef	SIG_DFL
#endif
#define SIG_DFL	((__sighandler_t)0)	/* default signal handling */
#ifdef	SIG_IGN
#undef	SIG_IGN
#endif
#define SIG_IGN	((__sighandler_t)1)	/* ignore signal */

struct sigaction {
  unsigned int		sa_flags;
  __sighandler_t	sa_handler;
};

/* The TRY_EXCEPTION ... CATCH_EXCEPTION must enclose all code blocks
   which access objects in the Stable Store directly by using
   SS_real_address() resp. SH_key_to_address() and dereference the
   pointers returned by those calls. */

#define TRY_EXCEPTION	\
{ \
  int				__nSignal__ = -1; \
  __sighandler_t		__pfnSignalHandler__ = NULL; \
  struct sigcontext_struct	__SignalContext__; \
  __try {
#define CATCH_EXCEPTION	\
  } __except ( fnHandleException ( GetExceptionCode (), \
				   GetExceptionInformation (), \
				   &__nSignal__, \
				   &__pfnSignalHandler__, \
				   &__SignalContext__ ) ) \
 { \
    if ( __pfnSignalHandler__ != NULL ) { \
      int __errno__	= errno; \
      ( * __pfnSignalHandler__ ) ( __nSignal__, __SignalContext__ ); \
      errno	= __errno__; \
    } \
 } \
}

/**
 * Exception handler, maps an exception into a signal handler call.
 */
int	fnHandleException	( DWORD		dwExceptionCode,
				  struct _EXCEPTION_POINTERS *	pException,
				  int			* pnSignal,
				  __sighandler_t	* pfnSignalHandler,
				  struct sigcontext_struct * pSignalContext );

int	sigaction	( int			__sig,
			  struct sigaction *	__act,
			  struct sigaction *	__oldact );

#endif /* _WIN32SIG_H */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
