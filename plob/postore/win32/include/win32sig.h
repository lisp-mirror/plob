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
__try {
#define CATCH_EXCEPTION	\
} __except ( fnHandleException ( GetExceptionCode (), \
				 GetExceptionInformation () ) ) {}

/**
 * Exception handler, maps an exception into a signal handler call.
 */
int	fnHandleException	( DWORD		dwExceptionCode,
				  struct _EXCEPTION_POINTERS *	pException );

int	sigaction	( int			__sig,
			  struct sigaction *	__act,
			  struct sigaction *	__oldact );

#endif /* _WIN32SIG_H */
