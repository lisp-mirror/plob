/**
 * 1998/02/17 HK: Created.
 */
#include	<stdio.h>
#include	<signal.h>

#include	"u2win32.h"
#include	"win32sig.h"

/* ------------------------------------------------------------ */
typedef struct {
  struct sigaction	Action;
}	SIGNALHANDLER, * PSIGNALHANDLER;

SIGNALHANDLER	GlobalSignalHandlers [ NSIG ];

/* ------------------------------------------------------------ */
#define	INITIALIZE_MODULE()	\
((__bInitializeModule__)?fnInitializeSiginfoModule(),TRUE:FALSE)

/* ------------------------------------------------------------ */
static BOOL	__bInitializeModule__	= TRUE;

/* ------------------------------------------------------------ */
static void	fnInitializeSiginfoModule	()
{
  int	i;
  if ( __bInitializeModule__ ) {
    __bInitializeModule__	= FALSE;
    memset ( GlobalSignalHandlers, 0, sizeof ( GlobalSignalHandlers ) );
    for ( i = 0; i < length ( GlobalSignalHandlers ); i++ ) {
      GlobalSignalHandlers [ i ].Action.sa_flags	= SA_ONESHOT;
      GlobalSignalHandlers [ i ].Action.sa_handler	= SIG_DFL;
    }
  }
}

/* ------------------------------------------------------------ */
static int	fnMapExceptionToSignal ( DWORD	dwExceptionCode )
{
  int	nSignal	= -1;

  switch ( dwExceptionCode ) {
  case STATUS_ILLEGAL_INSTRUCTION:
    nSignal	= SIGILL;
    break;
  case STATUS_FLOAT_DENORMAL_OPERAND:
  case STATUS_FLOAT_DIVIDE_BY_ZERO:
  case STATUS_FLOAT_INEXACT_RESULT:
  case STATUS_FLOAT_INVALID_OPERATION:
  case STATUS_FLOAT_OVERFLOW:
  case STATUS_FLOAT_STACK_CHECK:
  case STATUS_FLOAT_UNDERFLOW:
    nSignal	= SIGFPE;
    break;
  case STATUS_ACCESS_VIOLATION:
    nSignal	= SIGSEGV;
    break;
  default:
    break;
  }

  return nSignal;
}

/* ------------------------------------------------------------ */
static void	fnFillSignalContext	( struct sigcontext_struct *
					  pSignalContext,
					  struct _EXCEPTION_POINTERS *
					  pException,
					  DWORD		dwExceptionCode,
					  int		nSignal )
{
  memset ( pSignalContext, 0, sizeof ( *pSignalContext ) );

  if ( ( pException->ContextRecord->ContextFlags &
	 CONTEXT_FLOATING_POINT ) != 0 ) {
    /* 1998/02/18 HK: Floating point stuff not filled. */
  }

  if ( ( pException->ContextRecord->ContextFlags & CONTEXT_SEGMENTS ) != 0 ) {
    pSignalContext->gs		= LOWORD ( pException->ContextRecord->SegGs );
    pSignalContext->__gsh	= HIWORD ( pException->ContextRecord->SegGs );
    pSignalContext->fs		= LOWORD ( pException->ContextRecord->SegFs );
    pSignalContext->__fsh	= HIWORD ( pException->ContextRecord->SegFs );
    pSignalContext->es		= LOWORD ( pException->ContextRecord->SegEs );
    pSignalContext->__esh	= HIWORD ( pException->ContextRecord->SegEs );
    pSignalContext->ds		= LOWORD ( pException->ContextRecord->SegDs );
    pSignalContext->__dsh	= HIWORD ( pException->ContextRecord->SegDs );
  }

  if ( ( pException->ContextRecord->ContextFlags & CONTEXT_INTEGER ) != 0 ) {
    pSignalContext->edi		= pException->ContextRecord->Edi;
    pSignalContext->esi		= pException->ContextRecord->Esi;
    pSignalContext->ebx		= pException->ContextRecord->Ebx;
    pSignalContext->edx		= pException->ContextRecord->Edx;
    pSignalContext->ecx		= pException->ContextRecord->Ecx;
    pSignalContext->eax		= pException->ContextRecord->Eax;
  }

  if ( ( pException->ContextRecord->ContextFlags & CONTEXT_CONTROL ) != 0 ) {
    pSignalContext->ebp		= pException->ContextRecord->Ebp;
    pSignalContext->eip		= pException->ContextRecord->Eip;
    pSignalContext->cs		= LOWORD ( pException->ContextRecord->SegCs );
    pSignalContext->__csh	= HIWORD ( pException->ContextRecord->SegCs );
    pSignalContext->eflags	= pException->ContextRecord->EFlags;
    pSignalContext->esp		= pException->ContextRecord->Esp;
    pSignalContext->ss		= LOWORD ( pException->ContextRecord->SegSs );
    pSignalContext->__ssh	= HIWORD ( pException->ContextRecord->SegSs );
  }
  pSignalContext->trapno	= dwExceptionCode;
  pSignalContext->err		= nSignal;
  pSignalContext->cr2		=
    ( pException->ExceptionRecord->NumberParameters >= 2 ) ?
    pException->ExceptionRecord->ExceptionInformation [ 1 ] : 0;
}

/* ------------------------------------------------------------ */
int	fnHandleException	( DWORD		dwExceptionCode,
				  struct _EXCEPTION_POINTERS *	pException )
{
  int			nSignal		= -1;
  int			nErrNo		= errno;
  int			nContinue	= EXCEPTION_CONTINUE_SEARCH;
  PSIGNALHANDLER	pHandler	= NULL;
  struct sigcontext_struct	SignalContext;
  __sighandler_t	pFunction	= NULL;

  INITIALIZE_MODULE ();

  nSignal	= fnMapExceptionToSignal ( dwExceptionCode );
  if ( nSignal >= 0 ) {
    pHandler	= &GlobalSignalHandlers [ nSignal ];
    pFunction	= pHandler->Action.sa_handler;
    if ( pFunction == SIG_IGN ) {
      nContinue	= EXCEPTION_CONTINUE_EXECUTION;
    } else if ( pFunction == SIG_DFL ) {
      DebugBreak ();
      exit ( 3 );
    } else {
      nContinue	= EXCEPTION_CONTINUE_EXECUTION;
      fnFillSignalContext ( &SignalContext,
			    pException, dwExceptionCode, nSignal );
      if ( ( GlobalSignalHandlers [ nSignal ].Action.sa_flags &
	     SA_ONESHOT ) != 0 ) {
	pHandler->Action.sa_handler	= SIG_DFL;
      }
      ( * pFunction ) ( nSignal, SignalContext );
    }
  }

  errno	= nErrNo;
  return nContinue;
}

/* ------------------------------------------------------------ */
int	sigaction	( int			__sig,
			  struct sigaction *	__act,
			  struct sigaction *	__oldact )
{
  int	nReturnCode	= -1;

  INITIALIZE_MODULE ();
  errno		= 0;

  if ( 0 <= __sig && __sig < length ( GlobalSignalHandlers ) ) {
    nReturnCode	= 0;
    if ( __oldact != NULL ) {
      *__oldact	= GlobalSignalHandlers [ __sig ].Action;
    }
    if ( __act != NULL ) {
      GlobalSignalHandlers [ __sig ].Action	= *__act;
    }
  } else {
    errno	= EINVAL;
  }

  return nReturnCode;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
