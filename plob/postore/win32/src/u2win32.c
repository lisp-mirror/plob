/**
 * 1998/02/17 HK: Created.
 */
#include	"u2win32.h"

/* ------------------------------------------------------------ */
/* #define	DEBUG 0 */
/* #define	DEBUG 1 */
#define	DEBUG 0

/* ------------------------------------------------------------ */
static FDHANDLE		GlobalFdHandles [ 8 ];

/* ------------------------------------------------------------ */
#define	INITIALIZE_MODULE()	\
((__bInitializeModule__)?fnInitializeU2win32Module(),TRUE:FALSE)

/* ------------------------------------------------------------ */
static BOOL	__bInitializeModule__	= TRUE;

/* ------------------------------------------------------------ */
static void	fnInitializeU2win32Module	()
{
  int		i;
  PFDHANDLE	pFdHandle	= NULL;

  if ( __bInitializeModule__ ) {
    __bInitializeModule__	= FALSE;
    for ( i = 0; i < length ( GlobalFdHandles ); i++ ) {
      pFdHandle			= & GlobalFdHandles [ i ];
      memset ( pFdHandle, 0, sizeof ( *pFdHandle ) );
      pFdHandle->hFile		= INVALID_HANDLE_VALUE;
    }
  }
}

/* ------------------------------------------------------------ */
PFDHANDLE	fnFdHandle		( int	__fd )
{
  PFDHANDLE	pFdHandle	= NULL;

  INITIALIZE_MODULE ();

  pFdHandle	= ( nFirstFdHandle <= __fd &&
		    __fd < nFirstFdHandle + length ( GlobalFdHandles ) ) ?
    & GlobalFdHandles [ __fd - nFirstFdHandle ] : NULL;

  return pFdHandle;
}

/* ------------------------------------------------------------ */
HANDLE		fnFdToFileHandle	( int	__fd )
{
  HANDLE	hFile		= INVALID_HANDLE_VALUE;
  PFDHANDLE	pFdHandle	= NULL;

  INITIALIZE_MODULE ();

  pFdHandle	= fnFdHandle ( __fd );
  if ( pFdHandle != NULL ) {
    hFile	= pFdHandle->hFile;
  }
  return hFile;
}

/* ------------------------------------------------------------ */
int		fnGetLastErrno		 ( DWORD	dwLastError,
					   int		nDefaultErrNo )
{
  int	nErrNo	= 0;

  INITIALIZE_MODULE ();

#if (DEBUG+0) > 0
  /* 1998/02/19 HK: Debug: */
  {
    char	szBuffer [ 256 ];
    FormatMessage ( FORMAT_MESSAGE_FROM_SYSTEM, NULL,
		    GetLastError (), 0,
		    szBuffer, sizeof ( szBuffer ), NULL );
    printf ( "dwLastError %lu: %s\n", dwLastError, szBuffer );
  }
#endif

  switch ( dwLastError ) {
  case ERROR_FILE_NOT_FOUND:
  case ERROR_PATH_NOT_FOUND:
  case ERROR_ACCESS_DENIED:
  case ERROR_INVALID_DRIVE:
  case ERROR_WRITE_FAULT:
  case ERROR_READ_FAULT:
  case ERROR_INVALID_ADDRESS:
    nErrNo	= EACCES;
    break;
  case ERROR_LOCK_VIOLATION:
    nErrNo	= EAGAIN;
    break;
  case ERROR_NOT_ENOUGH_MEMORY:
  case ERROR_OUTOFMEMORY:
    nErrNo	= EAGAIN;
    break;
  case ERROR_SEEK:
  case ERROR_INVALID_PARAMETER:
    nErrNo	= EINVAL;
    break;
  case ERROR_FILE_EXISTS:
    nErrNo	= EEXIST;
    break;
  default:
    nErrNo	= nDefaultErrNo;
    break;
  }
  return nErrNo;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
