/**
 * 1998/02/17 HK: Created.
 */

#include	<stdarg.h>
#include	<stdio.h>

#include	"u2win32.h"
#include	"sys/file.h"

/* ------------------------------------------------------------ */
int		errno		= 0;

/* ------------------------------------------------------------ */
static mode_t	GlobalModeMask	= 0;

/* ------------------------------------------------------------ */
#define	INITIALIZE_MODULE()	\
((__bInitializeModule__)?fnInitializeFileModule(),TRUE:FALSE)

/* ------------------------------------------------------------ */
static BOOL	__bInitializeModule__	= TRUE;

/* ------------------------------------------------------------ */
static void	fnInitializeFileModule	()
{
  if ( __bInitializeModule__ ) {
    __bInitializeModule__	= FALSE;
  }
}

/* ------------------------------------------------------------ */
int	fnChmod	( const char *	__filename,
		  int		__pmode )
{
  int	nReturnCode	= -1;

  INITIALIZE_MODULE ();
  errno		= 0;

  nReturnCode	= 0;
  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnClose	( int		__fd )
{
  int		nReturnCode	= -1;
  PFDHANDLE	pFdHandle	= NULL;

  INITIALIZE_MODULE ();
  errno		= 0;

  pFdHandle	= fnFdHandle ( __fd );
  if ( pFdHandle == NULL ) {
    errno	= EBADF;
    return nReturnCode;
  }

  if ( pFdHandle->hFile == INVALID_HANDLE_VALUE ||
       ! CloseHandle ( pFdHandle->hFile ) ) {
    errno		= fnGetLastErrno ( GetLastError (), EACCES );
    pFdHandle->hFile	= INVALID_HANDLE_VALUE;
    return nReturnCode;
  }

  pFdHandle->hFile	= INVALID_HANDLE_VALUE;
  nReturnCode		= 0;
  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnFcntl	( int		__fd,
		  int		__cmd,
		  ... /* long	__arg */ )
{
  int	nReturnCode	= -1;

  INITIALIZE_MODULE ();
  errno		= 0;

  nReturnCode	= 0;
  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnFsync	( int		__fd )
{
  int		nReturnCode	= -1;
  HANDLE	hFile		= INVALID_HANDLE_VALUE;

  INITIALIZE_MODULE ();
  errno		= 0;

  hFile		= fnFdToFileHandle ( __fd );
  if ( hFile == INVALID_HANDLE_VALUE ) {
    errno	= EBADF;
    return nReturnCode;
  }

  if ( FlushFileBuffers ( hFile ) ) {
    nReturnCode	= 0;
  } else {
    errno	= fnGetLastErrno ( GetLastError (), EACCES );
  }

  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnFtruncate	( int	__fd,
			  off_t	__len )
{
  int		nReturnCode	= -1;
  HANDLE	hFile		= INVALID_HANDLE_VALUE;

  INITIALIZE_MODULE ();
  errno		= 0;

  hFile		= fnFdToFileHandle ( __fd );
  if ( hFile == INVALID_HANDLE_VALUE ) {
    errno	= EBADF;
    return nReturnCode;
  }

  if ( fnLseek ( __fd, __len, SEEK_SET ) != (off_t) -1 ) {
    if ( SetEndOfFile ( hFile ) ) {
      nReturnCode	= 0;
    } else {
      errno		= fnGetLastErrno ( GetLastError (), EACCES );
    }
  }

  nReturnCode	= 0;
  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnLockf	( int		__fd, 
		  int		__cmd,
		  off_t		__len )
{
  int		nReturnCode		= -1;
  HANDLE	hFile			= INVALID_HANDLE_VALUE;
  DWORD		dwFileOffsetLow		= 0;
  DWORD		nNumberOfBytesToLockLow	=
    ( __len == 0 ) ? (DWORD) -1 : (DWORD) __len;
  DWORD		dwFlags			=
    LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK;
  OVERLAPPED	Overlapped;

  INITIALIZE_MODULE ();
  errno		= 0;

  hFile		= fnFdToFileHandle ( __fd );
  if ( hFile == INVALID_HANDLE_VALUE ) {
    errno	= EBADF;
    return nReturnCode;
  }

  switch ( __cmd ) {
  case F_ULOCK:
    dwFileOffsetLow	= lseek ( __fd, 0, SEEK_CUR );
    if ( UnlockFile ( hFile, dwFileOffsetLow, 0,
		      nNumberOfBytesToLockLow, 0 ) ) {
      nReturnCode	= 0;
    } else {
      errno		= fnGetLastErrno ( GetLastError (), EINVAL );
    }
    break;
  case F_LOCK:
    dwFileOffsetLow	= lseek ( __fd, 0, SEEK_CUR );
    if ( LockFile ( hFile, dwFileOffsetLow, 0,
		    nNumberOfBytesToLockLow, 0 ) ) {
      nReturnCode	= 0;
    } else {
      errno		= fnGetLastErrno ( GetLastError (), EINVAL );
    }
    break;
  case F_TLOCK:
    if ( LockFileEx ( hFile, dwFlags, 0, nNumberOfBytesToLockLow, 0,
		      &Overlapped ) ) {
      nReturnCode	= 0;
    } else {
      errno		= fnGetLastErrno ( GetLastError (), EINVAL );
    }
    break;
  case F_TEST:
    /* 1998/02/17 HK: F_TEST not implemented: */
    /* break; */
  default:
    errno		= EINVAL;
    break;
  }

  return nReturnCode;
}

/* ------------------------------------------------------------ */
off_t	fnLseek	( int		__fd,
		  off_t		__offset,
		  int		__whence )
{
  off_t		nLocation	= -1;
  HANDLE	hFile		= INVALID_HANDLE_VALUE;
  LONG		lDistanceToMove	= __offset;
  DWORD		dwMoveMethod;

  INITIALIZE_MODULE ();
  errno		= 0;

  hFile		= fnFdToFileHandle ( __fd );
  if ( hFile == INVALID_HANDLE_VALUE ) {
    errno	= EBADF;
    return nLocation;
  }

  switch ( __whence ) {
  case SEEK_SET:
    dwMoveMethod	= FILE_BEGIN;
    break;
  case SEEK_CUR:
    dwMoveMethod	= FILE_CURRENT;
    break;
  case SEEK_END:
    dwMoveMethod	= FILE_END;
    break;
  default:
    errno		= EINVAL;
    return nLocation;
    break;
  }

  nLocation	= (off_t) SetFilePointer ( hFile, lDistanceToMove, NULL,
					   dwMoveMethod );
  if ( nLocation == (off_t) -1 ) {
    errno	= fnGetLastErrno ( GetLastError (), EINVAL );
  }

  return nLocation;
}

/* ------------------------------------------------------------ */
int	fnMkdir	( const char *	__directory,
		  int		__mode )
{
  int	nReturnCode	= 0;

  INITIALIZE_MODULE ();
  errno		= 0;
  
  if ( CreateDirectory ( __directory, NULL ) ) {
    nReturnCode	= 0;
  } else {
    errno	= fnGetLastErrno ( GetLastError (), EACCES );
  }

  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnMkstemp( char *	__template )
{
  int	__fd		= -1;
  char	szTempPath [ MAX_PATH ], szTempFileName [ MAX_PATH ];
  UINT	nReturnCode	= 0;

  INITIALIZE_MODULE ();
  errno		= 0;

  szTempPath [ 0 ]	= '.';
  szTempPath [ 1 ]	= '\0';
  GetTempPath ( sizeof ( szTempPath ), szTempPath );
  nReturnCode	= GetTempFileName ( szTempPath, "tmp", 0, szTempFileName );
  if ( nReturnCode == 0 ) {
    strcpy ( szTempFileName, "c:/tmp/temp.tmp" );
  }
  strcpy ( __template, szTempFileName );
  __fd	= fnOpen ( szTempFileName,
		   O_CREAT | O_RDWR | O_TEMPORARY,
		   0666 );

  return __fd;
}

/* ------------------------------------------------------------ */
int	fnOpen	( const char *	__filename,
		  int		__oflag,
		  ... /* int	__pmode */ )
{
  int		i, __pmode	= 0;
  va_list	pArg;
  PFDHANDLE	pFdHandle	= NULL;
  int		__fd		= -1;

  LPCTSTR	lpFileName		= __filename;
  DWORD		dwDesiredAccess		= GENERIC_READ;
  DWORD		dwCreationDistribution	= OPEN_EXISTING;
  DWORD		dwFlagsAndAttributes	= FILE_ATTRIBUTE_NORMAL;

  INITIALIZE_MODULE ();
  errno		= 0;

  for ( i = nFirstFdHandle; ( pFdHandle = fnFdHandle ( i ) ) != NULL; i++ ) {
    if ( pFdHandle->hFile == INVALID_HANDLE_VALUE ) {
      __fd	= i;
      break;
    }
  }

  if ( pFdHandle == NULL ) {
    /* No more free handles: */
    errno	= EMFILE;
    return __fd;
  }

  if ( ( __oflag & O_CREAT ) != 0 ) {
    va_start ( pArg, __oflag );
    __pmode	= va_arg ( pArg, int );
    va_end ( pArg );
  }

  if ( ( __oflag & O_WRONLY ) != 0 ) {
    dwDesiredAccess	= GENERIC_WRITE;
  } else if ( ( __oflag & O_RDWR ) != 0 ) {
    dwDesiredAccess	= GENERIC_READ | GENERIC_WRITE;
  }

  if ( ( __oflag & ( O_CREAT | O_EXCL ) ) == ( O_CREAT | O_EXCL ) ) {
    dwCreationDistribution	= CREATE_NEW;
  } else if ( ( __oflag & ( O_CREAT | O_TRUNC ) ) == ( O_CREAT | O_TRUNC ) ) {
    dwCreationDistribution	= CREATE_ALWAYS;
  } else if ( ( __oflag & O_CREAT ) != 0 ) {
    dwCreationDistribution	= OPEN_ALWAYS;
  } else if ( ( __oflag & O_TRUNC ) != 0 ) {
    dwCreationDistribution	= TRUNCATE_EXISTING;
  }

  if ( ( __oflag & O_TEMPORARY ) != 0 ) {
    dwFlagsAndAttributes	|= FILE_FLAG_DELETE_ON_CLOSE;
  }

  pFdHandle->hFile	=
    CreateFile (  lpFileName,
		  dwDesiredAccess,
		  FILE_SHARE_READ | FILE_SHARE_WRITE,
		  NULL,
		  dwCreationDistribution, 
		  dwFlagsAndAttributes, 
		  INVALID_HANDLE_VALUE );

  if ( pFdHandle->hFile == INVALID_HANDLE_VALUE ) {
    __fd	= -1;
    errno	= fnGetLastErrno ( GetLastError (), EACCES );
  }

  return __fd;
}

/* ------------------------------------------------------------ */
int	fnRead	( int		__fd,
		  void		* __buffer,
		  unsigned int	__count )
{
  int		nRead			= -1;
  HANDLE	hFile			= INVALID_HANDLE_VALUE;
  LPVOID	lpBuffer		= __buffer;
  DWORD		nNumberOfBytesToRead	= __count;
  DWORD		nNumberOfBytesRead	= 0;

  INITIALIZE_MODULE ();
  errno		= 0;

  hFile		= fnFdToFileHandle ( __fd );
  if ( hFile == INVALID_HANDLE_VALUE ) {
    errno	= EBADF;
    return nRead;
  }

  if ( ReadFile ( hFile, lpBuffer, nNumberOfBytesToRead,
		  &nNumberOfBytesRead, NULL ) ) {
    nRead	= (int) nNumberOfBytesRead;
  } else {
    errno	= fnGetLastErrno ( GetLastError (), EACCES );
  }

  return nRead;
}

/* ------------------------------------------------------------ */
mode_t	fnUmask	( mode_t	__mask )
{
  mode_t	OldModeMask	= GlobalModeMask;

  INITIALIZE_MODULE ();

  GlobalModeMask		= __mask;
  return OldModeMask;
}

/* ------------------------------------------------------------ */
int	fnUnlink( const char *	__filename )
{
  int	nReturnCode	= -1;

  INITIALIZE_MODULE ();
  errno		= 0;

  if ( DeleteFile ( __filename ) ) {
    nReturnCode	= 0;
  } else {
    errno	= fnGetLastErrno ( GetLastError (), EACCES );
  }

  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnWrite	( int		__fd,
		  const void	* __buffer,
		  unsigned int	__count )
{
  int		nWritten		= -1;
  HANDLE	hFile			= INVALID_HANDLE_VALUE;
  LPCVOID	lpBuffer		= __buffer;
  DWORD		nNumberOfBytesToWrite	= __count;
  DWORD		nNumberOfBytesWritten	= 0;

  INITIALIZE_MODULE ();
  errno		= 0;

  hFile		= fnFdToFileHandle ( __fd );
  if ( hFile == INVALID_HANDLE_VALUE ) {
    errno	= EBADF;
    return nWritten;
  }

  if ( WriteFile ( hFile, lpBuffer, nNumberOfBytesToWrite,
		   &nNumberOfBytesWritten, NULL ) ) {
    nWritten	= (int) nNumberOfBytesWritten;
  } else {
    errno	= fnGetLastErrno ( GetLastError (), EACCES );
  }

  return nWritten;
}

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
