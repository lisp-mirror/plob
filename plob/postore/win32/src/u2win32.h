/**
 * 1998/02/17 HK: Created.
 */
#include	<windows.h>
#include	<errno.h>

/* ------------------------------------------------------------ */
/* 2005-03-29 hkirschk: Quick and dirty debug support: */
#if (DEBUG+0) > 0

#include	"../../../src/include/global.h"

#else

#define	INFO( format )

/* ------------------------------------------------------------ */
#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
typedef enum { FALSE, TRUE } BOOL;


/* ------------------------------------------------------------ */
/**
 * Get the length of an array.
 */
#define	length( array )		(sizeof(array)/sizeof((array)[0]))

#endif


/* ------------------------------------------------------------ */
typedef struct {
  HANDLE	hFile;
}	FDHANDLE, * PFDHANDLE;
enum { nFirstFdHandle	= 3 };


/* ------------------------------------------------------------ */
/**
 * Map a low-level file handle to a PFDHANDLE.
 */
PFDHANDLE	fnFdHandle		( int	__fd );

/* ------------------------------------------------------------ */
/**
 * Map a low-level file handle to a Windows/NT HANDLE.
 */
HANDLE		fnFdToFileHandle	( int	__fd );

/* ------------------------------------------------------------ */
/**
 * Map the return value of GetLastError to errno.
 */
int		fnGetLastErrno		 ( DWORD	dwLastError,
					   int		nDefaultErrNo );

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
