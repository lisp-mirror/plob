/**
 * 1998/02/17 HK: Created.
 */
#include	<windows.h>
#include	<errno.h>

/* ------------------------------------------------------------ */
#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
typedef enum { FALSE, TRUE } BOOL;

/* ------------------------------------------------------------ */
typedef struct {
  HANDLE	hFile;
}	FDHANDLE, * PFDHANDLE;
enum { nFirstFdHandle	= 3 };

/* ------------------------------------------------------------ */
/**
 * Get the length of an array.
 */
#define	length( array )		(sizeof(array)/sizeof((array)[0]))

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
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
