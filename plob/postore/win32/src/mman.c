/**
 * 1998/02/17 HK: Created.
 *
 * The NT functions used for emulating mmap() and munmap() are not
 * very robust. For example, munmap()ping memory regions which have
 * not been mmap()ped will result in the memory region found before
 * the passed address to get unmapped.
 *
 * To avoid such unwanted surprises, all mmap() requests are broken
 * down internally into single page mappings, and all page mappings are
 * recorded in a bit map, with a bit set for each mapped page.
 *
 */

#include	<stdio.h>

#include	"u2win32.h"

#include	"sys/mman.h"

/* ------------------------------------------------------------ */
/* #define	DEBUG 0 */
/* #define	DEBUG 1 */

#if (DEBUG+0) > 0

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

#endif

/* ------------------------------------------------------------ */
static const char __file__ []	= __FILE__;

/* ------------------------------------------------------------ */
#define	PAGE_WRITEMODE	PAGE_READWRITE
#define FILE_WRITEMODE	FILE_MAP_WRITE

/* ------------------------------------------------------------ */
typedef struct {
  DWORD		dwBits;
  LPDWORD	pdwBits;
}	BITVECTOR, * PBITVECTOR;

/* ------------------------------------------------------------ */
static PBITVECTOR	fnBitVectorCreate	( PBITVECTOR	pBitVector,
						  DWORD		dwBits )
{
  size_t	nBytes;

  pBitVector->dwBits	= dwBits;
  nBytes		= ( pBitVector->dwBits + 7 ) / 8;
  /* Round up nBytes to next multiple of
     sizeof ( *(pBitVector->pdwBits) ): */
  nBytes		=
    sizeof ( *(pBitVector->pdwBits) ) *
    ( ( nBytes + sizeof ( *(pBitVector->pdwBits) ) - 1 ) /
      sizeof ( *(pBitVector->pdwBits) ) );
  if ( nBytes <= 0 ) {
    fprintf ( stderr, "%s(%d): Attempt to allocate a 0-length bit vector.\n",
	      __file__, __LINE__ );
    DebugBreak ();
  }
  pBitVector->pdwBits	= malloc ( nBytes );
  memset ( pBitVector->pdwBits, 0, nBytes );

  return pBitVector;
}

/* ------------------------------------------------------------ */
static PBITVECTOR	fnBitVectorDestroy	( PBITVECTOR	pBitVector  )
{
  pBitVector->dwBits	= 0;

  if ( pBitVector->pdwBits != NULL ) {
    free ( pBitVector->pdwBits );
    pBitVector->pdwBits	= NULL;
  }
  return pBitVector;
}

/* ------------------------------------------------------------ */
static void		fnBitVectorGetError	( PBITVECTOR	pBitVector,
						  DWORD		dwLocation )
{
  fprintf ( stderr, "%s(%d): Attempt to read from invalid bit vector\n"
	    "location. Bit vector length %lu, location %lu.\n",
	    __file__, __LINE__,
	    pBitVector->dwBits, dwLocation );
  DebugBreak ();
}

/* ------------------------------------------------------------ */
static __inline BOOL	fnBitVectorGet		( PBITVECTOR	pBitVector,
						  DWORD		dwLocation )
{
  BOOL	bOn			= FALSE;

  if ( dwLocation >= pBitVector->dwBits ) {
    fnBitVectorGetError ( pBitVector, dwLocation );
  } else {
    bOn	=
      ( pBitVector->pdwBits [ dwLocation /
			      ( sizeof ( *(pBitVector->pdwBits) ) * 8 ) ] &
	( 1 <<
	  ( dwLocation % ( sizeof ( *(pBitVector->pdwBits) ) * 8 ) ) ) ) != 0;
  }
  return bOn;
}

/* ------------------------------------------------------------ */
static void		fnBitVectorSetError	( PBITVECTOR	pBitVector,
						  DWORD		dwLocation )
{
  fprintf ( stderr, "%s(%d): Attempt to write to invalid bit vector\n"
	    "location. Bit vector length %lu, location %lu.\n",
	    __file__, __LINE__,
	    pBitVector->dwBits, dwLocation );
  DebugBreak ();
}

/* ------------------------------------------------------------ */
static BOOL		fnBitVectorSet		( PBITVECTOR	pBitVector,
						  DWORD		dwLocation,
						  BOOL		bOn )
{
  BOOL	bOldOn			= FALSE;
  DWORD	dwDwordLocation		= 0;
  int	nInDwordLocation	= 0;
  DWORD	dwOnMask		= 0;

  if ( dwLocation >= pBitVector->dwBits ) {
    fnBitVectorSetError ( pBitVector, dwLocation );
  } else {
    dwDwordLocation	=
      dwLocation / ( sizeof ( *(pBitVector->pdwBits) ) * 8 );
    nInDwordLocation	=
      dwLocation % ( sizeof ( *(pBitVector->pdwBits) ) * 8 );
    dwOnMask		= ( 1 << nInDwordLocation );
    bOldOn		=
      ( pBitVector->pdwBits [ dwDwordLocation ] & dwOnMask ) != 0;
    if ( bOn ) {
      pBitVector->pdwBits [ dwDwordLocation ]	|= dwOnMask;
    } else {
      pBitVector->pdwBits [ dwDwordLocation ]	&= ~dwOnMask;
    }

  }

  return bOldOn;
}

/* ------------------------------------------------------------ */
/**
 * One bit for each mmap()ped page.
 */
static BITVECTOR	GlobalMmapEntries;

/**
 * The page granularity of Windows/NT. Pages and lengths given to
 * mmap() must fall onto multiples of dwPageGranularity resp.
 * dwLengthGranularity.
 */
static DWORD	dwGlobalPageGranularity;
static DWORD	dwGlobalLengthGranularity;

/**
 * The min. resp. max page number encountered so far with mmap().
 */
static DWORD	dwGlobalMinPageNo	= 0;
static DWORD	dwGlobalMaxPageNo	= 0;

/* ------------------------------------------------------------ */
#define	INITIALIZE_MODULE()	\
((__bInitializeModule__)?fnInitializeMmanModule(),TRUE:FALSE)

/* ------------------------------------------------------------ */
static BOOL	__bInitializeModule__	= TRUE;

/* ------------------------------------------------------------ */
static void	fnInitializeMmanModule	()
{
  if ( __bInitializeModule__ ) {
    SYSTEM_INFO	SystemInfo;
    DWORD	dwBits;
    __bInitializeModule__	= FALSE;
    GetSystemInfo ( &SystemInfo );
    dwGlobalPageGranularity	= SystemInfo.dwAllocationGranularity;
    dwGlobalLengthGranularity	= SystemInfo.dwAllocationGranularity;
    /* Number of pages in units of SystemInfo.dwAllocationGranularity: */
    dwBits	= 0x40000000 / ( dwGlobalPageGranularity / 4 );
    fnBitVectorCreate ( &GlobalMmapEntries, dwBits );
  }
}

/* ------------------------------------------------------------ */
caddr_t	fnGetBaseAddr		( caddr_t	__proposal,
				  size_t	__len )
{
  LPVOID			pFound = NULL;
  LPVOID			p = NULL;
  MEMORY_BASIC_INFORMATION	m;
  size_t			maxSize = 0;
  LPVOID			maxP = NULL;

#if (DEBUG+0) > 0

  PROCEDURE ( fnGetBaseAddr );
#endif

  INITIALIZE_MODULE ();

  INFO (( " ( proposal 0x%p (%d), len 0x%X (%lu, %lu m) )",
	  __proposal, __proposal, __len, __len, __len/1024/1024 ));

  while ( pFound == NULL && ( (DWORD) p ) < 0x80000000 ) {
    VirtualQuery ( p, &m, sizeof ( m ) );
    if ( m.State == MEM_FREE ) {
#if (DEBUG+0) > 0
      INFO (( "Found free memory at 0x%p size 0x%X (%lu, %lu m)",
	      m.BaseAddress, m.RegionSize, m.RegionSize,
	      m.RegionSize / 1024 / 1024 ));
#endif
      if ( maxSize < m.RegionSize ) {
	maxSize	= m.RegionSize;
	maxP	= m.BaseAddress;
      }
      p = (LPVOID) ( ( (char *) p ) + m.RegionSize );
    } else if ( m.RegionSize > dwGlobalPageGranularity ) {
	p = (LPVOID) ( ( (char *) p ) + m.RegionSize );
    } else {
	p = (LPVOID) ( ( (char *) p ) + dwGlobalPageGranularity );
    }
  }

  pFound	= ( maxSize >= __len ) ? maxP : NULL;

#if (DEBUG+0) > 0
  INFO (( "__len - maxSize = 0x%X (%ld), maxP 0x%p, returns 0x%p",
	  __len - maxSize, __len - maxSize, maxP, pFound ));
#endif

  return (caddr_t) pFound;
} /* fnGetBaseAddr */

/* ------------------------------------------------------------ */
caddr_t	fnMmap		( caddr_t	__addr,
			  size_t	__len,
			  int		__prot,
			  int		__flags,
			  int		__fd,
			  off_t		__off )
{
  caddr_t	pMapped			= (caddr_t) -1;
  HANDLE	hFile			= INVALID_HANDLE_VALUE;
  HANDLE	hMapping		= INVALID_HANDLE_VALUE;
  DWORD		flProtect		= PAGE_WRITEMODE;
  DWORD		dwDesiredAccess		= FILE_WRITEMODE;
  DWORD		dwFileOffsetLow		= __off;
  DWORD		dwNumberOfBytesToMap	= __len;
  LPVOID	lpBaseAddress		= __addr;
  LPVOID	pPage = NULL, pMap = NULL;
  DWORD		firstPage = 0, pages = 0, page;
  BITVECTOR	myMappings;

#if (DEBUG+0) > 0
  PROCEDURE ( fnMmap );
#endif

  INITIALIZE_MODULE ();
  errno		= 0;

  hFile		= fnFdToFileHandle ( __fd );
  if ( hFile == INVALID_HANDLE_VALUE ) {
#if (DEBUG+0) > 0
    INFO ((  "Invalid file handle %d.", __fd ));
#endif
    errno	= EBADF;
    return pMapped;
  }

#if (DEBUG+0) > 0
  {
    const char	* pszProtect, * pszAccess;

     /* 1998/02/20 HK: Debug: */
    switch ( flProtect ) {
    case PAGE_READONLY:
      pszProtect	= "PAGE_READONLY";
      break;
    case PAGE_READWRITE:
      pszProtect	= "PAGE_READWRITE";
      break;
    case PAGE_WRITECOPY:
      pszProtect	= "PAGE_WRITECOPY";
      break;
    default:
      pszProtect	= "???";
      break;
    }
    switch ( dwDesiredAccess ) {
    case FILE_MAP_READ:
      pszAccess		= "FILE_MAP_READ";
      break;
    case FILE_MAP_WRITE:
      pszAccess		= "FILE_MAP_WRITE";
      break;
    case FILE_MAP_COPY:
      pszAccess		= "FILE_MAP_COPY";
      break;
    case FILE_MAP_ALL_ACCESS:
      pszAccess		= "FILE_MAP_ALL_ACCESS";
      break;
    default:
      pszAccess		= "???";
      break;
    }
    INFO (( " ( base 0x%p (%d), len 0x%X (%d), prot %s, access %s )",
	    __addr, __addr, __len, __len, pszProtect, pszAccess ));
  }
#endif

  if ( ( ( (DWORD) lpBaseAddress ) % dwGlobalPageGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched base" ));
#endif
    errno	= EINVAL;
    return pMapped;
  }

  if ( ( dwNumberOfBytesToMap % dwGlobalLengthGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched len" ));
#endif
    errno	= EINVAL;
    return pMapped;
  }

  /* 2002-04-16 HK: The file mapping has always to be done read/write,
     since all other modes will prevent an upgrading e.g. from
     read-only to write at mprotect.
   */
  hMapping = CreateFileMapping ( hFile, NULL, flProtect, 0, 0, NULL );
  if ( hMapping == NULL || hMapping == INVALID_HANDLE_VALUE ) {
#if (DEBUG+0) > 0
    INFO (( "CreateFileMapping failed, GetLastError() %d", GetLastError () ));
#endif
    hMapping	= INVALID_HANDLE_VALUE;
    errno	= EBADF;
    return pMapped;
  }

  pages		= dwNumberOfBytesToMap / dwGlobalLengthGranularity;
  fnBitVectorCreate ( &myMappings, pages );
  for ( page = 0, pPage = lpBaseAddress;
	page < pages;
	page++,
	  dwFileOffsetLow += dwGlobalLengthGranularity,
	  pPage = (LPVOID)
	  ( ( (char *) pPage ) + dwGlobalLengthGranularity ) ) {
    /* Check if the region is already mapped in: */
    BOOL	bMapIt	=
      ( firstPage == 0 ) ?
      TRUE : ! fnBitVectorGet ( &GlobalMmapEntries, firstPage + page );
    pMap	= ( bMapIt ) ?
      MapViewOfFileEx ( hMapping, dwDesiredAccess,
			0, dwFileOffsetLow,
			dwGlobalLengthGranularity, pPage ) : pPage;
#if (DEBUG+0) > 0
    /* 1998/02/25 HK: Debug: */
    if ( bMapIt ) {
      INFO (( "Mapped 0x%p, len 0x%X (%d) to 0x%p, GetLastError () %d",
	      pPage, dwGlobalLengthGranularity,
	      dwGlobalLengthGranularity, pMap, GetLastError () ));
    }
#endif
    if ( pMap == NULL ) {
      /* Error; no mapping was established. Rollback all mapping
	 operations done up to now. */
      if ( pMapped != (caddr_t) -1 ) {
	DWORD	p;
	for ( p = 0, pPage = pMapped; p < page;
	      p++, pPage = (LPVOID)
		( ( (char *) pPage ) + dwGlobalLengthGranularity ) ) {
	  if ( fnBitVectorGet ( &myMappings, p ) ) {
	    UnmapViewOfFile ( pPage );
	    fnBitVectorSet ( &GlobalMmapEntries, firstPage + p, FALSE );
	  }
	}
	pMapped	= (caddr_t) -1;
      }
      break;
    }
    if ( page == 0 ) {
      firstPage	= ( (DWORD) pMap ) / dwGlobalPageGranularity;
      pPage	= pMap;
      pMapped	= (caddr_t) pMap;
      if ( bMapIt &&
	   ( dwGlobalMinPageNo == 0 || dwGlobalMinPageNo > firstPage ) ) {
	dwGlobalMinPageNo	= firstPage;
      }
    }
    fnBitVectorSet ( &GlobalMmapEntries, firstPage + page, bMapIt );
    fnBitVectorSet ( &myMappings, page, bMapIt );
    if ( bMapIt &&
	 ( dwGlobalMaxPageNo == 0 || dwGlobalMaxPageNo < firstPage + page ) ) {
      dwGlobalMaxPageNo	= firstPage + page;
    }
  }

  if ( pMapped == (caddr_t) -1 ) {
    int	nErrNo;
    nErrNo	= fnGetLastErrno ( GetLastError (), EINVAL );
#if (DEBUG+0) > 0
    INFO (( "returns 0x%p, errno %d", pMapped, nErrNo ));
#endif
    errno	= nErrNo;
  }
#if (DEBUG+0) > 0
  else {
    INFO (( "returns 0x%p (ok)", pMapped ));
  }
#endif

  fnBitVectorDestroy ( &myMappings );
  CloseHandle ( hMapping );
  hMapping	= INVALID_HANDLE_VALUE;

  return pMapped;
}

/* ------------------------------------------------------------ */
int	fnMprotect	( const caddr_t	__addr,
			  size_t	__len,
			  int		__prot )
{
  int		nReturnCode	= -1;
  LPVOID	lpBaseAddress	= __addr;
  DWORD		dwSize		= __len;
  DWORD		flProtect	= PAGE_READONLY;
  LPDWORD	pflOldProtects	= NULL;
  LPVOID	pPage		= NULL;
  DWORD		firstPage = 0, pages = 0, page;
  BITVECTOR	myProtects;

#if (DEBUG+0) > 0
  PROCEDURE ( fnMprotect );
#endif

  INITIALIZE_MODULE ();
  errno		= 0;

  if ( __prot == PROT_NONE ) {
    flProtect	= PAGE_NOACCESS;
  } else if ( ( __prot & ( PROT_READ | PROT_WRITE ) ) ==
       ( PROT_READ | PROT_WRITE ) ) {
    flProtect	= PAGE_WRITEMODE;
  }

#if (DEBUG+0) > 0
  { /* 1998/02/20 HK: Debug: */
    const char	* pszProtect;
    switch ( flProtect ) {
    case PAGE_NOACCESS:
      pszProtect	= "PAGE_NOACCESS";
      break;
    case PAGE_READONLY:
      pszProtect	= "PAGE_READONLY";
      break;
    case PAGE_READWRITE:
      pszProtect	= "PAGE_READWRITE";
      break;
    case PAGE_WRITECOPY:
      pszProtect	= "PAGE_WRITECOPY";
      break;
    default:
      pszProtect	= "???";
      break;
    }
    INFO (( " ( base 0x%p (%d), len 0x%X (%d), prot %s )",
	    __addr, __addr, __len, __len, pszProtect ));
  }
#endif

  if ( ( ( (DWORD) lpBaseAddress ) % dwGlobalPageGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched base" ));
#endif
    errno	= EINVAL;
    return nReturnCode;
  }

  if ( ( dwSize % dwGlobalLengthGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched len" ));
#endif
    errno	= EINVAL;
    return nReturnCode;
  }

  pages			= dwSize / dwGlobalLengthGranularity;
  firstPage		= ( (DWORD) lpBaseAddress ) / dwGlobalPageGranularity;
  fnBitVectorCreate ( &myProtects, pages );
  pflOldProtects	= malloc ( pages * sizeof ( *pflOldProtects ) );
  nReturnCode		= 0;
  for ( page = 0, pPage = lpBaseAddress;
	page < pages;
	page++,
	  pPage = (LPVOID)
	  ( ( (char *) pPage ) + dwGlobalLengthGranularity ) ) {
    BOOL	bProtectIt	= 
      /* Check if the region is mapped in: */
      fnBitVectorGet ( &GlobalMmapEntries, firstPage + page );
    if ( bProtectIt ) {
      if ( VirtualProtect ( pPage, dwGlobalLengthGranularity,
			    flProtect, &pflOldProtects [ page ] ) ) {
	fnBitVectorSet ( &myProtects, page, TRUE );
      } else {
	/* Rollback all protections done here: */
	DWORD	p, flOldProtect;
#if (DEBUG+0) > 0
	INFO (( "VirtualProtect failed for page no. %lu at 0x%p, GetLastError () %d",
		page, pPage, GetLastError () ));
#endif
	for ( p = 0, pPage = lpBaseAddress; p < page;
	      p++, pPage = (LPVOID)
		( ( (char *) pPage ) + dwGlobalLengthGranularity ) ) {
	  if ( fnBitVectorGet ( &myProtects, p ) ) {
	    VirtualProtect ( pPage, dwGlobalLengthGranularity,
			     pflOldProtects [ page ], &flOldProtect );
	  }
	}
	nReturnCode	= -1;
	break;
      }
    }
  }

  free ( pflOldProtects );
  pflOldProtects	= NULL;
  fnBitVectorDestroy ( &myProtects );

  if ( nReturnCode < 0 ) {
    int	nErrNo;
    nErrNo	= fnGetLastErrno ( GetLastError (), EINVAL );
#if (DEBUG+0) > 0
    INFO (( "returns %d, errno %d", nReturnCode, nErrNo ));
#endif
    errno	= nErrNo;
  }
#if (DEBUG+0) > 0
  else {
    INFO (( "returns %d (ok)", nReturnCode ));
  }
#endif

  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnMsync		( caddr_t	__addr,
			  size_t	__len,
			  int		__flags )
{
  int		nReturnCode		= -1;
  LPVOID	lpBaseAddress		= __addr;
  DWORD		dwNumberOfBytesToFlush	= __len;
  LPVOID	pPage			= NULL;
  DWORD		firstPage = 0, pages = 0, page;

#if (DEBUG+0) > 0
  PROCEDURE ( fnMsync );
#endif

  INITIALIZE_MODULE ();
  errno		= 0;

#if (DEBUG+0) > 0
  /* 1998/02/20 HK: Debug: */
  INFO (( " ( base 0x%p (%d), len 0x%X (%d) )", __addr, __addr, __len, __len ));
#endif

  if ( ( ( (DWORD) lpBaseAddress ) % dwGlobalPageGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched base" ));
#endif
    errno	= EINVAL;
    return nReturnCode;
  }

  if ( ( dwNumberOfBytesToFlush % dwGlobalLengthGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched len" ));
#endif
    errno	= EINVAL;
    return nReturnCode;
  }

  pages		= dwNumberOfBytesToFlush / dwGlobalLengthGranularity;
  firstPage	= ( (DWORD) lpBaseAddress ) / dwGlobalPageGranularity;
  nReturnCode	= 0;
  for ( page = 0, pPage = lpBaseAddress;
	page < pages;
	page++,
	  pPage = (LPVOID)
	  ( ( (char *) pPage ) + dwGlobalLengthGranularity ) ) {
    BOOL	bFlushIt	= 
      /* Check if the region is mapped in: */
      fnBitVectorGet ( &GlobalMmapEntries, firstPage + page );
    if ( bFlushIt ) {
      if ( ! FlushViewOfFile ( lpBaseAddress, dwGlobalLengthGranularity ) ) {
#if (DEBUG+0) > 0
	INFO (( "FlushViewOfFile failed at base 0x%p, len %lu, GetLastError() %d",
		lpBaseAddress, dwGlobalLengthGranularity, GetLastError () ));
#endif
	nReturnCode	= -1;
	break;
      }
    }
  }

  if ( nReturnCode < 0 ) {
    int	nErrNo;
    nErrNo	= fnGetLastErrno ( GetLastError (), EINVAL );
#if (DEBUG+0) > 0
    INFO (( "returns %d, errno %d", nReturnCode, nErrNo ));
#endif
    errno	= nErrNo;
  }
#if (DEBUG+0) > 0
  else {
    INFO (( "returns %d (ok)", nReturnCode ));
  }
#endif

  return nReturnCode;
}

/* ------------------------------------------------------------ */
int	fnMunmap	( caddr_t	__addr,
			  size_t	__len )
{
  int		nReturnCode		= -1;
  LPVOID	lpBaseAddress		= __addr;
  DWORD		dwNumberOfBytesToUnmap	= __len;
  LPVOID	pPage			= NULL;
  DWORD		firstPage = 0, pages = 0, page;

#if (DEBUG+0) > 0
  PROCEDURE ( fnMunmap );
#endif

  INITIALIZE_MODULE ();
  errno		= 0;

#if (DEBUG+0) > 0
  /* 1998/02/20 HK: Debug: */
  INFO (( " ( base 0x%p (%d), len 0x%X (%d) )", __addr, __addr, __len, __len ));
#endif

  if ( ( ( (DWORD) lpBaseAddress ) % dwGlobalPageGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched base" ));
#endif
    errno	= EINVAL;
    return nReturnCode;
  }

  if ( ( dwNumberOfBytesToUnmap % dwGlobalLengthGranularity ) != 0 ) {
#if (DEBUG+0) > 0
    INFO (( "Mismatched len" ));
#endif
    errno	= EINVAL;
    return nReturnCode;
  }

  pages		= dwNumberOfBytesToUnmap / dwGlobalLengthGranularity;
  firstPage	= ( (DWORD) lpBaseAddress ) / dwGlobalPageGranularity;
  nReturnCode	= 0;
  for ( page = 0, pPage = lpBaseAddress;
	page < pages;
	page++,
	  pPage = (LPVOID)
	  ( ( (char *) pPage ) + dwGlobalLengthGranularity ) ) {
    BOOL	bUnmapIt	= 
      /* Check if the region is mapped in: */
      fnBitVectorGet ( &GlobalMmapEntries, firstPage + page );
    if ( bUnmapIt ) {
#if (DEBUG+0) > 0
      /* 1998/02/25 HK: Debug: */
      INFO (( "Unmapping 0x%p", pPage ));
#endif
      if ( UnmapViewOfFile ( pPage ) ) {
	fnBitVectorSet ( &GlobalMmapEntries, firstPage + page, FALSE );
      } else {
#if (DEBUG+0) > 0
      /* 1998/02/25 HK: Debug: */
      INFO (( "UnmapViewOfFile failed at page 0x%p, GetLastError %d",
	      pPage, GetLastError () ));
#endif
	nReturnCode	= -1;
      }
    }
  }

#if (DEBUG+0) > 0
  INFO (( "returns %d", nReturnCode ));
#endif

  return nReturnCode;
}

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
