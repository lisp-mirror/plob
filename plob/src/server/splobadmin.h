/* -------------------------------------------------------------------------
| Module	splobadmin.h
| Author	Heiko Kirschke
| Copyright	(C) 1998 Heiko Kirschke
| Date		1998/02/04
| Description	PLOB administration functions.
 ------------------------------------------------------------------------- */

#include	<rpc/rpc.h>
#include	<rpc/svc.h>
#include	<sys/types.h>

#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif

#include	"plobadmin.h"

/* -------------------------------------------------------------------------
| Macros
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Functions
 ------------------------------------------------------------------------- */

int	fnStorePid		( LPCSTR	pszDirectory );
void	fnUnstorePid		( LPCSTR	pszDirectory );

LPSTR	fnGetDirectoryByVersion	( LPSTR		pszDirectory,
				  size_t	nDirectory,
				  int		nVersion );
int	fnGetPortByDirectory	( const char *	pszDirectory );

int	fnGetPidByPort		( LPCSTR	pszHost,
				  LPCSTR	pszTransport,
				  int		nPort,
				  int		nTimeout /* seconds */ );

#if WIN32
typedef unsigned int	uid_t;
typedef unsigned int	gid_t;
#endif

BOOL	fnGetClientCred	( struct svc_req	* pRequest,
			  LPCSTR		pszUserName,
			  uid_t			* pnUid,
			  gid_t			* pnGid,
			  short			* pnGidMax,
			  gid_t			* pnGidList,
			  int			pnClientAddr [ 4 ],
			  LPSTR			pszClientName,
			  size_t		nClientName );

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
