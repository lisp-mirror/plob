/**
 * 1998/02/16 HK: Hacked up version of asm/mman.h and sys/mman.h found
 * in Linux kernel version 2.0.0
 */

#ifndef _SYS_MMAN_H__
#define _SYS_MMAN_H__

#if !defined(_WIN32)
#error ERROR: Only Win32 targets supported!
#endif

#include	<stddef.h>	/* for size_t */
#include	<sys/types.h>

#define PROT_READ	0x1		/* page can be read */
#define PROT_WRITE	0x2		/* page can be written */
#define PROT_EXEC	0x4		/* page can be executed */
#define PROT_NONE	0x0		/* page can not be accessed */

#define MAP_SHARED	0x01		/* Share changes */
#define MAP_PRIVATE	0x02		/* Changes are private */
#define MAP_TYPE	0x0f		/* Mask for type of mapping */
#define MAP_FIXED	0x10		/* Interpret addr exactly */
#define MAP_ANONYMOUS	0x20		/* don't use a file */

#define MAP_GROWSDOWN	0x0100		/* stack-like segment */
#define MAP_DENYWRITE	0x0800		/* ETXTBSY */
#define MAP_EXECUTABLE	0x1000		/* mark it as a executable */
#define MAP_LOCKED	0x2000		/* pages are locked */

#define MS_ASYNC	1		/* sync memory asynchronously */
#define MS_INVALIDATE	2		/* invalidate the caches */
#define MS_SYNC		4		/* synchronous memory sync */

#define MCL_CURRENT	1		/* lock all current mappings */
#define MCL_FUTURE	2		/* lock all future mappings */

/* compatibility flags */
#define MAP_ANON	MAP_ANONYMOUS
#define MAP_FILE	0

typedef void *		caddr_t;

#define	getbaseaddr	fnGetBaseAddr
/* 2005-03-30 hkirschk: fnGetBaseAddr tries to find an free memory
   region at least of size __len and returns a pointer to it. The
   returned memory region can be used for subsequent calls to
   mmap(). */
caddr_t fnGetBaseAddr	( caddr_t	__proposal,
			  size_t	__len );

#define	mmap		fnMmap
caddr_t	fnMmap		( caddr_t	__addr,
			  size_t	__len,
			  int		__prot,
			  int		__flags,
			  int		__fd,
			  _off_t	__off );

#define	mprotect	fnMprotect
int	fnMprotect	( const caddr_t	__addr,
			  size_t	__len,
			  int		__prot );

#define	msync		fnMsync
int	fnMsync		( caddr_t	__addr,
			  size_t	__len,
			  int		__flags );

#define	munmap		fnMunmap
int	fnMunmap	( caddr_t	__addr,
			  size_t	__len );

#endif /* _SYS_MMAN_H__ */

/*
  Local variables:
  buffer-file-coding-system: raw-text-unix
  End:
*/
