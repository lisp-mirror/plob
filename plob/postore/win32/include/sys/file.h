/**
 * 1998/02/16 HK: Hacked up version of sys/file.h found in Linux
 * kernel version 2.0.0 and of fcntl.h found in VC 5.00
 */

#ifndef _SYS_FILE_H
#define _SYS_FILE_H

#if !defined(_WIN32)
#error ERROR: Only Win32 targets supported!
#endif

#include <direct.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>

/* 1998/02/17 HK: F_ macros from asm/fcntl.h found in Linux
   kernel version 2.0.0 */
#define F_DUPFD		0	/* dup */
#define F_GETFD		1	/* get f_flags */
#define F_SETFD		2	/* set f_flags */
#define F_GETFL		3	/* more flags (cloexec) */
#define F_SETFL		4
#define F_GETLK		5
#define F_SETLK		6
#define F_SETLKW	7

#define F_SETOWN	8	/*  for sockets. */
#define F_GETOWN	9	/*  for sockets. */

/* for F_[GET|SET]FL */
#define FD_CLOEXEC	1	/* actually anything with low bit set goes */

#define O_RDONLY	_O_RDONLY
#define O_WRONLY	_O_WRONLY
#define O_RDWR		_O_RDWR
#define O_APPEND	_O_APPEND

#define O_CREAT		_O_CREAT
#define O_TRUNC		_O_TRUNC
#define O_EXCL		_O_EXCL

/* `lockf' is a simpler interface to the locking facilities of `fcntl'.
   LEN is always relative to the current file position.
   The CMD argument is one of the following.  */

#define F_ULOCK 0	/* Unlock a previously locked region.  */
#define F_LOCK  1	/* Lock a region for exclusive use.  */
#define F_TLOCK 2	/* Test and lock a region for exclusive use.  */
#define F_TEST  3	/* Test a region for other processes locks.  */

typedef	_off_t	off_t;

/* Do not shadow the original symbols lockf, open, etc., because this
   will prevent printf etc. from working. Instead, redirect calls to
   these functions by defining a macro expanding to a call to the
   effective function. */

#define	chmod	fnChmod
int	fnChmod	( const char *	__filename,
		  int		__pmode );

#define	close	fnClose
int	fnClose	( int		__fd );

#define	fcntl	fnFcntl
int	fnFcntl	( int		__fd,
		  int		__cmd,
		  ... /* long	__arg */ );

#define	fsync	fnFsync
int	fnFsync	( int		__fd );

#define	ftruncate	fnFtruncate
int	fnFtruncate	( int	__fd,
			  off_t	__len );

#define	lockf	fnLockf
int	fnLockf	( int		__fd, 
		  int		__cmd,
		  off_t		__len );

#define	lseek	fnLseek
off_t	fnLseek	( int		__fd,
		  off_t		__offset,
		  int		__whence );

#ifdef mkdir
#undef mkdir
#endif
#define mkdir	fnMkdir
int	fnMkdir	( const char *	__directory,
		  int		__mode );

#define	mkstemp	fnMkstemp
int	fnMkstemp( char *	__template );

#define	open	fnOpen
int	fnOpen	( const char *	__filename,
		  int		__oflag,
		  ... /* int	__pmode */ );


#define	read	fnRead
int	fnRead	( int		__fd,
		  void		* __buffer,
		  unsigned int	__count );

typedef int	mode_t;
#define	umask	fnUmask
mode_t	fnUmask	( mode_t	__mask );

#ifdef unlink
#undef unlink
#endif
#define	unlink	fnUnlink
int	fnUnlink( const char *	__filename );

#define	write	fnWrite
int	fnWrite	( int		__fd,
		  const void	* __buffer,
		  unsigned int	__count );

#endif /* _SYS_FILE_H */
