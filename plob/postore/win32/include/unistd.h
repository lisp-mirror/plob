/**
 * 1998/02/18 HK: Hacked up version of unistd.h found in Linux
 * kernel version 2.0.0
 */
#ifndef	_UNISTD_H
#define	_UNISTD_H

#define	getpagesize	fnGetpagesize
size_t	fnGetpagesize	( void );

#endif /* unistd.h  */

