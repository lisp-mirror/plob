/* ----------------------------------------------------------------------------
| Module	stdarg.h
| Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		27.10.92
| Description	ANSI compatible stdarg.h for SUN
 --------------------------------------------------------------------------- */

#ifndef _sys_stdarg_h
#define _sys_stdarg_h

typedef char *va_list;
#if defined(sparc)
# define va_alist __builtin_va_alist
#endif
#ifdef	__GNUC__
# define va_start(list,arg)list=((char*)&arg)+sizeof(arg)+6*sizeof(int)
#else
# define va_start(list,arg)list=((char*)&arg)+sizeof(arg)
#endif
# define va_end(list)
# if defined(__BUILTIN_VA_ARG_INCR) && !defined(lint)
#    define va_arg(list,mode) ((mode*)__builtin_va_arg_incr((mode *)list))[0]
# else
#    define va_arg(list,mode) ((mode *)(list += sizeof(mode)))[-1]
# endif

#endif /*!_sys_stdarg_h*/
