@echo off
rem PLOB dummy make for Windows/NT
rem PLOB (C) 1994-1998 Heiko Kirschke Heiko.Kirschke@acm.org
if '%1' == '' goto doconfig
if '%1' == 'config' goto doconfig
goto err
:doconfig
  bin\make-config
:err
  echo Dummy make for Windows/NT, only target "config" is supported.
:exit

rem Local variables:
rem buffer-file-coding-system: iso-latin-1-unix
rem End:
