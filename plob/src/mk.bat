@echo off
rem @echo on

if '%1' == '-goto' goto %2
goto main

:command
  shift
  shift
  set szDirectory=%1
  echo make: Entering directory `%szDirectory%'
  cd %1
  shift
  echo nmake -f %szMakefile% %1 %2 %3 %4 %5 %6 %7 %8 %9
  nmake -nologo -f %szMakefile% %1 %2 %3 %4 %5 %6 %7 %8 %9
  cd ..
  echo make: Leaving directory `%szDirectory%'
  goto return

:main
  setlocal
  set szScript=%0
  set szMakefile=win32/makefile
  call %szScript% -goto command include %1 %2 %3 %4 %5 %6 %7 %8 %9
  call %szScript% -goto command common  %1 %2 %3 %4 %5 %6 %7 %8 %9
  call %szScript% -goto command lcommon %1 %2 %3 %4 %5 %6 %7 %8 %9
  call %szScript% -goto command server  %1 %2 %3 %4 %5 %6 %7 %8 %9
  call %szScript% -goto command client  %1 %2 %3 %4 %5 %6 %7 %8 %9
  call %szScript% -goto command local   %1 %2 %3 %4 %5 %6 %7 %8 %9
  call %szScript% -goto command admin   %1 %2 %3 %4 %5 %6 %7 %8 %9
  endlocal
:return

rem Local variables:
rem buffer-file-coding-system: iso-latin-1-unix
rem End:
