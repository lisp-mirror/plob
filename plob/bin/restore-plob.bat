@echo off
rem ------------------------------------------------------------
rem 1998/05/07 Heiko Kirschke Heiko.Kirschke@acm.org
rem $Header$
rem This script is a replacement for the UNIX one-liner:
rem	cat plob-*.gz.* > plob.tar.gz
rem I *really* like DOS ... it cares for good jobs
rem like writing brain-damaged scripts like this ...
rem ------------------------------------------------------------

if '%1' == '-goto' goto %2

rem Startup with cmd.exe
cmd /C %0 -goto aftercmd %1 %2 %3 %4 %5 %6 %7 %8 %9
goto return
:aftercmd
setlocal
set szSelf=%0
shift
shift

rem ------------------------------------------------------------
rem Constants
rem ------------------------------------------------------------
set szScript=restore-plob
set szSpaces=            
set szWWW=http://www.lisp.de/software/plob
set szSourceMask=plob-*.gz.*
set szDestFile=plob.tar.gz

rem ------------------------------------------------------------
rem Variables
rem ------------------------------------------------------------

rem set szVerbose=rem
set szVerbose=echo
set szAllFiles=

rem ------------------------------------------------------------
rem Option handling
rem ------------------------------------------------------------
:loop01
  if '%1' == '' goto pool01

  if '%1' == '-h' goto help
  if '%1' == '-H' goto help
  if '%1' == '-?' goto help

  if '%1' == '+v' goto verbose0
  if '%1' == '-v' goto verbose1

  goto pool01

:verbose0
  set szVerbose=rem
  goto next01
:verbose1
  set szVerbose=echo
  goto next01

:next01
  shift
  goto loop01
:pool01
if not '%1' == '' goto xtraargs

rem ------------------------------------------------------------
rem Mainline processing
rem ------------------------------------------------------------

for %%f in (%szSourceMask%) do call %szScript% -goto loop02 %%f
goto pool02

:loop02
  shift
  shift
  if '%szAllFiles%' == '' goto else01
    set szAllFiles=%szAllFiles%+%1
    goto fi01
  :else01
    set szAllFiles=%1
  :fi01
  goto return

:pool02

%szVerbose% copy /B %szAllFiles% %szDestFile%
copy /B %szAllFiles% %szDestFile% > nul

%szVerbose%.
%szVerbose% %szScript%: PLOB archive has been restored to %szDestFile%

goto exit

rem ------------------------------------------------------------
rem Subroutines
rem ------------------------------------------------------------

:help
  echo Usage:
  echo   %szScript% [-h] [-v]
  echo Purpose:
  echo   Restore a PLOB archive from its splitted components. The resulting
  echo   complete PLOB archive is written to %szDestFile%.
  echo Options:
  echo   -h  Show this help text.
  echo   -v  Switch to verbose mode.
  echo   +v  Switch to silent mode.
  echo See Also:
  echo   %szWWW% is the official
  echo   distribution site for PLOB.
  echo Copyright:
  echo   (C) 1994--2001 Heiko Kirschke Heiko.Kirschke@acm.org
  goto exit

:xtraargs
  echo %szScript%: Error: Additional arguments have been passed.
  echo %szSpaces%         Call %szScript% -h for a help text.
  goto exit

:exit
  endlocal
:return

rem Local variables:
rem buffer-file-coding-system: iso-latin-1-unix
rem End:
