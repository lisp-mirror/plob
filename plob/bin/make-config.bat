@echo off
rem PLOB installation script for Windows/NT
rem PLOB (C) 1994--1998 Heiko Kirschke kirschke@informatik.uni-hamburg.de

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
set szScript=make-config
set szSpaces=           
set szDefaultSystemRoot=c:\winnt
set szDefaultSystemRoots=%szDefaultSystemRoot% d:\winnt c:\windows d:\windows
set szDefaultPlobRoot=.
set szOncRpc=oncrpc-1.12
set szSystemVers=win32
set szWWW=http://www.lisp.de/software/plob
set szDefaultDataRootDrv=c
set szDefaultDataRootDir=\opt\data\plob

rem ------------------------------------------------------------
rem Variables
rem ------------------------------------------------------------
rem set szVerbose=rem
set szVerbose=echo
set bOncRpcInstalled=

rem ------------------------------------------------------------
rem Option handling
rem ------------------------------------------------------------
if '%SystemRoot%' == '' goto fi01
  set szDefaultSystemRoot=%SystemRoot%
:fi01
:loop01
  if '%1' == '' goto pool01

  if '%1' == '-h' goto help
  if '%1' == '-H' goto help
  if '%1' == '-?' goto help

  if '%1' == '+d' goto datroot0
  if '%1' == '-d' goto datroot1

  if '%1' == '+p' goto plbroot0
  if '%1' == '-p' goto plbroot1

  if '%1' == '+s' goto sysroot0
  if '%1' == '-s' goto sysroot1

  if '%1' == '+v' goto verbose0
  if '%1' == '-v' goto verbose1

  goto pool01

:datroot0
  set szDataRootDrv=
  set szDataRootDir=
  goto next01
:datroot1
  if '%3' == '' goto missarg
  set szDataRootDrv=%2
  set szDataRootDir=%3
  shift
  shift
  goto next01

:plbroot0
  set szPlobRoot=
  goto next01
:plbroot1
  if '%2' == '' goto missarg
  set szPlobRoot=%2
  shift
  goto next01

:sysroot0
  set SystemRoot=
  goto next01
:sysroot1
  if '%2' == '' goto missarg
  set SystemRoot=%2
  shift
  goto next01

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
if not '%1' == '' goto xtraarg

rem ------------------------------------------------------------
rem Argument checking
rem ------------------------------------------------------------
if '%szDataRootDrv%' == '' set szDataRootDrv=%szDefaultDataRootDrv%
if '%szDataRootDir%' == '' set szDataRootDir=%szDefaultDataRootDir%

if '%SystemRoot%' == '' set SystemRoot=%szDefaultSystemRoot%
if exist %SystemRoot% goto fi02
  set SystemRoot=
  for %%d in (%szDefaultSystemRoots%) do if '%SystemRoot%' == '' if exist %%d set SystemRoot=%%d
:fi02
if '%SystemRoot%' == '' goto missys
if not exist %SystemRoot% goto missys
if not exist %SystemRoot%\system32 goto missys32

if '%szPlobRoot%' == '' set szPlobRoot=%szDefaultplobRoot%
if '%szPlobRoot%' == '' goto misplb
if not exist %szPlobRoot% goto misplb
if not exist %szPlobRoot%\src\allegro goto misplb

rem ------------------------------------------------------------
rem Mainline processing
rem ------------------------------------------------------------

echo.
echo About to configure PLOB to following settings:
echo.
echo   System root directory:          %SystemRoot%
echo   PLOB! installation directory:   %szPlobRoot%
echo   PLOB! database root directory:  %szDataRootDrv%:%szDataRootDir%
echo.
echo Press ^^C to cancel now or
pause
%szVerbose%.

rem Extend PATH

rem %szVerbose% path %szPlobRoot%\bin;%szPlobRoot%\%szOncRpc%\bin;%%PATH%%
path %szPlobRoot%\bin;%szPlobRoot%\%szOncRpc%\bin;%PATH%

rem Create directories

if not exist %szPlobRoot%\lib\%szSystemVers% call %szSelf% -goto domkdir %szPlobRoot%\lib\%szSystemVers%

if not exist %szPlobRoot%\src\allegro\allegro5 call %szSelf% -goto domkdir %szPlobRoot%\src\allegro\allegro5

if not exist %szPlobRoot%\src\util\allegro5 call %szSelf% -goto domkdir %szPlobRoot%\src\util\allegro5

if not exist %szDataRootDrv%:%szDataRootDir% call %szSelf% -goto domkdir %szDataRootDrv%:%szDataRootDir%

rem Copy ONC RPC files

if exist %SystemRoot%\system32\oncrpc.dll goto then05
if exist %SystemRoot%\system32\portmap.exe goto then05
goto else05
:then05
  echo %szScript%: Info: It looks as if there is already an installed version of
  echo %szSpaces%        ONC RPC for Windows/NT on this machine. If you would like
  echo %szSpaces%        to replace your current ONC RPC version with the one
  echo %szSpaces%        delivered with PLOB, do the following:
  echo %szSpaces%         - Terminate any application which might use the ONC RPC DLL
  echo %szSpaces%         - Start the control panel, select "Services"
  echo %szSpaces%         - Stop the "portmap" service
  echo %szSpaces%         - In %SystemRoot%\system32, delete oncrpc.dll and portmap.exe
  echo %szSpaces%         - Restart this script
  echo %szSpaces%        Normally, a replacement of ONC RPC is *not* necessary.
  echo.
  echo Press ^^C to cancel now or
  pause
  echo.
  set bOncRpcInstalled=t
  goto fi05
:else05
  call %szSelf% -goto docopy %szPlobRoot%\%szOncRpc%\bin\oncrpc.dll %SystemRoot%\system32\oncrpc.dll
  call %szSelf% -goto docopy %szPlobRoot%\%szOncRpc%\bin\portmap.exe %SystemRoot%\system32\portmap.exe
  call %szSelf% -goto docopy %szPlobRoot%\%szOncRpc%\etc\rpc %SystemRoot%\system32\drivers\etc\rpc
:fi05

rem Install the portmap service

%szVerbose% %szScript%: Call: inst_pm %SystemRoot%\system32\portmap.exe
inst_pm %SystemRoot%\system32\portmap.exe > nul
rem On an error return, assume an already running portmap daemon:
if errorlevel 1 set bOncRpcInstalled=t

rem Start the portmap service

echo.
echo %szScript%: Info: Now, the portmap service is started. This service is needed
echo %szSpaces%        for building up the client/server communication of PLOB:
echo %szSpaces%         - In the control panel, open "Services"
echo %szSpaces%         - In the "Service" list, select the "portmap" service.
echo %szSpaces%         - Press the "Startup..." button.
echo %szSpaces%         - Select a "Startup Type" of "Automatic".
echo %szSpaces%         - Press the "OK" button.
echo %szSpaces%         - Press the "Close" button.
echo %szSpaces%         - Close the control panel.
echo %szSpaces%         - Continue with this script.

if '%bOncRpcInstalled%' == '' goto fi06
  echo %szSpaces%        NB: It looks as if the portmap service is already running
  echo %szSpaces%        on your system. If this is the case, close the control
  echo %szSpaces%        panel and continue with this script.
:fi06
echo %szSpaces%        Starting control panel now, may take some seconds ...
start control
echo.
echo Press ^^C to cancel now or
pause
%szVerbose%.

rem Copy PLOB files

call %szSelf% -goto docopy %szPlobRoot%\conf\%szSystemVers%\libpostore.lib %szPlobRoot%\lib\%szSystemVers%\libpostore.lib

call %szSelf% -goto docopy %szPlobRoot%\conf\%szSystemVers%\*.dll %szPlobRoot%\lib\%szSystemVers%

call %szSelf% -goto docopy %szPlobRoot%\conf\%szSystemVers%\plobdadmin.exe %szDataRootDrv%:%szDataRootDir%\plobdadmin.exe

call %szSelf% -goto docopy %szPlobRoot%\conf\%szSystemVers%\librpclientplob.dll %szDataRootDrv%:%szDataRootDir%\librpclientplob.dll

set bStartTaskMgr=1

:loop02

  set bPlobRunning=
  echo.
  call %szSelf% -goto docopy %szPlobRoot%\conf\%szSystemVers%\plobd.exe %szDataRootDrv%:%szDataRootDir%\plobd.exe

  if '%bPlobRunning%' == '' goto pool02

  echo.

  if '%bStartTaskMgr%' == '' goto else09
    echo %szScript%: Info: Copying the PLOB daemon failed, maybe it is running.
    echo %szSpaces%        Please kill the current active plobd.exe process.
    echo %szSpaces%        Starting task manager now, may take some seconds ...
    set bStartTaskMgr=
    start taskmgr
    goto fi09
  :else09
    echo %szScript%: Warn: Copying the PLOB daemon failed; this might result
    echo %szSpaces%        in unexpected errors when starting PLOB.
    echo %szSpaces%        To try a new installation, restart this script.
    goto pool02
  :fi09
  echo.
  echo Press ^^C to cancel now or
  pause
  echo.
  goto loop02

:pool02

%szVerbose%.

if not '%bPlobRunning%' == '' goto fi10
  rem Start PLOB (this may fail if portmap is not yet started)
  %szVerbose% %szScript%: Call: %szDataRootDrv%:%szDataRootDir%\plobd.exe
  cmd /C %szSelf% -goto dostart
:fi10

rem Some final messages

echo %szScript%: Info: To finish the Plob installation, please edit file
echo %szSpaces%        src\allegro\defsystem-plob.lisp and change
echo %szSpaces%        the definition of constant +plob-dir+ to
echo %szSpaces%        point to the actual Plob installation directory at
if '%szPlobRoot%' == '.' goto else08
  echo %szSpaces%        %szPlobRoot%
  goto fi08
:else08
  cd
:fi08

goto exit

rem ------------------------------------------------------------
rem Subroutines
rem ------------------------------------------------------------

:domkdir
  shift
  shift
  %szVerbose% %szScript%: Call: mkdir %1
  mkdir %1 > nul
  if exist %1 goto fi03
    echo %szScript%: Warning: Could not mkdir %1
  :fi03
  goto return

:docopy
  shift
  shift
  %szVerbose% %szScript%: Call: copy %1 %2
  copy /B %1 %2 > nul
  if errorlevel 1 goto then04
  if not exist %2 goto then04
  goto fi04
  :then04
    set bPlobRunning=t
    echo %szScript%: Warning: Could not copy %1 to
    echo %szSpaces%           %2
  :fi04
  goto return

:dostart
  shift
  shift
  %szDataRootDrv%:
  cd %szDataRootDir%
  .\plobd.exe
  goto return

:help
  echo Usage:
  echo   %szScript% [-h] [-v] [-d drive directory] [-p directory] [-s directory]
  echo Purpose:
  echo   Install PLOB! on a Windows/NT machine.
  echo Options:
  echo   -h  Show this help text.
  echo   -v  Switch to verbose mode.
  echo   +v  Switch to silent mode.
  echo   -d  Use passed drive and directory as PLOB database root directory.
  echo       Must be passed as 2 arguments.
  echo       Default: %szDefaultDataRootDrv% %szDefaultDataRootDir% meaning that %szDefaultDataRootDrv%:%szDefaultDataRootDir% 
  echo       will be used as PLOB database root directory.
  echo   -p  Use passed argument as PLOB installation directory.
  echo       Default: %szDefaultPlobRoot%
  echo   -s  Use passed argument as system root directory.
  echo       Default: %szDefaultSystemRoot%
  echo See Also:
  echo   %szWWW% is the official
  echo   distribution site for PLOB.
  echo Copyright:
  echo   (C) 1994-1998 Heiko Kirschke kirschke@informatik.uni-hamburg.de
  goto exit

:xtraarg
  echo %szScript%: Error: Additional arguments have been passed.
  echo %szSpaces%         Call %szScript% -h for a help text.
  goto exit

:missarg
  echo %szScript%: Error: Missing argument(s) for %1 option.
  echo %szSpaces%         Call %szScript% -h for a help text.
  goto exit

:missys
  echo %szScript%: Error: Could not locate system root directory.
  echo %szSpaces%         Call %szScript% with option -s;
  echo %szSpaces%         see %szScript% -h for a help text.
  goto exit

:missys32
  echo %szScript%: Error: Could not locate %SystemRoot%\system32 directory.
  echo %szSpaces%         Call %szScript% with option -s;
  echo %szSpaces%         see %szScript% -h for a help text.
  goto exit

:misplb
  echo %szScript%: Error: Could not locate PLOB installation directory.
  echo %szSpaces%         Call %szScript% with option -p;
  echo %szSpaces%         see %szScript% -h for a help text.
  goto exit

:exit
  endlocal
:return

rem Local variables:
rem buffer-file-coding-system: iso-latin-1-unix
rem End:
