cd librpc
nmake -f Makefile.bc %1
if errorlevel 1 goto end
cd ..\rpcgen
nmake -f Makefile.bc %1
if errorlevel 1 goto end
cd ..\service
nmake -f Makefile.bc %1
if errorlevel 1 goto end
cd ..\rpcinfo
nmake -f Makefile.bc %1
if errorlevel 1 goto end
cd ..\test
nmake -f Makefile.bc %1
:end
cd ..


