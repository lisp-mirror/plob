# Nmake macros for building Windows 32-Bit apps
!include <ntwin32.mak>

# hkirschk 2005-03-20
cc="$(MSVCPATH)/Bin/cl.exe"
link="$(MSVCPATH)/Bin/link.exe"

# If the rpc include directory is not included in the standard path
# you have to give the path to it here.
RPCINCLUDEPATH = ..

# If the rpc library is not included in the standard lib path
# you have to give the path to it here.
RPCLIBPATH = ..\bin\\
OBJS=rpc_main.obj rpc_hout.obj rpc_cout.obj rpc_pars.obj rpc_scan.obj rpc_util.obj \
	rpc_svco.obj rpc_clnt.obj

GOAL=rpcgen.exe

.c.obj:
	$(cc) /I$(RPCINCLUDEPATH) $(cdebug) $(cflags) $(cvarsdll) $*.c

$(GOAL): $(OBJS) $(RPCLIBPATH)\oncrpc.lib
	$(link) $(ldebug) $(conflags) $(OBJS) -out:$(GOAL) $(conlibsdll) $(RPCLIBPATH)\oncrpc.lib
	copy $(GOAL) ..\bin

clean:
	-del $(GOAL) $(OBJS)

rpc_main.obj: rpc_main.c
rpc_main.obj: .\rpc_util.h
rpc_main.obj: .\rpc_pars.h
rpc_main.obj: .\rpc_scan.h
rpc_hout.obj: rpc_hout.c
rpc_hout.obj: .\rpc_util.h
rpc_hout.obj: .\rpc_pars.h
rpc_cout.obj: rpc_cout.c
rpc_cout.obj: .\rpc_util.h
rpc_cout.obj: .\rpc_pars.h
rpc_pars.obj: rpc_pars.c
rpc_pars.obj: .\rpc_util.h
rpc_pars.obj: .\rpc_scan.h
rpc_pars.obj: .\rpc_pars.h
rpc_scan.obj: rpc_scan.c
rpc_scan.obj: .\rpc_scan.h
rpc_scan.obj: .\rpc_util.h
rpc_util.obj: rpc_util.c
rpc_util.obj: .\rpc_scan.h
rpc_util.obj: .\rpc_pars.h
rpc_util.obj: .\rpc_util.h
rpc_svco.obj: rpc_svco.c
rpc_svco.obj: .\rpc_pars.h
rpc_svco.obj: .\rpc_util.h
rpc_clnt.obj: rpc_clnt.c
rpc_clnt.obj: .\rpc_pars.h
rpc_clnt.obj: .\rpc_util.h

#### Local variables:
#### buffer-file-coding-system: raw-text-unix
#### End:
