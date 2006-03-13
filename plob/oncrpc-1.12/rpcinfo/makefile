# Nmake macros for building Windows 32-Bit apps
!include <ntwin32.mak>

# If the rpc include directory is not included in the standard path
# you have to give the path to it here.
RPCINCLUDEPATH = ..

# If the rpc library is not included in the standard lib path
# you have to give the path to it here.
RPCLIBPATH = ..\bin\\

OBJS=rpcinfo.obj
GOAL=rpcinfo.exe

.c.obj:
	$(cc) /I$(RPCINCLUDEPATH) $(cdebug) $(cflags) $(cvarsdll) $*.c

$(GOAL): $(OBJS) $(RPCLIBPATH)\oncrpc.lib
	$(link) $(ldebug) $(conflags) $(OBJS) -out:$(GOAL) $(conlibsdll) $(RPCLIBPATH)\oncrpc.lib wsock32.lib
	copy $(GOAL) ..\bin

clean:
	-del $(GOAL) $(OBJS)

rpcinfo.obj: rpcinfo.c getopt.c

#### Local variables:
#### buffer-file-coding-system: raw-text-unix
#### End:
