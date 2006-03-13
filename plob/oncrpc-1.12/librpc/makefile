# Nmake macros for building Windows 32-Bit apps

!include <ntwin32.mak>

# If the rpc include directory is not included in the standard path
# you have to give the path to it here.
RPCINCLUDEPATH=..

# If the rpc library is not included in the standard lib path
# you have to give the path to it here.
RPCLIBPATH=..\bin\\

DEFINITION=oncrpc.def

OBJS=clnt_raw.obj \
	xdr.obj \
	clnt_tcp.obj \
	clnt_udp.obj \
	pmap_rmt.obj \
	rpc_prot.obj \
	svc_autu.obj \
	svc_auth.obj \
	svc_raw.obj \
	svc_run.obj \
	svc_tcp.obj \
	svc_udp.obj \
	xdr_mem.obj \
	xdr_rec.obj \
	auth_non.obj \
	auth_uni.obj \
	authunix.obj \
	bindresv.obj \
	clnt_gen.obj \
	clnt_per.obj \
	clnt_sim.obj \
	get_myad.obj \
	getrpcen.obj \
	getrpcpo.obj \
	pmap_cln.obj \
	pmap_get.obj \
	pmap_gma.obj \
	pmap_pro.obj \
	pmap_pr.obj \
	rpc_call.obj \
	rpc_comm.obj \
	svc_simp.obj \
	xdr_arra.obj \
	xdr_floa.obj \
	xdr_refe.obj \
	xdr_stdi.obj \
	svc.obj \
	bcopy.obj \
	nt.obj

.c.obj:
	$(cc) /I$(RPCINCLUDEPATH) $(cdebug) $(cflags) $(cvarsdll) $*.c

all: oncrpc.dll portmap.exe

clean:
	del $(OBJS) oncrpc.lib oncrpc.dll oncrpc.exp portmap.obj portmap.exe ..\rpcgen\oncrpc.dll *.pdb

portmap.exe:    oncrpc.lib portmap.obj
	$(link) $(conlflags) $(ldebug) -out:portmap.exe PORTMAP.obj $(RPCLIBPATH)oncrpc.lib $(conlibsdll) wsock32.lib
	copy portmap.exe ..\bin\pm_ascii.exe

oncrpc.lib:     $(OBJS) oncrpc.def
	$(implib) /out:oncrpc.lib /def:$(DEFINITION) $(OBJS)

oncrpc.dll:     $(OBJS) oncrpc.lib oncrpc.exp
	$(link) /DLL /out:oncrpc.dll -entry:_DllMainCRTStartup$(DLLENTRY) $(ldebug) oncrpc.exp $(OBJS) $(conlibsdll) wsock32.lib advapi32.lib
	copy oncrpc.lib ..\bin
	copy oncrpc.dll ..\bin
	copy oncrpc.dll ..\rpcgen

#### Local variables:
#### buffer-file-coding-system: raw-text-unix
#### End:
