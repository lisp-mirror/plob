# --------------------------------------------------------------------
# 1998/05 HK: Top-level make file for PLOB
# --------------------------------------------------------------------
standard-config config:
	bin/plobconfig -config -standard
advanced-config:
	bin/plobconfig -config -advanced

all clean dist-clean initial makefile makefiles server:
	@echo 'Call:'
	@echo '    $(MAKE) -f <operating system>/makefile $@'
	@echo 'to rebuild target $@.'
	@echo 'Supported <operating system>s are:'
	@echo '    irix    - IRIX 6.4 and higher'
	@echo '    linux   - Linux kernel version 2.0.0 and higher'
	@echo '    solaris - Solaris 2.5 and 2.6'
	@echo '    win32   - Windows/NT 4.0 and higher'

# Local variables:
# buffer-file-coding-system: iso-latin-1-unix
# End:
