include $(ARCH)_Makefile

all: $(ARCH)_tester $(ARCH)_domain_mapper $(KEYS)

$(ARCH)_tester : tester.c ValidateHost.c
	rm -f $(ARCH)_tester
	$(CC) $(CFLAGS) -o $(ARCH)_tester tester.c
	chmod 500 $(ARCH)_tester

$(ARCH)_domain_mapper: mapper.c ValidateHost.c
	rm -f $(ARCH)_domain_mapper
	$(CC) $(CFLAGS) -DNIS -o $(ARCH)_domain_mapper mapper.c
	chmod 500 $(ARCH)_domain_mapper
	rm -f $(ARCH)_host_mapper
	$(CC) $(CFLAGS) -o $(ARCH)_host_mapper mapper.c
	chmod 500 $(ARCH)_host_mapper

clean:
	rm -f *_mapper *_tester NISKEYS PSKEYS

PSKEYS:	$(ARCH)_host_mapper HOSTS
	$(ARCH)_host_mapper < HOSTS > PSKEYS

NISKEYS: $(ARCH)_domain_mapper DOMAINS
	$(ARCH)_domain_mapper < DOMAINS > NISKEYS

# Local variables:
# buffer-file-coding-system: iso-latin-1-unix
# End:
