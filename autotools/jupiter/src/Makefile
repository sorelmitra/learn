# src/Makefile.  Generated from Makefile.in by configure.

# Package-related substitution variables
package = jupiter
version = 1.0
tarname = jupiter
distdir = $(tarname)-$(version)
srcdir = .


# Prefix-related substitution variables
prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin

Makefile: Makefile.in ../config.status
	cd .. && ./config.status $@

../config.status: ../configure
	cd .. && ./config.status --recheck

CC     = gcc
CFLAGS = -g -O2

all: jupiter

jupiter: main.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $(srcdir)/main.c

check: all
	./jupiter | grep "Hello from .*jupiter!"
	@echo "*** ALL TESTS PASSED ***"

clean:
	-rm jupiter

install:
	mkdir -p $(DESTDIR)$(bindir)
	install -d $(DESTDIR)$(bindir)
	install -m 0755 jupiter $(DESTDIR)$(bindir)

uninstall:
	-rm $(DESTDIR)$(bindir)/jupiter

.PHONY: all clean check uninstall
