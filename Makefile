# This file is part of eep0018 released under the MIT license. 
# See the LICENSE file for more information.

top_builddir := `pwd`

PACKAGE_VERNAME=json-0.0.1

ERL=erl
erl_lib_dir := $(shell $(ERL) -noinput -eval 'io:format("~s",[code:lib_dir()]), init:stop()')

INSTALL = install
INSTALL_UIDGID = -o root -g root
#DESTDIR =

.PHONY: doc

%.beam: %.erl
	erlc -o test/ $<

all:
	@mkdir -p ebin
	./rebar compile

check: test/etap.beam test/util.beam
	ERL_FLAGS="-pa ./ebin" JSON_NIF_DIR=$(top_builddir)/priv \
	  ./rebar eunit
	ERL_FLAGS="-pa ./ebin" JSON_NIF_DIR=$(top_builddir)/priv \
	  prove test/*.t

clean:
	./rebar clean
	rm -f test/*.beam

doc:
	./rebar doc

install:
	for d in ebin priv doc/html; do \
		$(INSTALL) $(INSTALL_UIDGID) -d $(DESTDIR)$(erl_lib_dir)/$(PACKAGE_VERNAME)/$$d || exit $$?; \
		for f in $$d/*; do \
			$(INSTALL) $(INSTALL_UIDGID) $$f $(DESTDIR)$(erl_lib_dir)/$(PACKAGE_VERNAME)/$$f || exit $$?; \
		done \
	done
	for f in LICENSE; do \
		$(INSTALL) $(INSTALL_UIDGID) $$f $(DESTDIR)$(erl_lib_dir)/$(PACKAGE_VERNAME)/$$f || exit $$?; \
	done
