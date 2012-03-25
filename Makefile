# This file is part of eep0018 released under the MIT license. 
# See the LICENSE file for more information.

top_builddir := `pwd`

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
