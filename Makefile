# This file is part of eep0018 released under the MIT license. 
# See the LICENSE file for more information.

top_builddir := `pwd`

%.beam: %.erl
	erlc -o test/ $<

all:
	@mkdir -p ebin
	./rebar compile

check: test/etap.beam test/util.beam json
	ERL_FLAGS="-pa ./json/ebin" JSON_NIF_DIR=$(top_builddir)/priv \
	  ./rebar eunit
	ERL_FLAGS="-pa ./json/ebin" JSON_NIF_DIR=$(top_builddir)/priv \
	  prove test/*.t
json:
	ln -s . json

clean:
	./rebar clean
	rm -f test/*.beam
