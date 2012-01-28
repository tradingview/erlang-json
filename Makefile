# This file is part of eep0018 released under the MIT license. 
# See the LICENSE file for more information.

%.beam: %.erl
	erlc -o test/ $<

all:
	@mkdir -p ebin
	./rebar compile

check: test/etap.beam test/util.beam json
	prove test/*.t
json:
	ln -s . json

clean:
	./rebar clean
	rm -f test/*.beam
