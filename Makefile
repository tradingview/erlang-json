
%.beam: %.erl
	erlc -o test/ $<

all:
	@mkdir -p ebin
	./rebar compile

check: test/etap.beam test/util.beam
	prove test/*.t

clean:
	./rebar clean
	rm test/*.beam
