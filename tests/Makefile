# Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
# Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
#
# This file is part of EEP0018, which is released under the MIT
# license.

BEAM = \
	compare.beam \
	mochijson2.beam \
	mochinum.beam \
	rabbitmq.beam \
	runner.beam

.SUFFIXES: .erl .beam

.erl.beam:
	erlc $^

all: $(BEAM)

clean: 
	rm -f *.so *.beam

copy:
	cp ../src/*.so ./
	cp ../src/*.beam ./

check: copy $(BEAM)
	erl -noshell -s runner main

