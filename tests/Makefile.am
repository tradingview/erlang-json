## Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
## Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
##
## This file is part of EEP0018, which is released under the MIT license.

TESTOBJS = \
    compare.beam \
    mochijson2.beam \
    mochinum.beam \
    rabbitmq.beam \
    runner.beam

CLEANFILES = $(TESTOBJS) eep0018.beam eep0018_drv.so

test: $(TESTOBJS)
	cp ../src/.libs/*.so ./
	cp ../src/*.beam ./
	erl -noshell -s runner main

%.beam: %.erl
	$(ERLC) $<;
