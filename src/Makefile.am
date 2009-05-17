## Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
## Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
##
## This file is part of EEP0018, which is released under the MIT license.

ACLOCAL_AMFLAGS = -I m4

eep0018ebindir = ./

lib_LTLIBRARIES = eep0018_drv.la
eep0018_drv_la_SOURCES = \
	eep0018.beam \
	eep0018.c \
	encode_json.c \
	decode_json.c \
	term_buf.c \
    yajl.c \
    yajl_alloc.c \
    yajl_buf.c \
    yajl_encode.c \
    yajl_gen.c \
    yajl_lex.c \
    yajl_parser.c

eep0018_drv_la_LDFLAGS = -module -avoid-version

eep0018ebin_DATA = eep0018.beam

%.beam: %.erl
	$(ERLC) $<;
