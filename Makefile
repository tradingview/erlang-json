#
# The erlang source dir
OTPROOT=/usr/local/lib/erlang


#
# The yajl dir
YAJLROOT=yajl

#
# The erlang interface dir
EIROOT=$(OTPROOT)/lib/erl_interface-3.5.8
ERTSROOT=$(OTPROOT)//erts-5.6.4


#
# -----------------------------------------------------------------------------
ERLANG_INCLUDES=-I$(OTPROOT)/usr/include/
# ERLANG_LIBS=-L$(OTPROOT)/usr/lib/ -lerts

EI_INCLUDES=-I $(EIROOT)/include -I $(EIROOT)/src/misc
EI_LIBS=-L$(EIROOT)/lib -lerl_interface -lei

# ./usr/include/
# -----------------------------------------------------------------------------

GCCFLAGS=-fPIC -bundle -flat_namespace -undefined suppress -fno-common -Wall

CFLAGS=$(GCCFLAGS) -I include $(ERLANG_INCLUDES) $(EI_INCLUDES)
LFLAGS=$(GCCFLAGS) $(ERLANG_LIBS) $(EI_LIBS)

ERLCFLAGS=

# -- objects -------------------------------------------------------------------

VPATH=src:$(YAJLROOT)/src

YAJL_OBJECTS=yajl.o yajl_encode.o yajl_lex.o yajl_buf.o yajl_gen.o yajl_parser.o
EEP_OBJECTS=json_to_term.o term_to_json.o eep0018.o
# EEP_OBJECTS=eep0018.o

PATHS=include include/yajl

DRIVER=eep0018_drv.so
BEAM=eep0018.beam

# -- rules --------------------------------------------------------------------

all: include include/yajl driver beam

clean: 
	rm -rf include *.o $(DRIVER) $(BEAM)

include: 
	mkdir include

include/yajl: 
	ln -sf ../yajl/src/api include/yajl

beam: $(BEAM)

eep0018.beam: eep0018.erl
	$(OTPROOT)/bin/erlc $^ $(ERLCFLAGS)

driver: $(DRIVER)

eep0018_drv.so: $(YAJL_OBJECTS) $(EEP_OBJECTS)
	gcc -o $@ $^ $(LFLAGS)
