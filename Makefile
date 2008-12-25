
#
# These will most likely need to be adjusted for your install.
#
OTPROOT=/usr/local/lib/erlang
EIROOT=$(OTPROOT)/lib/erl_interface-3.5.8
ERTSROOT=$(OTPROOT)/erts-5.6.4

#
# Should be good below here for OS X at least.
#
INCLUDES = -I$(OTPROOT)/usr/include/
INCLUDES += -I$(EIROOT)/include

LIBS = -L$(EIROOT)/lib -lerl_interface -lei

GCCFLAGS = -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common -Wall
CFLAGS = $(GCCFLAGS) $(INCLUDES)
LDFLAGS = $(GCCFLAGS) $(LIBS)

OBJECTS = \
	src/eep0018.o \
    src/ei_bin_buf.o \
	src/json_to_term.o \
	src/term_to_json.o \
	src/yajl.o \
	src/yajl_encode.o \
	src/yajl_lex.o \
	src/yajl_buf.o \
	src/yajl_gen.o \
	src/yajl_parser.o

DRIVER=eep0018_drv.so
BEAM=eep0018.beam
TESTING=runner.beam mochijson2.beam mochinum.beam

# -- rules --------------------------------------------------------------------

all: $(DRIVER) $(BEAM) $(TESTING)

clean: 
	rm -rf src/*.o *.beam $(DRIVER)

$(DRIVER): $(OBJECTS)
	gcc -o $@ $^ $(LDFLAGS)

$(BEAM): src/eep0018.erl
	erlc $^

$(TESTING): erl/runner.erl erl/mochijson2.erl erl/mochinum.erl
	erlc erl/runner.erl
	erlc erl/mochijson2.erl
	erlc erl/mochinum.erl

tests: $(DRIVER) $(BEAM) $(TESTING)
	erl -noshell -s runner main

