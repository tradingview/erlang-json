
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

GCCFLAGS = -fPIC -bundle -flat_namespace -undefined suppress -fno-common -Wall
CFLAGS = $(GCCFLAGS) $(INCLUDES)
LDFLAGS = $(GCCFLAGS) $(LIBS)

OBJECTS = \
	src/eep0018.o \
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

# -- rules --------------------------------------------------------------------

all: $(DRIVER) $(BEAM)

clean: 
	rm -rf *.o $(DRIVER) $(BEAM)

$(BEAM): src/eep0018.erl
	erlc $^

$(DRIVER): $(OBJECTS)
	gcc -o $@ $^ $(LDFLAGS)
