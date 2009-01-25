# These will most likely need to be adjusted for your install.

# R12B-2: erl_interface-3.5.6, erts-5.6.2
# R12B-4: erl_interface-3.5.8, erts-5.6.4

OTPROOT=/usr/local/lib/erlang
EIROOT=$(OTPROOT)/lib/erl_interface-3.5.8
ERTSROOT=$(OTPROOT)/erts-5.6.4

INCLUDES = -I$(OTPROOT)/usr/include/
INCLUDES += -I$(EIROOT)/include

LIBS = -L$(EIROOT)/lib -lerl_interface -lei

# OS X flags.
GCCFLAGS = -DNDEBUG -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common -Wall

# Linux Flags
#GCCFLAGS = -O3 -fPIC -shared -fno-common -Wall

CFLAGS = $(GCCFLAGS) $(INCLUDES)
LDFLAGS = $(GCCFLAGS) $(LIBS)

OBJECTS = \
	eep0018.o \
	json_parse.o \
    log.o \
	yajl.o \
	yajl_encode.o \
	yajl_lex.o \
	yajl_buf.o \
	yajl_gen.o \
	yajl_parser.o

DRIVER = eep0018_drv.so
BEAM = eep0018.beam

# -- rules --------------------------------------------------------------------

all: $(DRIVER) $(BEAM)

clean: 
	rm -f *.o *.beam $(DRIVER)

$(DRIVER): $(OBJECTS)
	gcc -o $@ $^ $(LDFLAGS)

$(BEAM): eep0018.erl
	erlc $^

check:
	@true
