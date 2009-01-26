# Point me to the erlang installation directory.

OTPROOT=/usr/local/lib/erlang

# Hopefully the rest works for everyone.

EIROOT=$(OTPROOT)/lib/$(shell ls $(OTPROOT)/lib/ | grep erl_interface)
ERTSROOT=$(OTPROOT)/$(shell ls $(OTPROOT) | grep erts)

INCLUDES = -I$(OTPROOT)/usr/include/
INCLUDES += -I$(EIROOT)/include

LIBS = -L$(EIROOT)/lib -lerl_interface -lei

include Makefile.$(shell uname -s)

CFLAGS = $(GCCFLAGS) $(INCLUDES)
LDFLAGS = $(GCCFLAGS) $(LIBS)

OBJECTS = \
	eep0018.o \
	ei_bin_buf.o \
	json_to_term.o \
	term_buf.o \
	term_to_json.o \
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
