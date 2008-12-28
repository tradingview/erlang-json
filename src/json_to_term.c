
#include "eep0018.h"
#include "term_buf.h"

#ifndef WIN32
#include <string.h>
#endif

#define YAJL_OK 1
#define YAJL_ERROR 0

typedef struct
{
    // Comm stuffs
    ErlDrvPort      port;
    ErlDrvTermData  pid;
    
    // SAX States
    ErlDrvTermData  value;
    ErlDrvTermData  map_open;
    ErlDrvTermData  map_close;
    ErlDrvTermData  map_key;
    ErlDrvTermData  list_open;
    ErlDrvTermData  list_close;
    ErlDrvTermData  finished;
    ErlDrvTermData  error;
    
    // Stored atoms
    ErlDrvTermData  atom_null;
    ErlDrvTermData  atom_true;
    ErlDrvTermData  atom_false;
} State;

static void
init_state(ErlDrvPort port, State* st)
{
    st->port = port;
    st->pid = driver_caller(port);
     
    st->value = driver_mk_atom("value");
    st->map_open = driver_mk_atom("map_open");
    st->map_close = driver_mk_atom("map_close");
    st->map_key = driver_mk_atom("map_key");
    st->list_open = driver_mk_atom("list_open");
    st->list_close = driver_mk_atom("list_close");
    st->finished = driver_mk_atom("finished");
    st->error = driver_mk_atom("error");

    st->atom_null = driver_mk_atom("null");
    st->atom_true = driver_mk_atom("true");
    st->atom_false = driver_mk_atom("false");
}

inline int
output_term(State* st, ErlDrvTermData* spec, int len)
{
    int resp = driver_output_term(st->port, spec, len);
    //int resp = driver_send_term(st->port, st->pid, spec, len);
    return resp == 1 ? YAJL_OK : YAJL_ERROR;
}

int
output_error(State* st, char* mesg)
{
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->error,
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) mesg, strlen((char*) mesg),
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term(st->port, spec, 7);
}

static int
erl_json_null(void* ctx)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->value,
        ERL_DRV_ATOM, st->atom_null,
        ERL_DRV_TUPLE, 2
    };
    return output_term(st, spec, 6);
}

static int
erl_json_boolean(void* ctx, int boolVal)
{
    State* st = (State*) ctx;
    ErlDrvTermData atom = boolVal ? st->atom_true : st->atom_false ;
    
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->value,
        ERL_DRV_ATOM, atom,
        ERL_DRV_TUPLE, 2
    };
    return output_term(st, spec, 6);
}

static int
erl_json_long(void* ctx, long val)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->value,
        ERL_DRV_INT,  (ErlDrvSInt) val,
        ERL_DRV_TUPLE, 2
    };
    return output_term(st, spec, 6);
}

static int
erl_json_double(void* ctx, double val)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->value,
        ERL_DRV_FLOAT, (ErlDrvTermData) &val,
        ERL_DRV_TUPLE, 2
    };
    return output_term(st, spec, 6);
}

static int
erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->value,
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) stringVal, (ErlDrvUInt) stringLen,
        ERL_DRV_TUPLE, 2
    };
    return output_term(st, spec, 7);
}
 
static int
erl_json_start_map(void* ctx)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->map_open
    };
    return output_term(st, spec, 2);
}

static int
erl_json_end_map(void* ctx)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->map_close
    };
    return output_term(st, spec, 2);
}

static int
erl_json_map_key(void* ctx, const unsigned char* keyVal, unsigned int keyLen)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->map_key,
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) keyVal, (ErlDrvUInt) keyLen,
        ERL_DRV_TUPLE, 2
    };
    return output_term(st, spec, 7);
}

static int
erl_json_start_array(void* ctx)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->list_open,
    };
    return output_term(st, spec, 2);
}

static int
erl_json_end_array(void* ctx)
{
    State* st = (State*) ctx;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, st->list_close,
    };
    return output_term(st, spec, 2);
}

static yajl_callbacks erl_json_callbacks = {
    erl_json_null,
    erl_json_boolean,
    erl_json_long,
    erl_json_double,
    NULL,
    erl_json_string,
    erl_json_start_map,
    erl_json_map_key,
    erl_json_end_map,
    erl_json_start_array,
    erl_json_end_array
};

#define ALLOW_COMMENTS 0
#define CHECK_UTF8 0

void
json_to_term(ErlDrvPort port, char* buf, int len)
{
    State st;
    unsigned char* msg;
    int resp;
   
    init_state(port, &st);
    
    yajl_parser_config conf = {ALLOW_COMMENTS, CHECK_UTF8};
    yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, &st);
    yajl_status stat = yajl_parse(handle, (unsigned char*) buf, len);

    if(stat != yajl_status_ok)
    {
        msg = yajl_get_error(handle, 0, NULL, 0);
        resp = output_error(&st, (char*) msg);
        yajl_free_error(msg);
        yajl_free(handle);
    }
    else
    {
        yajl_free(handle);
    }
}

