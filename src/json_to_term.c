
#include "eep0018.h"
#include "ei_bin_buf.h"


#ifndef WIN32
#include <string.h>
#endif

/* 
 * See ei documentation at 
 *
 * http://erlang.org/doc/man/ei.html 
 */

#define ST_NORMAL   1
#define ST_ARRAY    2
#define ST_MAP      3

typedef struct
{
    ei_bin_buf* buf;
    int         state[MAX_DEPTH];
    int         depth;
} State;

static State*
prepare(void* ctx, int mode)
{
    State* st = (State*) ctx;
    if(st->state[st->depth] == ST_ARRAY)
    {
        ei_bin_buf_list(st->buf, 1);
    }

    if(mode != ST_NORMAL)
    {
        st->depth++;
        st->state[st->depth] = mode;
    }
    return st;
}

static int
finish(State* st, int mode)
{
    if(st->state[st->depth] != mode) return 0;
    st->state[st->depth] = ST_NORMAL;
    st->depth--;
    return 1;
}

static int
erl_json_null(void* ctx)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_bin_buf_atom(st->buf, "null", 4);
    return 1;
}

static int
erl_json_boolean(void* ctx, int boolVal)
{
    State* st = prepare(ctx, ST_NORMAL);
    
    if(boolVal)
        ei_bin_buf_atom(st->buf, "true", 4);
    else
        ei_bin_buf_atom(st->buf, "false", 5);

    return 1;
}

static int
erl_json_long(void* ctx, long val)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_bin_buf_long(st->buf, val);
    return 1;
}

static int
erl_json_double(void* ctx, double val)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_bin_buf_double(st->buf, val);
    return 1;
}

static int
erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_bin_buf_binary(st->buf, stringVal, stringLen);
    return 1;
}
 
static int
erl_json_start_map(void* ctx)
{
    // {"foo": 1} -> {[{<<"foo">>, 1}]}
    State* st = prepare(ctx, ST_MAP);
    ei_bin_buf_tuple(st->buf, 1);
    return 1;
}

static int
erl_json_end_map(void* ctx)
{
    //State* st = prepare(ctx, ST_MAP);
    State* st = prepare(ctx, ST_NORMAL);
    ei_bin_buf_empty_list(st->buf);
    return finish(st, ST_MAP);
}

static int
erl_json_map_key(void* ctx, const unsigned char* keyVal, unsigned int keyLen)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_bin_buf_list(st->buf, 1);
    ei_bin_buf_tuple(st->buf, 2);
    ei_bin_buf_binary(st->buf, keyVal, keyLen);
    return 1;
}

static int
erl_json_start_array(void* ctx)
{
    //State* st = prepare(ctx, ST_ARRAY);
    prepare(ctx, ST_ARRAY);
    return 1;
}

static int
erl_json_end_array(void* ctx)
{
    State* st = (State*) ctx;
    ei_bin_buf_empty_list(st->buf);
    return finish(st, ST_ARRAY);
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

int
json_to_term(char* buf, int len, char** rbuf, int rlen)
{
    unsigned char* msg;
    State st;
    st.buf = ei_bin_buf_from_buffer(rbuf, rlen);
    st.depth = 0;
    st.state[st.depth] = ST_NORMAL;

    // Skip erlang's header
    buf += 6;
    len -= 6;

    ei_bin_buf_version(st.buf);
    
    yajl_parser_config conf = {ALLOW_COMMENTS, CHECK_UTF8};
    yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, &st);
    yajl_status stat = yajl_parse(handle, (unsigned char*) buf, len);

    if(stat != yajl_status_ok)
    {
        //msg = yajl_get_error(handle, 1, buf, len);
        msg = yajl_get_error(handle, 0, NULL, 0);
        fprintf(stderr, "ERROR: %s\r\n", msg);
        yajl_free_error(msg);
        return -1;
    }

    *rbuf = st.buf->data;
    rlen = st.buf->used;
    yajl_free(handle);
    ei_bin_buf_close(st.buf, 0);
    return rlen;
}
