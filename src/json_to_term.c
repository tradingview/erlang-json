#include "erl_interface.h"
#include "ei.h"    

#include <stdlib.h>

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

#include <erl_driver.h>
#include <ei.h>

#include "putget.h"

#ifndef WIN32
#include <string.h>
#endif

/* 
 * See ei documentation at 
 *
 * http://erlang.org/doc/man/ei.html 
 */

#define MAX_DEPTH   1024
#define ST_NORMAL   1
#define ST_ARRAY    2
#define ST_MAP      3

typedef struct
{
    ei_x_buff   buf;
    int         state[MAX_DEPTH];
    int         depth;
} State;

static State*
prepare(void* ctx, int mode)
{
    State* st = (State*) ctx;
    if(st->state[st->depth] == ST_ARRAY)
    {
        ei_x_encode_list_header(&st->buf, 1);
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
    ei_x_encode_atom_len(&st->buf, "null", 4);
    return 1;
}

static int
erl_json_boolean(void* ctx, int boolVal)
{
    State* st = prepare(ctx, ST_NORMAL);
    
    if(boolVal)
        ei_x_encode_atom_len(&st->buf, "true", 4);
    else
        ei_x_encode_atom_len(&st->buf, "false", 5);

    return 1;
}

static int
erl_json_long(void* ctx, long val)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_long(&st->buf, val);
    return 1;
}

static int
erl_json_double(void* ctx, double val)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_double(&st->buf, val);
    return 1;
}

static int
erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_binary(&st->buf, stringVal, stringLen);
    return 1;
}
 
static int
erl_json_start_map(void* ctx)
{
    // {"foo": 1} -> {[{<<"foo">>, 1}]}
    State* st = prepare(ctx, ST_MAP);
    ei_x_encode_tuple_header(&st->buf, 1);
    return 1;
}

static int
erl_json_end_map(void* ctx)
{
    //State* st = prepare(ctx, ST_MAP);
    State* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_empty_list(&st->buf);
    return finish(st, ST_MAP);
}

static int
erl_json_map_key(void* ctx, const unsigned char* keyVal, unsigned int keyLen)
{
    State* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_list_header(&st->buf, 1);
    ei_x_encode_tuple_header(&st->buf, 2);
    ei_x_encode_binary(&st->buf, keyVal, keyLen);
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
    ei_x_encode_empty_list(&st->buf);
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

#define ALLOW_COMMENTS 1
#define CHECK_UTF8 1

int
json_to_term(char* inBuf, char** retBuf, int retBufLen)
{
    /* initialize state */
    unsigned char* msg;
    int32_t length;
    int32_t ret;
    State state;
    ei_x_new_with_version(&state.buf);
    state.depth = 0;
    state.state[state.depth] = ST_NORMAL;
    
    /* get erlang binary length */
    memcpy(&length, inBuf, sizeof(length));
    inBuf += sizeof(length);

    /* get a parser handle */
    yajl_parser_config conf = { ALLOW_COMMENTS, CHECK_UTF8 };
    yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, &state);

    //fprintf(stderr, "VER: %d\r\n", (unsigned char) state.buf.buff[0]);
    
    /* start parser */
    yajl_status stat = yajl_parse(handle, (unsigned char*) inBuf, length);

    /* if result is not ok: we might raise an error?? */
    if (stat != yajl_status_ok)
    {
        fprintf(stderr, "ERROR: %d\r\n", stat);
        msg = yajl_get_error(handle, 1, (unsigned char*) inBuf, length);
        fprintf(stderr, "ERROR: %s\r\n", msg);
        yajl_free_error(msg);
        ei_x_free(&state.buf);
        return -1;
    }

    //fprintf(stderr, "DONE\r\n");
   
    if(*retBuf == NULL || state.buf.index > retBufLen)
    {
        (*retBuf) = (char*) driver_alloc(state.buf.index);
        if(*retBuf == NULL)
        {
            return -1;
        }
    }
    
    memcpy(*retBuf, state.buf.buff, state.buf.index);
    //fprintf(stderr, "<<%d", (unsigned char) state.buf.buff[0]);
    //for(length = 1; length < state.buf.index; length++)
    //    fprintf(stderr, ", %d", (unsigned char) state.buf.buff[length]);
    //fprintf(stderr, ">>\r\n");
    ret = state.buf.index;
    ei_x_free(&state.buf);

    return ret;
}
