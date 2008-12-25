
#include "eep0018.h"

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

static eep0018_data*
prepare(void* ctx, int mode)
{
    eep0018_data* st = (eep0018_data*) ctx;
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
finish(eep0018_data* st, int mode)
{
    if(st->state[st->depth] != mode) return 0;
    st->state[st->depth] = ST_NORMAL;
    st->depth--;
    return 1;
}

static int
erl_json_null(void* ctx)
{
    eep0018_data* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_atom_len(&st->buf, "null", 4);
    return 1;
}

static int
erl_json_boolean(void* ctx, int boolVal)
{
    eep0018_data* st = prepare(ctx, ST_NORMAL);
    
    if(boolVal)
        ei_x_encode_atom_len(&st->buf, "true", 4);
    else
        ei_x_encode_atom_len(&st->buf, "false", 5);

    return 1;
}

static int
erl_json_long(void* ctx, long val)
{
    eep0018_data* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_long(&st->buf, val);
    return 1;
}

static int
erl_json_double(void* ctx, double val)
{
    eep0018_data* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_double(&st->buf, val);
    return 1;
}

static int
erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen)
{
    eep0018_data* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_binary(&st->buf, stringVal, stringLen);
    return 1;
}
 
static int
erl_json_start_map(void* ctx)
{
    // {"foo": 1} -> {[{<<"foo">>, 1}]}
    eep0018_data* st = prepare(ctx, ST_MAP);
    ei_x_encode_tuple_header(&st->buf, 1);
    return 1;
}

static int
erl_json_end_map(void* ctx)
{
    //State* st = prepare(ctx, ST_MAP);
    eep0018_data* st = prepare(ctx, ST_NORMAL);
    ei_x_encode_empty_list(&st->buf);
    return finish(st, ST_MAP);
}

static int
erl_json_map_key(void* ctx, const unsigned char* keyVal, unsigned int keyLen)
{
    eep0018_data* st = prepare(ctx, ST_NORMAL);
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
    eep0018_data* st = (eep0018_data*) ctx;
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

#define ALLOW_COMMENTS 0
#define CHECK_UTF8 0

void
json_to_term(void* ctx)
{
    /* initialize state */
    eep0018_data* st = (eep0018_data*) ctx;
    unsigned char* msg;
    int32_t length;
    ei_x_new_with_version(&st->buf);
    st->depth = 0;
    st->state[st->depth] = ST_NORMAL;

    /* get a parser handle */
    yajl_parser_config conf = { ALLOW_COMMENTS, CHECK_UTF8 };
    st->phandle = yajl_alloc(&erl_json_callbacks, &conf, st);

    //fprintf(stderr, "VER: %d\r\n", (unsigned char) state.buf.buff[0]);
    
    /* start parser */
    //fprintf(stderr, "JSON: ");
    //fwrite(st->bin->orig_bytes, st->bin->orig_size, 1, stderr);
    //fprintf(stderr, "\r\n");
    yajl_status stat = yajl_parse(st->phandle, (unsigned char*) st->bin->orig_bytes, st->bin->orig_size);

    /* if result is not ok: we might raise an error?? */
    if (stat != yajl_status_ok)
    {
        fprintf(stderr, "ERROR: %d\r\n", stat);
        msg = yajl_get_error(st->phandle, 1, (unsigned char*) st->bin->orig_bytes, st->bin->orig_size);
        fprintf(stderr, "ERROR: %s\r\n", msg);
        yajl_free_error(msg);
        st->result = ERROR;
        return;
    }

    //fprintf(stderr, "DONE\r\n");
   
    //fprintf(stderr, "<<%d", (unsigned char) state.buf.buff[0]);
    //for(length = 1; length < state.buf.index; length++)
    //    fprintf(stderr, ", %d", (unsigned char) state.buf.buff[length]);
    //fprintf(stderr, ">>\r\n");
    st->result = OK;

    return;
}
