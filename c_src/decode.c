// This file is part of eep0018 released under the MIT license. 
// See the LICENSE file for more information.

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "yajl/yajl_parse.h"
#include "yajl/yajl_parser.h"
#include "yajl/yajl_lex.h"

#define MAX_DEPTH       2048
#define OK              1
#define ERROR           0

#define WHERE \
    (fprintf(stderr, "(%s)%d:%s\r\n", __FILE__, __LINE__, __FUNCTION__))

#define THE_NON_VALUE   ((ERL_NIF_TERM)0)
#define is_non_value(v) ((v)==THE_NON_VALUE)
#define is_value(v)     ((v)!=THE_NON_VALUE)

#ifdef HUGE_VAL
/* HUGE_VALL and HUGE_VALF are defined in C99:<math.h> */
#  ifndef HUGE_VALL
#    define HUGE_VALL HUGE_VAL
#  endif
#  ifndef HUGE_VALF
#    define HUGE_VALF (-HUGE_VAL)
#  endif
#endif

typedef enum _value_ignore {
    ign_none,
    ign_next,
    ign_all
} value_ignore;

// LIFO stack instead of lists:reverse/1
#define OBJ_SLAB_SIZE   512
typedef struct _obj
{
    ERL_NIF_TERM    key;
    ERL_NIF_TERM    slab[OBJ_SLAB_SIZE];
    short           used;
    short           ignore;
    struct _obj*    next;
} Object; // Map or Array

typedef enum _key_decode {
    /* bitwise-or of binary=1, existent_atom=2, ignore=4. */
    key_binary        = 1,
    key_existent_atom = 2,
    key_existent_atom_or_binary = 3,
    key_existent_atom_or_ignore = 6
} key_decode;

// Depth stack to handle nested objects
typedef struct
{
    ErlNifEnv*      env;
    ERL_NIF_TERM    error;
    ERL_NIF_TERM    val;
    Object*         stack[MAX_DEPTH];
    int             depth;
    yajl_handle     handle;
    key_decode      key_decode;
    int             badvals_len;
    ERL_NIF_TERM*   badvals;
    ERL_NIF_TERM    pre_decoded;
} Decoder;

void
init_decoder(Decoder* dec, ErlNifEnv* env, ERL_NIF_TERM pre_decoded)
{
    dec->env   = env;
    dec->error = THE_NON_VALUE;
    dec->val   = THE_NON_VALUE;
    dec->depth = -1;
    dec->handle = NULL;
    dec->key_decode  = key_binary;
    dec->badvals_len = 0;
    dec->badvals     = NULL;
    dec->pre_decoded = pre_decoded;
    memset(dec->stack, '\0', sizeof(ERL_NIF_TERM) * MAX_DEPTH);
}

void
destroy_decoder(Decoder* dec, ErlNifEnv* env)
{
    Object* obj = NULL;
    while(dec->depth >= 0)
    {
        while(dec->stack[dec->depth] != NULL)
        {
            obj = dec->stack[dec->depth];
            dec->stack[dec->depth] = obj->next;
            enif_free_compat(dec->env, obj);
        }
        dec->depth--;
    }
}

const char* LEX_ERRORS[] =
{
    "ok",
    "invalid_utf8",
    "invalid_escaped_char",
    "invalid_json_char",
    "invalid_hex_char",
    "invalid_char",
    "invalid_string",
    "missing_integer_after_decimal",
    "missing_integer_after_exponent",
    "missing_integer_after_minus",
    "unallowed_comment"
};

const char* PARSE_ERRORS[] =
{
    "ok",
    "client_cancelled",
    "integer_overflow",
    "numeric_overflow",
    "invalid_token",
    "internal_invalid_token",
    "key_must_be_string",
    "pair_missing_colon",
    "bad_token_after_map_value",
    "bad_token_after_array_value"
};

ERL_NIF_TERM
make_error(yajl_handle handle, ErlNifEnv* env)
{
    ERL_NIF_TERM atom;
    
    yajl_parser_error pe = handle->parserError;
    yajl_lex_error le = yajl_lex_get_error(handle->lexer);

    if(le != yajl_lex_e_ok)
    {
        atom = enif_make_atom(env, LEX_ERRORS[le]);
    }
    else if(pe != yajl_parser_e_ok)
    {
        atom = enif_make_atom(env, PARSE_ERRORS[pe]);
    }
    else
    {
        atom = enif_make_atom(env, "unknown");
    }

    return enif_make_tuple(env, 2,
        enif_make_atom(env, "error"),
        enif_make_tuple(env, 2,
            enif_make_uint(env, yajl_get_bytes_consumed(handle)),
            atom
        )
    );
}

static inline int
push_value(Decoder* dec, ERL_NIF_TERM val)
{
    Object* obj = NULL;
    Object* new = NULL;

    // Single value parsed
    if(dec->depth < 0)
    {
        if( is_value(dec->val) ) return ERROR;
        dec->val = val;
        return OK;
    }
    
    assert(dec->stack[dec->depth] != NULL);
    obj = dec->stack[dec->depth];

    switch( obj->ignore )
    {
    case ign_none:
        break;
    case ign_next:
        obj->key    = THE_NON_VALUE;
        obj->ignore = ign_none;
        return OK;
    case ign_all:
        obj->key = THE_NON_VALUE;
        return OK;
    }

    if( is_value(obj->key) )
    {
        val = enif_make_tuple(dec->env, 2, obj->key, val);
        obj->key = THE_NON_VALUE;
    }

    // Room left in object slab
    if(obj->used < OBJ_SLAB_SIZE)
    {
        obj->slab[obj->used++] = val;
        return OK;
    }
    
    // New object slab required
    new = (Object*) enif_alloc_compat(dec->env, sizeof(Object));
    if(new == NULL)
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return ERROR;
    }
    memset(new, '\0', sizeof(Object));
    new->key = THE_NON_VALUE;
    new->slab[0] = val;
    new->used = 1;
    new->next = obj;
    dec->stack[dec->depth] = new;
    
    return OK;
}

static inline int
pop_object(Decoder* dec, ERL_NIF_TERM* val)
{
    Object* curr = NULL;
    ERL_NIF_TERM ret = enif_make_list(dec->env, 0);

    if(dec->depth < 0)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_depth");
        return ERROR;
    }
    if(dec->stack[dec->depth]->used > OBJ_SLAB_SIZE)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_slab_use");
        return ERROR;
    }
    
    while(dec->stack[dec->depth] != NULL)
    {
        curr = dec->stack[dec->depth];
        while(curr->used > 0)
        {
            ret = enif_make_list_cell(dec->env, curr->slab[--curr->used], ret);
        }
        dec->stack[dec->depth] = curr->next;
        enif_free_compat(dec->env, curr);
    }

    dec->depth--;
    *val = ret;
    return OK;
}

static int
decode_null(void* ctx)
{
    Decoder* dec = (Decoder*) ctx;
    return push_value(dec, enif_make_atom(dec->env, "null"));
}

static int
decode_boolean(void* ctx, int val)
{
    Decoder* dec = (Decoder*) ctx;
    if(val)
    {
        return push_value(dec, enif_make_atom(dec->env, "true"));
    }
    else
    {
        return push_value(dec, enif_make_atom(dec->env, "false"));
    }

    return OK;
}

static int
decode_integer(void* ctx, long val)
{
    Decoder* dec = (Decoder*) ctx;
    return push_value(dec, enif_make_long(dec->env, val));
}

static int
decode_double(void* ctx, double val)
{
    Decoder* dec = (Decoder*) ctx;
    return push_value(dec, enif_make_double(dec->env, val));
}

static int
decode_bigval(Decoder* dec, const char* buf, long len)
{
    ERL_NIF_TERM cell = dec->pre_decoded;
    ErlNifBinary bin;

    /* find term from predecoded values. */
    while( !enif_is_empty_list(dec->env, cell) )
    {
        ERL_NIF_TERM head;
        ERL_NIF_TERM const* tuple;
        int arity;
        if( !enif_get_list_cell(dec->env, cell, &head, &cell) )
        {
            dec->error = enif_make_atom(dec->env, "badarg");
            return ERROR;
        }
        if( !enif_get_tuple(dec->env, head, &arity, &tuple) )
        {
            dec->error = enif_make_atom(dec->env, "badarg");
            return ERROR;
        }
        if( arity <  2 )
        {
            dec->error = enif_make_atom(dec->env, "badarg");
            return ERROR;
        }
        if( !enif_is_binary(dec->env, tuple[0]) )
        {
            dec->error = enif_make_atom(dec->env, "badarg");
            return ERROR;
        }
        if( !enif_inspect_binary(dec->env, tuple[0], &bin) )
        {
            dec->error = enif_make_atom(dec->env, "badarg");
            return ERROR;
        }
        if( bin.size != len || memcmp(bin.data, buf, len) != 0 )
        {
            /* not match. */
            continue;
        }
        /* match. */
        return push_value(dec, tuple[1]);
    }

    /* if not found, append to unknown values result. */
    {
        ErlNifBinary bin;
        ERL_NIF_TERM atom_term;
        ERL_NIF_TERM bin_term;
        ERL_NIF_TERM pos_term;
        ERL_NIF_TERM term;

        if( !enif_alloc_binary(len, &bin) )
        {
            dec->error = enif_make_atom(dec->env, "alloc_binary");
            return ERROR;
        }
        memcpy(bin.data, buf, len);
        bin_term = enif_make_binary(dec->env, &bin);

        pos_term = enif_make_uint(dec->env, yajl_get_bytes_consumed(dec->handle));

        atom_term = enif_make_atom(dec->env, "bigval");
        term = enif_make_tuple(dec->env, 3, atom_term, bin_term, pos_term);
        dec->badvals = enif_realloc_compat(dec->env, dec->badvals, sizeof(ERL_NIF_TERM)*(dec->badvals_len+1));
        dec->badvals[dec->badvals_len] = term;
        ++ dec->badvals_len;
    }
    return OK;
}

static int
decode_number(void* ctx, const char* buf, unsigned int len)
{
    Decoder* dec = (Decoder*) ctx;
    char bufz[22];
    char* endp;
    long int lval;
    double dval;

    /* copy as numm-terminated text. */
    if( len >  sizeof(bufz)-1 )
    {
        return decode_bigval(dec, buf, len);
    }
    memcpy(bufz, buf, len);
    bufz[len] = '\0';

    /* at first, try decoding as integer. */
    lval = strtol(bufz, &endp, 10);
    if( *endp == '\0' )
    {
        if( (lval == LONG_MIN || lval == LONG_MAX) && errno == ERANGE )
        {
            return decode_bigval(dec, buf, len);
        }
        return decode_integer(dec, lval);
    }
    while(*endp && isdigit(*endp) )
    {
        ++ endp;
    }
    if( *endp == '\0' )
    {
        return decode_bigval(dec, buf, len);
    }

    /* next, try decoding as double. */
    dval = strtod(bufz, &endp);
    if( *endp == '\0' )
    {
        if( (dval == HUGE_VALF || dval == HUGE_VALL) && errno == ERANGE )
        {
            return decode_bigval(dec, buf, len);
        }
        return decode_double(dec, dval);
    }

    /* both failed, append unknown values result. */
    return decode_bigval(dec, buf, len);
}

static int
decode_string(void* ctx, const unsigned char* data, unsigned int size)
{
    ErlNifBinary bin;
    Decoder* dec = (Decoder*) ctx;
    if(!enif_alloc_binary_compat(dec->env, size, &bin))
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return ERROR;
    }
    memcpy(bin.data, data, size);
    return push_value(dec, enif_make_binary(dec->env, &bin));
}

static int
decode_start_obj(void* ctx)
{
    Object* obj = NULL;
    Decoder* dec = (Decoder*) ctx;
    
    if(dec->depth+1 < 0)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_depth");
        return ERROR;           
    }
    if(dec->depth+1 >= MAX_DEPTH)
    {
        dec->error = enif_make_atom(dec->env, "max_depth_exceeded");
        return ERROR;   
    }
    dec->depth++;
    
    obj = (Object*) enif_alloc_compat(dec->env, sizeof(Object));
    if(obj == NULL)
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return ERROR;
    }
    memset(obj, '\0', sizeof(Object));
    obj->key = THE_NON_VALUE;
    obj->used = 0;
    obj->next = NULL;
    dec->stack[dec->depth] = obj;
    
    return OK;
}

static int
decoder_make_binary(Decoder* dec, const unsigned char* data, unsigned int size, ERL_NIF_TERM* p_term)
{
    ErlNifBinary bin;
    if(!enif_alloc_binary_compat(dec->env, size, &bin))
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return 0; /* false. */
    }
    memcpy(bin.data, data, size);
    *p_term = enif_make_binary(dec->env, &bin);
    return 1; /* true. */
}

static int
is_7bit_data(const unsigned char* data, unsigned int len)
{
    unsigned int i;
    for( i=0; i<len; ++i )
    {
        if( data[i] & ~127 )
        {
            return 0; /* false. */
        }
    }
    return 1; /* true. */
}

static int
decode_map_key(void* ctx, const unsigned char* data, unsigned int size)
{
    Decoder* dec = (Decoder*) ctx;
    if( dec->depth < 0 )
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_map_key_depth");
        return ERROR;
    }
    if( is_value(dec->stack[dec->depth]->key) )
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_no_key_set");
        return ERROR;
    }

    switch( dec->key_decode )
    {
    case key_binary:
        break;
    case key_existent_atom:
    case key_existent_atom_or_binary:
    case key_existent_atom_or_ignore:
        {
            int acceptable = is_7bit_data(data, size);
            if( acceptable )
            {
                ERL_NIF_TERM atom_key;
                if( enif_make_existing_atom_len(dec->env, (const char*)data, size, &atom_key, ERL_NIF_LATIN1) )
                {
                    dec->stack[dec->depth]->key = atom_key;
                    return OK;
                }
            }
            /* not exist/not acceptable. */
            if( dec->key_decode == key_existent_atom )
            {
                ERL_NIF_TERM reason;
                ERL_NIF_TERM bin_term;
                if( !decoder_make_binary(dec, data, size, &bin_term) )
                {
                    return ERROR;
                }
                if( acceptable )
                {
                    reason = enif_make_atom(dec->env, "atom_not_exist");
                }else
                {
                    reason = enif_make_atom(dec->env, "not_acceptable_as_atom");
                }
                dec->error = enif_make_tuple(dec->env, 2, reason, bin_term);
                return ERROR;
            }
            if( dec->key_decode == key_existent_atom_or_ignore )
            {
                dec->stack[dec->depth]->key    = THE_NON_VALUE;
                dec->stack[dec->depth]->ignore = ign_next;
                return OK;
            }
            break;
        }
    }

    {
        ERL_NIF_TERM binary_key;
        if( !decoder_make_binary(dec, data, size, &binary_key) )
        {
            return ERROR;
        }
        dec->stack[dec->depth]->key = binary_key;
    }
    return OK;
}

static int
decode_end_map(void* ctx)
{
    ERL_NIF_TERM val;
    Decoder* dec = (Decoder*) ctx;
    if(!pop_object(dec, &val)) return ERROR;
    val = enif_make_tuple(dec->env, 1, val);
    return push_value(dec, val);
}

static int
decode_end_array(void* ctx)
{
    ERL_NIF_TERM val;
    Decoder* dec = (Decoder*) ctx;
    if(!pop_object(dec, &val)) return ERROR;
    return push_value(dec, val);
}

static yajl_callbacks
decoder_callbacks = {
    decode_null,
    decode_boolean,
    NULL, /* yajl_integer */
    NULL, /* yajl_double  */
    decode_number,
    decode_string,
    decode_start_obj, /* yajl_start_map. */
    decode_map_key,
    decode_end_map,
    decode_start_obj, /* yajl_start_array. */
    decode_end_array
};

static int
check_rest(unsigned char* data, unsigned int size, unsigned int used)
{
    unsigned int i = 0;
    for(i = used; i < size; i++)
    {
        switch(data[i])
        {
            case ' ':
            case '\t':
            case '\r':
            case '\n':
                continue;
            default:
                return ERROR;
        }
    }
    
    return OK;
}

static int
parse_decode_opts(ErlNifEnv* env, Decoder* dec, yajl_parser_config* conf, ERL_NIF_TERM opts)
{
    ERL_NIF_TERM head, tail;
    ERL_NIF_TERM key, value;
    int ret;
    ERL_NIF_TERM am_allow_comments;
    ERL_NIF_TERM am_true;
    ERL_NIF_TERM am_false;
    ERL_NIF_TERM am_key_decode;
    ERL_NIF_TERM am_existent_atom;
    ERL_NIF_TERM am_binary;
    ERL_NIF_TERM am_ignore;
    int arity;
    const ERL_NIF_TERM* array;

    if( enif_is_empty_list(env, opts) )
    {
        return 0; /* success. */
    }

    am_allow_comments = enif_make_atom(env, "allow_comments");
    am_true           = enif_make_atom(env, "true");
    am_false          = enif_make_atom(env, "false");
    am_key_decode     = enif_make_atom(env, "key_decode");
    am_existent_atom  = enif_make_atom(env, "existent_atom");
    am_binary         = enif_make_atom(env, "binary");
    am_ignore         = enif_make_atom(env, "ignore");
    ret = 0;
    do
    {
        if( !enif_get_list_cell(env, opts, &head, &tail) )
        {
            ret = 1; /* failure. */
            break;
        }
        opts = tail;

        /* single Atom in options is treated as {Atom, true}. */
        if( enif_is_atom(env, head) )
        {
            key   = head;
            value = am_true;
        }else if( enif_get_tuple(env, head, &arity, &array) )
        {
            /* {Atom, Value} pair. */
            if( arity < 2 )
            {
                ret = 1; /* failure. */
                break;
            }
            key   = array[0];
            value = array[1];
        }else
        {
            /* malformed option. */
            ret = 1; /* failure. */
            break;
        }

        if( key == am_allow_comments )
        {
            if( value == am_true )
            {
                conf->allowComments = 1;
            }else if( value == am_false )
            {
                conf->allowComments = 0;
            }else
            {
                ret = 1; /* failure. */
                break;
            }
        }else if( key == am_key_decode )
        {
            ERL_NIF_TERM term_1, term_2;
            if( value == am_binary )
            {
                dec->key_decode = key_binary;
            }else if( value == am_existent_atom )
            {
                dec->key_decode = key_existent_atom;
            }else if( enif_get_list_cell(dec->env, value, &term_1, &value) )
            {
                if( !enif_get_list_cell(dec->env, value, &term_2, &value) )
                {
                    term_2 = THE_NON_VALUE;
                }
                if( !enif_is_empty_list(dec->env, value) )
                {
                    ret = 1; /* failure. */
                    break;
                }
                /* valid values are: [binary], [existent_atom], 
                 * [existent_atom, binary], [existent_atom, ignore]. */
                if( term_1 == am_binary )
                {
                    if( is_value(term_2) )
                    {
                        ret = 1; /* failure. */
                        break;
                    }
                    dec->key_decode = key_binary;
                }else if( term_1 == am_existent_atom )
                {
                    if( term_2 == am_binary )
                    {
                        dec->key_decode = key_existent_atom_or_binary;
                    }else if( term_2 == am_ignore )
                    {
                        dec->key_decode = key_existent_atom_or_ignore;
                    }else if( is_non_value(term_2) )
                    {
                        dec->key_decode = key_existent_atom;
                    }else
                    {
                        ret = 1; /* failure. */
                        break;
                    }
                }else
                {
                    ret = 1; /* failure. */
                    break;
                }
            }else
            {
                /* malformed key_decode option. */
                ret = 1; /* failure. */
                break;
            }
        }else
        {
            /* unknown option. */
            ret = 1; /* failure. */
            break;
        }
    } while( !enif_is_empty_list(env, opts) );
 
    return ret;
}

ERL_NIF_TERM
decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Decoder dec;
    yajl_parser_config conf;
    yajl_status status;
    unsigned int used;
    ErlNifBinary bin;
    ERL_NIF_TERM ret;
    
    init_decoder(&dec, env, argv[2]);

    if( argc != 3 )
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    if( !enif_is_list(env, argv[1]) )
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    if( !enif_is_list(env, argv[2]) )
    {
        ret = enif_make_badarg(env);
        goto done;
    }

    memset(&conf, 0, sizeof(conf));
    conf.allowComments = 0; // No comments.
    conf.checkUTF8     = 1; // check utf8.

    if( parse_decode_opts(env, &dec, &conf, argv[1]) != 0 )
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    dec.handle = yajl_alloc(&decoder_callbacks, &conf, NULL, &dec);

    if(dec.handle == NULL)
    {
        ret = enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "memory_error")
        );
        goto done;
    }

    if(!enif_inspect_iolist_as_binary(env, argv[0], &bin))
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    
    status = yajl_parse(dec.handle, bin.data, bin.size);
    used = yajl_get_bytes_consumed(dec.handle);
    destroy_decoder(&dec, env);

    // Parsing something like "2.0" (without quotes) will
    // cause a spurious semi-error. We add the extra size
    // check so that "2008-20-10" doesn't pass.
    if(status == yajl_status_insufficient_data && used == bin.size)
    {
        status = yajl_parse_complete(dec.handle);
    }

    if(status == yajl_status_ok && used != bin.size)
    {
        if(check_rest(bin.data, bin.size, used) != OK)
        {
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "garbage_after_value")
            );
            goto done;
        }
    }

    switch(status)
    {
        case yajl_status_ok:
            if( dec.badvals_len != 0 )
            {
                int i;
                ret = enif_make_list(env, 0);
                for( i=dec.badvals_len-1; i>=0; --i )
                {
                    ret = enif_make_list_cell(env, dec.badvals[i], ret);
                }
                ret = enif_make_tuple(env, 2, enif_make_atom(env, "badvals"), ret);
                goto done;
            }else
            {
                ret = enif_make_tuple(env, 2, enif_make_atom(env, "ok"), dec.val);
                goto done;
            }

        case yajl_status_error:
            ret = make_error(dec.handle, env);
            goto done;

        case yajl_status_insufficient_data:
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "insufficient_data")
            );
            goto done;

        case yajl_status_client_canceled:
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_tuple(env, 2,
                    enif_make_uint(env, yajl_get_bytes_consumed(dec.handle)),
                    dec.error
                )
            );
            goto done;

        default:
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "unknown")
            );
            goto done;
    }

done:
    if(dec.handle != NULL) yajl_free(dec.handle);
    if( dec.badvals != NULL )
    {
        enif_free_compat(env, dec.badvals);
    }
    return ret;
}
