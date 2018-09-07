// This file is part of eep0018 released under the MIT license. 
// See the LICENSE file for more information.

#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "yajl/yajl_gen.h"

#define OK 1
#define ERROR 0

#define WHERE \
    (fprintf(stderr, "(%s)%d:%s\r\n", __FILE__, __LINE__, __FUNCTION__))

typedef struct
{
    ErlNifEnv* env;
    yajl_gen handle;
    ERL_NIF_TERM error;
    int           badvals_len;
    ERL_NIF_TERM* badvals;
    ERL_NIF_TERM  pre_encoded;
} Encoder;

int enc_json(Encoder* enc, ERL_NIF_TERM term);

#define MIN(a, b) ((a) < (b) ? (a) : (b))

int
enc_atom(Encoder* enc, ERL_NIF_TERM atom)
{
    int ret = ERROR;
    char buf[512];
    
    if(!enif_get_atom_compat(enc->env, atom, buf, 512))
    {
        enc->error = enif_make_atom(enc->env, "internal_atom_error");
        return ERROR;
    }
    
    if(strncmp(buf, "null", MIN(strlen(buf), 4)) == 0)
    {
        if(yajl_gen_null(enc->handle) != yajl_gen_status_ok) goto done;
    }
    else if(strncmp(buf, "true", MIN(strlen(buf), 4)) == 0)
    {
        if(yajl_gen_bool(enc->handle, 1) != yajl_gen_status_ok) goto done;
    }
    else if(strncmp(buf, "false", MIN(strlen(buf), 5)) == 0)
    {
        if(yajl_gen_bool(enc->handle, 0) != yajl_gen_status_ok) goto done;
    }
    else
    {
        if(yajl_gen_string(enc->handle, (unsigned char*) buf, strlen(buf))
                != yajl_gen_status_ok)
        {
            goto done;
        }
    }
    
    ret = OK;

done:
    return ret;
}

int
enc_binary(Encoder* enc, ERL_NIF_TERM term)
{
    ErlNifBinary bin;
    
    if(!enif_inspect_binary(enc->env, term, &bin))
    {
        enc->error = enif_make_atom(enc->env, "unable_to_inspect_binary");
        return ERROR;
    }
    
    if(yajl_gen_string(enc->handle, bin.data, bin.size) != yajl_gen_status_ok)
    {
        enc->error = enif_make_atom(enc->env, "failed_writing_binary");
        return ERROR;
    }
    
    return OK;
}

int
enc_array(Encoder* enc, ERL_NIF_TERM head, ERL_NIF_TERM tail)
{
    if(yajl_gen_array_open(enc->handle) != yajl_gen_status_ok)
    {
        enc->error = enif_make_atom(enc->env, "failed_to_open_array");
        return ERROR;
    }
    
    do {
        if(enc_json(enc, head) != OK) return ERROR;
    } while(enif_get_list_cell(enc->env, tail, &head, &tail));
    
    if(yajl_gen_array_close(enc->handle) != yajl_gen_status_ok)
    {
        enc->error = enif_make_atom(enc->env, "failed_to_close_array");
        return ERROR;
    }
    
    return OK;
}

int
enc_key(Encoder* enc, ERL_NIF_TERM key)
{
    char buf[512];
    ErlNifBinary bin;
    
    if(enif_is_atom(enc->env, key))
    {
        if(!enif_get_atom_compat(enc->env, key, buf, 512))
        {
            enc->error = enif_make_atom(enc->env, "failed_getting_atom_key");
            return ERROR;
        }
        if(yajl_gen_string(enc->handle, (unsigned char*) buf, strlen(buf))
                != yajl_gen_status_ok)
        {
            enc->error = enif_make_atom(enc->env, "failed_writing_atom_key");
            return ERROR;
        }
        return OK;
    }
    else if(enif_inspect_iolist_as_binary(enc->env, key, &bin))
    {
        if(yajl_gen_string(enc->handle, bin.data, bin.size) != yajl_gen_status_ok)
        {
            enc->error = enif_make_atom(enc->env, "failed_writing_binary_key");
            return ERROR;
        }
        return OK;
    }
    
    enc->error = enif_make_tuple(enc->env, 2,
        enif_make_atom(enc->env, "badkey"),
        key
    );
    return ERROR;
}

int
enc_map(Encoder* enc, ERL_NIF_TERM head, ERL_NIF_TERM tail)
{
    int arity;
    const ERL_NIF_TERM* tuple;

    if(yajl_gen_map_open(enc->handle) != yajl_gen_status_ok)
    {
        enc->error = enif_make_atom(enc->env, "failed_to_open_map");
        return ERROR;
    }

    do {
        if(!enif_get_tuple(enc->env, head, &arity, &tuple))
        {
            enc->error = enif_make_tuple(enc->env, 2,
                enif_make_atom(enc->env, "badarg"),
                head
            );
            return ERROR;
        }
        if(arity != 2)
        {
            enc->error = enif_make_tuple(enc->env, 2,
                enif_make_atom(enc->env, "badarity"),
                head
            );
            return ERROR;
        }
        if(enc_key(enc, tuple[0]) != OK) return ERROR;
        if(enc_json(enc, tuple[1]) != OK) return ERROR;
    } while(enif_get_list_cell(enc->env, tail, &head, &tail));
    
    if(yajl_gen_map_close(enc->handle) != yajl_gen_status_ok)
    {
        enc->error = enif_make_atom(enc->env, "failed_to_close_map");
        return ERROR;
    }
    
    return OK;
}

int
enc_erl_map(Encoder* enc, ERL_NIF_TERM map)
{
    ERL_NIF_TERM key, value;
    ErlNifMapIterator iter;

    if(yajl_gen_map_open(enc->handle) != yajl_gen_status_ok)
    {
        enc->error = enif_make_atom(enc->env, "failed_to_open_map");
        return ERROR;
    }

    enif_map_iterator_create(enc->env, map, &iter, ERL_NIF_MAP_ITERATOR_FIRST);

    while (enif_map_iterator_get_pair(enc->env, &iter, &key, &value)) {
        if(enc_key(enc, key) != OK)
        {
            enif_map_iterator_destroy(enc->env, &iter);
            return ERROR;
        }
        if(enc_json(enc, value) != OK) 
        {
            enif_map_iterator_destroy(enc->env, &iter);
            return ERROR;
        }
        enif_map_iterator_next(enc->env, &iter);
    }

    enif_map_iterator_destroy(enc->env, &iter);

    if(yajl_gen_map_close(enc->handle) != yajl_gen_status_ok)
    {
        enc->error = enif_make_atom(enc->env, "failed_to_close_map");
        return ERROR;
    }   
    return OK;
}

int
enc_json(Encoder* enc, ERL_NIF_TERM term)
{
    ErlNifUInt64 ui64val;
    ErlNifSInt64 i64val;
    double dval;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    int arity;
    const ERL_NIF_TERM* tuple;
    
    if(enif_is_atom(enc->env, term))
    {
        return enc_atom(enc, term);
    }
    
    if(enif_is_binary(enc->env, term))
    {
        return enc_binary(enc, term);
    }
    
	if(enif_get_int64(enc->env, term, &i64val))
    {
        if(yajl_gen_integer(enc->handle, i64val) != yajl_gen_status_ok)
        {
            enc->error = enif_make_atom(enc->env, "bad_integer");
            return ERROR;
        }
        return OK;
    }

    if(enif_get_uint64(enc->env, term, &ui64val))
    {
        if(yajl_gen_integer(enc->handle, ui64val) != yajl_gen_status_ok)
        {
            enc->error = enif_make_atom(enc->env, "bad_unsigned_integer");
            return ERROR;
        }
        return OK;
    }

    if(enif_get_double(enc->env, term, &dval))
    {
        if(yajl_gen_double(enc->handle, dval) != yajl_gen_status_ok)
        {
            enc->error = enif_make_atom(enc->env, "bad_double");
            return ERROR;
        }
        return OK;
    }

    if(enif_is_empty_list(enc->env, term))
    {
        if(yajl_gen_array_open(enc->handle) != yajl_gen_status_ok)
        {
            enc->error = enif_make_atom(enc->env, "failed_to_open_empty_list");
            return ERROR;
        }
        if(yajl_gen_array_close(enc->handle) != yajl_gen_status_ok)
        {
            enc->error = enif_make_atom(enc->env, "failed_to_close_empty_list");
            return ERROR;
        }
        return OK;
    }

    if(enif_get_list_cell(enc->env, term, &head, &tail))
    {
        return enc_array(enc, head, tail);
    }
    
    if(enif_get_tuple(enc->env, term, &arity, &tuple))
    {
        if(arity == 1)
        {
            if(enif_is_empty_list(enc->env, tuple[0]))
            {
                if(yajl_gen_map_open(enc->handle) != yajl_gen_status_ok)
                {
                    enc->error = enif_make_atom(enc->env,
                                                "failed_to_open_empty_map");
                    return ERROR;
                }
                if(yajl_gen_map_close(enc->handle) != yajl_gen_status_ok)
                {
                    enc->error = enif_make_atom(enc->env,
                                                "failed_to_close_empty_map");
                    return ERROR;
                }
                return OK;
            }
            else if(enif_get_list_cell(enc->env, tuple[0], &head, &tail))
            {
                return enc_map(enc, head, tail);
            }
        }
    }

    if (enif_is_map(enc->env, term)) 
    {
        return enc_erl_map(enc, term);
    }

    /* find term from preencoded values. */
    {
        ERL_NIF_TERM cell = enc->pre_encoded;
        ErlNifBinary bin;
        while( !enif_is_empty_list(enc->env, cell) )
        {
            ERL_NIF_TERM head;
            ERL_NIF_TERM const* tuple;
            int arity;
            if( !enif_get_list_cell(enc->env, cell, &head, &cell) )
            {
                enc->error = enif_make_atom(enc->env, "badarg");
                return ERROR;
            }
            if( !enif_get_tuple(enc->env, head, &arity, &tuple) )
            {
                enc->error = enif_make_atom(enc->env, "badarg");
                return ERROR;
            }
            if( arity <  2 )
            {
                enc->error = enif_make_atom(enc->env, "badarg");
                return ERROR;
            }
            if( tuple[0] != term )
            {
                continue;
            }
            if( !enif_is_binary(enc->env, tuple[1]) )
            {
                enc->error = enif_make_atom(enc->env, "badarg");
                return ERROR;
            }
            if( !enif_inspect_binary(enc->env, tuple[1], &bin) )
            {
                enc->error = enif_make_atom(enc->env, "badarg");
                return ERROR;
            }
            if( yajl_gen_number(enc->handle, (const char*)bin.data, bin.size) != yajl_gen_status_ok )
            {
                enc->error = enif_make_atom(enc->env, "failed_writing_number");
                return ERROR;
            }
            return OK;
        }
    }

    /* if not found, append to unknown values result. */
    enc->badvals = enif_realloc_compat(enc->env, enc->badvals, sizeof(ERL_NIF_TERM)*(enc->badvals_len+1));
    enc->badvals[enc->badvals_len] = term;
    ++ enc->badvals_len;
    return OK;
}

ERL_NIF_TERM
encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Encoder enc;
    yajl_gen_config config = {0, NULL};
    yajl_gen handle = yajl_gen_alloc(&config, NULL);
    yajl_gen_status status;
    ERL_NIF_TERM ret;
    ErlNifBinary bin;
    const unsigned char* json;
    unsigned int jsonlen;

    memset(&enc, 0, sizeof(enc));
    
    if( argc != 2 )
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    if( !enif_is_list(env, argv[1]) )
    {
        ret = enif_make_badarg(env);
        goto done;
    }

    if(handle == NULL)
    {
        ret = enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "memory_error")
        );
        goto done;
    }

    enc.env = env;
    enc.handle = handle;
    enc.error = 0;
    enc.badvals_len = 0;
    enc.badvals     = NULL;
    enc.pre_encoded = argv[1];
    if(enc_json(&enc, argv[0]) == ERROR)
    {
        if(enc.error == 0)
        {
            ret = enif_make_atom(env, "unknown");
        }
        else
        {
            ret = enc.error;
        }
        ret = enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            ret
        );
        goto done;
    }
    if( enc.badvals_len != 0 )
    {
        int i;
        ret = enif_make_list(env, 0);
        for( i=enc.badvals_len-1; i>=0; --i )
        {
            ret = enif_make_list_cell(env, enc.badvals[i], ret);
        }
        ret = enif_make_tuple(env, 2, enif_make_atom(env, "badvals"), ret);
        goto done;
    }

    status = yajl_gen_get_buf(handle, &json, &jsonlen);
    if(status != yajl_gen_status_ok)
    {
        ret = enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "unknown")
        );
        goto done;
    }
    
    if(!enif_alloc_binary_compat(env, jsonlen, &bin))
    {
        ret = enif_make_atom(env, "memory_error");
        goto done;
    }
    
    memcpy(bin.data, json, jsonlen);
    
    ret = enif_make_tuple(env, 2,
        enif_make_atom(env, "ok"),
        enif_make_binary(env, &bin)
    );

done:
    if(handle != NULL) yajl_gen_free(handle);
    if( enc.badvals != NULL )
    {
        enif_free_compat(env, enc.badvals);
    }
    return ret;
}
