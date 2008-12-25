
#include "eep0018.h"

#ifndef WIN32
#include <string.h>
#endif

#define LONG        97
#define NEG_LONG    98
#define DOUBLE      99
#define ATOM        100
#define TUPLE       104
#define EMPTY_LIST  106
#define STRING      107
#define LIST        108
#define BINARY      109
#define VERSION     131

int value_to_json(char* buf, int* index, yajl_gen handle);

int
binary_to_json(char* buf, int* index, yajl_gen handle)
{
    
    int type, size;
    if(ei_get_type(buf, index, &type, &size)) return ERROR;
    if(type != BINARY) return ERROR;
    *index += 5;
    if(yajl_gen_string(handle, (unsigned char*) buf+*index, size)) return ERROR;
    *index += size;
    return OK;
}

int
key_to_json(char* buf, int* index, yajl_gen handle)
{
    char data[MAXATOMLEN];
    int type, size;
    if(ei_get_type(buf, index, &type, &size)) return ERROR;
    if(type == BINARY)
    {
        return binary_to_json(buf, index, handle);
    }
    else if(type == ATOM)
    {
        if(ei_decode_atom(buf, index, data)) return ERROR;
        if(yajl_gen_string(handle, (unsigned char*) data, size)) return ERROR;
        return OK;
    }
    else if(type == STRING)
    {
        *index += 3;
        if(yajl_gen_string(handle, (unsigned char*) buf+*index, size)) return ERROR;
        *index += size;
        return OK;
    }
    
    return ERROR;
}

int
string_to_json(char* buf, int* index, yajl_gen handle)
{
    int i, type, size;
    ei_get_type(buf, index, &type, &size);
    if(type != STRING) return ERROR;
    *index += 3;
    if(yajl_gen_array_open(handle)) return ERROR;
    for(i = 0; i < size; i++)
        if(yajl_gen_integer(handle, (int) (*(buf+*index+i)))) return ERROR;
    if(yajl_gen_array_close(handle)) return ERROR;
    *index += size;
    return OK;
}

int
atom_to_json(char* buf, int* index, yajl_gen handle)
{
    char data[MAXATOMLEN];
    int type, size;
    if(ei_get_type(buf, index, &type, &size)) return ERROR;
    if(type != ATOM) return ERROR;
    if(ei_decode_atom(buf, index, data)) return ERROR;
    if(strcmp(data, "true") == 0)
    {
        if(yajl_gen_bool(handle, 1)) return ERROR;
    }
    else if(strcmp(data, "false") == 0)
    {
        if(yajl_gen_bool(handle, 0)) return ERROR;
    }
    else if(strcmp(data, "null") == 0)
    {
        if(yajl_gen_null(handle)) return ERROR;
    }
    else
    {
        // Matching the old semantics.
        if(yajl_gen_string(handle, (unsigned char*) data, size)) return ERROR;
    }
    
    return OK;
}

int
map_to_json(char* buf, int* index, yajl_gen handle)
{
    int i, arity, size;

    if(ei_decode_tuple_header(buf, index, &arity)) return ERROR;
    if(arity != 1) return ERROR;
    if(ei_decode_list_header(buf, index, &arity)) return ERROR;
    if(yajl_gen_map_open(handle)) return ERROR;
   
    for(i = 0 ; i < arity ; i++)
    {
        if(ei_decode_tuple_header(buf, index, &size)) return ERROR;
        if(size != 2) return ERROR;
        if(key_to_json(buf, index, handle)) return ERROR;
        if(value_to_json(buf, index, handle)) return ERROR;
    }

    //Pass over empty list tail.
    if(arity > 0)
    {
        if(ei_decode_list_header(buf, index, &arity)) return ERROR;
        if(arity != 0) return ERROR;
    }
    
    if(yajl_gen_map_close(handle)) return ERROR;
    
    return OK;
}

int
array_to_json(char* buf, int* index, yajl_gen handle)
{
    int i, arity;
    if(ei_decode_list_header(buf, index, &arity)) return ERROR;
    if(yajl_gen_array_open(handle)) return ERROR;
    for(i = 0; i < arity; i++)
    {
        if(value_to_json(buf, index, handle)) return ERROR;
    }

    //Pass over empty list tail.
    if(arity > 0)
    {
        if(ei_decode_list_header(buf, index, &arity)) return ERROR;
        if(arity != 0) return ERROR;
    }
    
    if(yajl_gen_array_close(handle))
    {
        return ERROR;
    }
    
    return OK;
}

int
value_to_json(char* buf, int* index, yajl_gen handle)
{
    int type, size;
    long lval;
    double dval;
    
    if(ei_get_type(buf, index, &type, &size)) return ERROR;
    //fprintf(stderr, "Type: %d Size: %d\r\n", type, size);
    
    switch(type)
    {
        case VERSION:
            if(ei_decode_version(buf, index, &type)) return ERROR;
            return OK;
        case LONG:
        case NEG_LONG:
            if(ei_decode_long(buf, index, &lval)) return ERROR;
            if(yajl_gen_integer(handle, lval)) return ERROR;
            return OK;
        case DOUBLE:
            if(ei_decode_double(buf, index, &dval)) return ERROR;
            if(yajl_gen_double(handle, dval)) return ERROR;
            return OK;
        case ATOM:
            return atom_to_json(buf, index, handle);
        case TUPLE:
            return map_to_json(buf, index, handle);
        case LIST:
            return array_to_json(buf, index, handle);
        case STRING:
            return string_to_json(buf, index, handle);
        case EMPTY_LIST:
            if(ei_decode_list_header(buf, index, &size)) return ERROR;
            if(size != 0) return ERROR;
            if(yajl_gen_array_open(handle)) return ERROR;
            if(yajl_gen_array_close(handle)) return ERROR;
            return OK;
        case BINARY:
            return binary_to_json(buf, index, handle);
        default:
            return ERROR;
    }
}

int
term_to_json(char* buf, int len, char** rbuf, int rlen)
{
    int version;
    int index = 0;
    yajl_gen_config conf = {0, NULL};
    yajl_gen handle = yajl_gen_alloc(&conf);
    
    if(ei_decode_version(buf, &index, &version))
    {
        return -1;
    }
    else if(value_to_json(buf, &index, handle))
    {
        return -1;
    }
    else
    {
        *rbuf = (char*) yajl_gen_get_buf(handle);
        index = yajl_gen_get_buf(handle)->orig_size;
        yajl_gen_free(handle);
        return index;
    }
}
