/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#include <ei.h>

#include "ei_bin_buf.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define INIT_EI_BIN_BUF_LENGTH 2048

void
ei_bin_buf_alloc(ei_bin_buf* buf, unsigned int want)
{
    // Initail alloc using ErlDrvBinary
    if(buf->bin == NULL && buf->data == NULL)
    {
        buf->bin = driver_alloc_binary(want);
        buf->data = buf->bin->orig_bytes;
        buf->length = buf->bin->orig_size;
    }
    // Realloc for ErlDrvBinary
    else if(buf->bin != NULL)
    {
        buf->bin = driver_realloc_binary(buf->bin, want);
        buf->data = buf->bin->orig_bytes;
        buf->length = buf->bin->orig_size;
    }
    // Realloc for rbuf.
    else if(buf->data != NULL)
    {
        buf->data = driver_realloc(buf->data, want);
        buf->length = want;
    }
}

static void
ei_bin_buf_ensure_available(ei_bin_buf* buf, unsigned int want)
{
    unsigned int need;
    
    assert(buf != NULL);

    /* first call */
    if(buf->data == NULL)
    {
        ei_bin_buf_alloc(buf, INIT_EI_BIN_BUF_LENGTH);
        buf->data[0] = 0;
    }

    need = buf->length;

    while(want > need) need <<= 1;

    if(need != buf->length)
    {
        ei_bin_buf_alloc(buf, need);
    }
}

ei_bin_buf*
ei_bin_buf_init()
{
    ei_bin_buf* ret = driver_alloc(sizeof(ei_bin_buf));
    ret->bin = NULL;
    ret->data = NULL;
    ret->length = 0;
    ret->index = 0;
    ret->used = 0;
    return ret;
}

ei_bin_buf*
ei_bin_buf_from_buffer(char** rbuf, int rlen)
{
    ei_bin_buf* ret = ei_bin_buf_init();

    if(*rbuf != NULL)
    {
        ret->data = *rbuf;
        ret->length = rlen;
    }
    else
    {
        ret->length = INIT_EI_BIN_BUF_LENGTH;
        ret->data = driver_alloc(ret->length);
    }

    return ret;
}

void
ei_bin_buf_close(ei_bin_buf* buf, int free_binary)
{
    assert(buf != NULL);
    if(buf->bin && free_binary) driver_free_binary(buf->bin);
    driver_free(buf);
}

void
ei_bin_buf_version(ei_bin_buf* buf)
{
    ei_encode_version(NULL, &buf->index);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_version(buf->data, &buf->used);
}

void
ei_bin_buf_tuple(ei_bin_buf* buf, int elements)
{
    ei_encode_tuple_header(NULL, &buf->index, elements);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_tuple_header(buf->data, &buf->used, elements);
}

void
ei_bin_buf_list(ei_bin_buf* buf, int elements)
{
    ei_encode_list_header(NULL, &buf->index, elements);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_list_header(buf->data, &buf->used, elements);
}

void
ei_bin_buf_empty_list(ei_bin_buf* buf)
{
    ei_encode_empty_list(NULL, &buf->index);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_empty_list(buf->data, &buf->used);
}

void
ei_bin_buf_atom(ei_bin_buf* buf, const char* data, int length)
{
    ei_encode_atom_len(NULL, &buf->index, data, length);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_atom_len(buf->data, &buf->used, data, length);
}

void
ei_bin_buf_long(ei_bin_buf* buf, long value)
{
    ei_encode_long(NULL, &buf->index, value);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_long(buf->data, &buf->used, value);
}

void
ei_bin_buf_double(ei_bin_buf* buf, double value)
{
    ei_encode_double(NULL, &buf->index, value);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_double(buf->data, &buf->used, value);
}

void
ei_bin_buf_binary(ei_bin_buf* buf, const void* data, long length)
{
    ei_encode_binary(NULL, &buf->index, data, length);
    ei_bin_buf_ensure_available(buf, buf->index);
    ei_encode_binary(buf->data, &buf->used, data, length);
}

void
ei_bin_buf_append(ei_bin_buf* buf, const void* data, unsigned int length)
{
    ei_bin_buf_ensure_available(buf, buf->used + length);
    memcpy(buf->data+buf->used, data, length);
    buf->index += length;
    buf->used += length;
}

void
ei_bin_buf_clear(ei_bin_buf* buf)
{
    buf->used = 0;
    if(buf->data) buf->data[buf->used] = 0;
}
