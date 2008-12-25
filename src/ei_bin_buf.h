/*
 */ 

#ifndef __EI_BIN_BUF_H__
#define __EI_BIN_BUF_H__

#include <erl_driver.h>

typedef struct
{
    ErlDrvBinary*   bin;
    char*           data;
    int             length;
    int             index;
    int             used;
} ei_bin_buf;

ei_bin_buf* ei_bin_buf_init(void);
ei_bin_buf* ei_bin_buf_from_buffer(char** rbuf, int rlen);
void ei_bin_buf_close(ei_bin_buf* buf, int free_binary);
void ei_bin_buf_version(ei_bin_buf* buf);
void ei_bin_buf_tuple(ei_bin_buf* buf, int elements);
void ei_bin_buf_list(ei_bin_buf* buf, int elements);
void ei_bin_buf_empty_list(ei_bin_buf* buf);
void ei_bin_buf_atom(ei_bin_buf* buf, const char* data, int length);
void ei_bin_buf_long(ei_bin_buf* buf, long value);
void ei_bin_buf_double(ei_bin_buf* buf, double value);
void ei_bin_buf_binary(ei_bin_buf* buf, const void* data, long length);
void ei_bin_buf_append(ei_bin_buf* buf, const void* data, unsigned int len);
void ei_bin_buf_clear(ei_bin_buf* buf);

#endif
