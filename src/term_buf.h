/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#ifndef __TERM_BUF_H__
#define __TERM_BUF_H__

#include <erl_driver.h>

typedef struct
{
    double*         data;
    int             used;
    int             length;
} dbl_store;

typedef struct
{
    ErlDrvTermData* terms;
    int             length;
    int             used;
    dbl_store*      store;
} term_buf;

term_buf* term_buf_init(void);
void term_buf_destroy(term_buf* buf);

int term_buf_tuple(term_buf* buf, unsigned int elements);
int term_buf_list(term_buf* buf, unsigned int elements);
int term_buf_binary(term_buf* buf, const void* data, unsigned int length);

// These can be better optimized to store the atom value and reuse the value.
int term_buf_true(term_buf* buf);
int term_buf_false(term_buf* buf);
int term_buf_null(term_buf* buf);

int term_buf_int(term_buf* buf, int value);
int term_buf_double(term_buf* buf, double value);

#endif
