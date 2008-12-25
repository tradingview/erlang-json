#ifndef __EEP0018_H__
#define __EEP0018_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>

#include "yajl_gen.h"
#include "yajl_parse.h"

#define ERROR 1
#define OK 0

#define MAX_DEPTH   1024

void json_to_term(void* ctx);
void term_to_json(void* ctx);

typedef struct
{
    int                 direction;  // 1: JSON -> ERLANG, 2: ERLANG -> JSON
    int                 result;

    ErlDrvBinary*       bin;
    
    //ERLANG -> JSON
    yajl_gen            ghandle;

    //JSON -> ERLANG
    ei_x_buff           buf;
    yajl_handle         phandle;
    int                 state[MAX_DEPTH];
    int                 depth;
} eep0018_data;

#endif
