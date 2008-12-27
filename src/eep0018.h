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

typedef struct
{
    // Comm stuffs
    ErlDrvPort      port;
    ErlDrvTermData  pid;

    // JSON Data
    char*           json;
    int             jlen;

    // Respones
    int             resp;
    
    // SAX States
    ErlDrvTermData  value;
    ErlDrvTermData  map_open;
    ErlDrvTermData  map_close;
    ErlDrvTermData  map_key;
    ErlDrvTermData  list_open;
    ErlDrvTermData  list_close;
    ErlDrvTermData  finished;
    ErlDrvTermData  error;
    
    // Stored atoms
    ErlDrvTermData  atom_null;
    ErlDrvTermData  atom_true;
    ErlDrvTermData  atom_false;
} State;

int term_to_json(char* buf, int len, char** rbuf, int rlen);
int json_to_term(ErlDrvPort port, char* buf, int len, char** rbuf, int rlen);

State* init_state(ErlDrvPort, char* json, int jlen);
void destroy_state(void* ctx);
int output_error(ErlDrvPort port, ErlDrvTermData pid, char* mesg);

#endif
